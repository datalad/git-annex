{- git-annex command
 -
 - Copyright 2012-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE ApplicativeDo #-}

module Command.Import where

import Command
import qualified Git
import qualified Annex
import qualified Command.Add
import qualified Command.Reinject
import qualified Types.Remote as Remote
import qualified Git.Ref
import Utility.CopyFile
import Utility.OptParse
import Backend
import Types.KeySource
import Annex.CheckIgnore
import Annex.NumCopies
import Annex.FileMatcher
import Annex.Ingest
import Annex.InodeSentinal
import Annex.Import
import Annex.Perms
import Annex.RemoteTrackingBranch
import Utility.InodeCache
import Logs.Location
import Git.FilePath
import Git.Types
import Types.Import
import Utility.Metered
import qualified Utility.RawFilePath as R

import Control.Concurrent.STM
import System.PosixCompat.Files (isDirectory, isSymbolicLink, isRegularFile)

cmd :: Command
cmd = notBareRepo $
	withAnnexOptions opts $
		command "import" SectionCommon 
			"add a tree of files to the repository"
			(paramPaths ++ "|BRANCH")
			(seek <$$> optParser)
  where
	opts =
		[ backendOption
		, jobsOption
		, jsonOptions
		, jsonProgressOption
		-- These options are only used when importing from a
		-- directory, not from a special remote. So it's ok
		-- to use LimitDiskFiles.
		, fileMatchingOptions LimitDiskFiles
		]

data ImportOptions 
	= LocalImportOptions
		{ importFiles :: CmdParams
		, duplicateMode :: DuplicateMode
		, checkGitIgnoreOption :: CheckGitIgnore
		}
	| RemoteImportOptions
		{ importFromRemote :: DeferredParse Remote
		, importToBranch :: Branch
		, importToSubDir :: Maybe FilePath
		, importContent :: Bool
		, checkGitIgnoreOption :: CheckGitIgnore
		}

optParser :: CmdParamsDesc -> Parser ImportOptions
optParser desc = do
	ps <- cmdParams desc
	mfromremote <- optional $ mkParseRemoteOption <$> parseFromOption
	content <- invertableSwitch "content" True
		( help "do not get contents of imported files"
		)
	dupmode <- fromMaybe Default <$> optional duplicateModeParser
	ic <- Command.Add.checkGitIgnoreSwitch
	return $ case mfromremote of
		Nothing -> LocalImportOptions ps dupmode ic
		Just r -> case ps of
			[bs] -> 
				let (branch, subdir) = separate (== ':') bs
				in RemoteImportOptions r
					(Ref (encodeBS branch))
					(if null subdir then Nothing else Just subdir)
					content
					ic
			_ -> giveup "expected BRANCH[:SUBDIR]"

data DuplicateMode = Default | Duplicate | DeDuplicate | CleanDuplicates | SkipDuplicates | ReinjectDuplicates
	deriving (Eq)

duplicateModeParser :: Parser DuplicateMode
duplicateModeParser = 
	flag' Duplicate
		( long "duplicate" 
		<> help "do not delete source files"
		)
	<|> flag' DeDuplicate
		( long "deduplicate"
		<> help "delete source files whose content was imported before"
		)
	<|> flag' CleanDuplicates
		( long "clean-duplicates"
		<> help "delete duplicate source files (import nothing)"
		)
	<|> flag' SkipDuplicates
		( long "skip-duplicates"
		<> help "import only new files (do not delete source files)"
		)
	<|> flag' ReinjectDuplicates
		( long "reinject-duplicates"
		<> help "import new files, and reinject the content of files that were imported before"
		)

seek :: ImportOptions -> CommandSeek
seek o@(LocalImportOptions {}) = startConcurrency commandStages $ do
	repopath <- liftIO . absPath =<< fromRepo Git.repoPath
	inrepops <- liftIO $ filter (dirContains repopath)
		<$> mapM (absPath . toRawFilePath) (importFiles o)
	unless (null inrepops) $ do
		giveup $ "cannot import files from inside the working tree (use git annex add instead): " ++ unwords (map fromRawFilePath inrepops)
	largematcher <- largeFilesMatcher
	addunlockedmatcher <- addUnlockedMatcher
	(commandAction . startLocal o addunlockedmatcher largematcher (duplicateMode o))
		`withPathContents` importFiles o
seek o@(RemoteImportOptions {}) = startConcurrency commandStages $ do
	r <- getParsed (importFromRemote o)
	unlessM (Remote.isImportSupported r) $
		giveup "That remote does not support imports."
	subdir <- maybe
		(pure Nothing)
		(Just <$$> inRepo . toTopFilePath . toRawFilePath)
		(importToSubDir o)
	seekRemote r (importToBranch o) subdir (importContent o) (checkGitIgnoreOption o)

startLocal :: ImportOptions -> AddUnlockedMatcher -> GetFileMatcher -> DuplicateMode -> (RawFilePath, RawFilePath) -> CommandStart
startLocal o addunlockedmatcher largematcher mode (srcfile, destfile) =
	ifM (liftIO $ isRegularFile <$> R.getSymbolicLinkStatus srcfile)
		( starting "import" ai si pickaction
		, stop
		)
  where
 	ai = ActionItemTreeFile destfile
	si = SeekInput []

	deletedup k = do
		showNote $ "duplicate of " ++ serializeKey k
		verifyExisting k destfile
			( do
				liftIO $ R.removeLink srcfile
				next $ return True
			, do
				warning "Could not verify that the content is still present in the annex; not removing from the import location."
				stop
			)
	reinject k = do
		showNote "reinjecting"
		Command.Reinject.perform srcfile k
	importfile ld k = checkdestdir $ do
		ignored <- checkIgnored (checkGitIgnoreOption o) destfile
		if ignored
			then do
				warning $ "not importing " ++ fromRawFilePath destfile ++ " which is .gitignored (use --no-check-gitignore to override)"
				stop
			else do
				existing <- liftIO (catchMaybeIO $ R.getSymbolicLinkStatus destfile)
				case existing of
					Nothing -> importfilechecked ld k
					Just s
						| isDirectory s -> notoverwriting "(is a directory)"
						| isSymbolicLink s -> ifM (Annex.getRead Annex.force)
							( do
								liftIO $ removeWhenExistsWith R.removeLink destfile
								importfilechecked ld k
							, notoverwriting "(is a symlink)"
							)
						| otherwise -> ifM (Annex.getRead Annex.force)
							( do
								liftIO $ removeWhenExistsWith R.removeLink destfile
								importfilechecked ld k
							, notoverwriting "(use --force to override, or a duplication option such as --deduplicate to clean up)"
							)
	checkdestdir cont = do
		let destdir = parentDir destfile
		existing <- liftIO (catchMaybeIO $ R.getSymbolicLinkStatus destdir)
		case existing of
			Nothing -> cont
			Just s
				| isDirectory s -> cont
				| otherwise -> do
					warning $ "not importing " ++ fromRawFilePath destfile ++ " because " ++ fromRawFilePath destdir ++ " is not a directory"
					stop

	importfilechecked ld k = do
		-- Move or copy the src file to the dest file.
		-- The dest file is what will be ingested.
		createWorkTreeDirectory (parentDir destfile)
		unwind <- liftIO $ if mode == Duplicate || mode == SkipDuplicates
			then do
				void $ copyFileExternal CopyAllMetaData 
					(fromRawFilePath srcfile)
					(fromRawFilePath destfile)
				return $ removeWhenExistsWith R.removeLink destfile
			else do
				moveFile srcfile destfile
				return $ moveFile destfile srcfile
		-- Make sure that the dest file has its write permissions
		-- removed; the src file normally already did, but may
		-- have imported it from a filesystem that does not allow
		-- removing write permissions, to a repo on a filesystem
		-- that does.
		when (lockingFile (lockDownConfig ld)) $ do
			freezeContent destfile
			checkLockedDownWritePerms destfile srcfile >>= \case
				Just err -> do
					liftIO unwind
					giveup err
				Nothing -> noop
		-- Get the inode cache of the dest file. It should be
		-- weakly the same as the originally locked down file's
		-- inode cache. (Since the file may have been copied,
		-- its inodes may not be the same.)
		s <- liftIO $ R.getSymbolicLinkStatus destfile
		newcache <- withTSDelta $ \d -> liftIO $ toInodeCache d destfile s
		let unchanged = case (newcache, inodeCache (keySource ld)) of
			(_, Nothing) -> True
			(Just newc, Just c) | compareWeak c newc -> True
			_ -> False
		unless unchanged $
			giveup "changed while it was being added"
		-- The LockedDown needs to be adjusted, since the destfile
		-- is what will be ingested.
		let ld' = ld
			{ keySource = KeySource
				{ keyFilename = destfile
				, contentLocation = destfile
				, inodeCache = newcache
				}
			}
		ifM (checkFileMatcher largematcher destfile)
			( ingestAdd' nullMeterUpdate (Just ld') (Just k)
				>>= maybe
					stop
					(\addedk -> next $ Command.Add.cleanup addedk True)
			, Command.Add.addSmall (DryRun False) destfile s
			)
	notoverwriting why = do
		warning $ "not overwriting existing " ++ fromRawFilePath destfile ++ " " ++ why
		stop
	lockdown a = do
		let mi = MatchingFile $ FileInfo
			{ contentFile = srcfile
			, matchFile = destfile
			, matchKey = Nothing
			}
		lockingfile <- not <$> addUnlocked addunlockedmatcher mi True
		-- Minimal lock down with no hard linking so nothing
		-- has to be done to clean up from it.
		let cfg = LockDownConfig
			{ lockingFile = lockingfile
			, hardlinkFileTmpDir = Nothing
			-- The write perms of the file may not be able to be
			-- removed, if it's being imported from a crippled
			-- filesystem. So lockDown is asked to not check
			-- the write perms. They will be checked later, after
			-- the file gets copied into the repository.
			, checkWritePerms = False
			}
		v <- lockDown cfg (fromRawFilePath srcfile)
		case v of
			Just ld -> do
				backend <- chooseBackend destfile
				k <- fst <$> genKey (keySource ld) nullMeterUpdate backend
				a (ld, k)
			Nothing -> stop
	checkdup k dupa notdupa = ifM (isKnownKey k)
		( dupa
		, notdupa
		)
	pickaction = lockdown $ \(ld, k) -> case mode of
		DeDuplicate -> checkdup k (deletedup k) (importfile ld k)
		CleanDuplicates -> checkdup k
			(deletedup k)
			(skipbecause "not duplicate")
		SkipDuplicates -> checkdup k 
			(skipbecause "duplicate")
			(importfile ld k)
		ReinjectDuplicates -> checkdup k
			(reinject k)
			(importfile ld k)
		_ -> importfile ld k
	skipbecause s = showNote (s ++ "; skipping") >> next (return True)

verifyExisting :: Key -> RawFilePath -> (CommandPerform, CommandPerform) -> CommandPerform
verifyExisting key destfile (yes, no) = do
	-- Look up the numcopies setting for the file that it would be
	-- imported to, if it were imported.
	(needcopies, mincopies) <- getFileNumMinCopies destfile

	(tocheck, preverified) <- verifiableCopies key []
	verifyEnoughCopiesToDrop [] key Nothing needcopies mincopies [] preverified tocheck
		(const yes) no

seekRemote :: Remote -> Branch -> Maybe TopFilePath -> Bool -> CheckGitIgnore -> CommandSeek
seekRemote remote branch msubdir importcontent ci = do
	importtreeconfig <- case msubdir of
		Nothing -> return ImportTree
		Just subdir ->
			let mk tree = pure $ ImportSubTree subdir tree
			in fromtrackingbranch Git.Ref.tree >>= \case
				Just tree -> mk tree
				Nothing -> inRepo (Git.Ref.tree branch) >>= \case
					Just tree -> mk tree
					Nothing -> giveup $ "Unable to find base tree for branch " ++ fromRef branch
	
	trackingcommit <- fromtrackingbranch Git.Ref.sha
	cmode <- annexCommitMode <$> Annex.getGitConfig
	let importcommitconfig = ImportCommitConfig trackingcommit cmode importmessage
	let commitimport = commitRemote remote branch tb trackingcommit importtreeconfig importcommitconfig

	importabletvar <- liftIO $ newTVarIO Nothing
	void $ includeCommandAction (listContents remote importtreeconfig ci importabletvar)
	liftIO (atomically (readTVar importabletvar)) >>= \case
		Nothing -> return ()
		Just importable -> importKeys remote importtreeconfig importcontent False importable >>= \case
			Nothing -> warning $ concat
				[ "Failed to import some files from "
				, Remote.name remote
				, ". Re-run command to resume import."
				]
			Just imported -> void $
				includeCommandAction $ 
					commitimport imported
  where
	importmessage = "import from " ++ Remote.name remote

	tb = mkRemoteTrackingBranch remote branch

	fromtrackingbranch a = inRepo $ a (fromRemoteTrackingBranch tb)

listContents :: Remote -> ImportTreeConfig -> CheckGitIgnore -> TVar (Maybe (ImportableContentsChunkable Annex (ContentIdentifier, Remote.ByteSize))) -> CommandStart
listContents remote importtreeconfig ci tvar = starting "list" ai si $
	listContents' remote importtreeconfig ci $ \importable -> do
		liftIO $ atomically $ writeTVar tvar importable
		next $ return True
  where
	ai = ActionItemOther (Just (Remote.name remote))
	si = SeekInput []

listContents' :: Remote -> ImportTreeConfig -> CheckGitIgnore -> (Maybe (ImportableContentsChunkable Annex (ContentIdentifier, Remote.ByteSize)) -> Annex a) -> Annex a
listContents' remote importtreeconfig ci a = 
	makeImportMatcher remote >>= \case
		Right matcher -> tryNonAsync (getImportableContents remote importtreeconfig ci matcher) >>= \case
			Right importable -> a importable
			Left e -> giveup $ "Unable to list contents of " ++ Remote.name remote ++ ": " ++ show e
		Left err -> giveup $ unwords 
			[ "Cannot import from"
			, Remote.name remote
			, "because of a problem with its configuration:"
			, err
			]

commitRemote :: Remote -> Branch -> RemoteTrackingBranch -> Maybe Sha -> ImportTreeConfig -> ImportCommitConfig -> ImportableContentsChunkable Annex (Either Sha Key) -> CommandStart
commitRemote remote branch tb trackingcommit importtreeconfig importcommitconfig importable =
	starting "update" ai si $ do
		importcommit <- buildImportCommit remote importtreeconfig importcommitconfig importable
		next $ updateremotetrackingbranch importcommit
  where
	ai = ActionItemOther (Just $ fromRef $ fromRemoteTrackingBranch tb)
	si = SeekInput []
	-- Update the tracking branch. Done even when there
	-- is nothing new to import, to make sure it exists.
	updateremotetrackingbranch importcommit =
		case importcommit <|> trackingcommit of
			Just c -> do
				setRemoteTrackingBranch tb c
				return True
			Nothing -> do
				warning $ "Nothing to import and " ++ fromRef branch ++ " does not exist."
				return False
