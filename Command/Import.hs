{- git-annex command
 -
 - Copyright 2012-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Import where

import Command
import qualified Git
import qualified Annex
import qualified Command.Add
import qualified Command.Reinject
import Utility.CopyFile
import Backend
import Types.KeySource
import Annex.CheckIgnore
import Annex.NumCopies
import Annex.FileMatcher
import Annex.Ingest
import Annex.InodeSentinal
import Utility.InodeCache
import Logs.Location

cmd :: Command
cmd = withGlobalOptions (jobsOption : jsonOption : fileMatchingOptions) $ notBareRepo $
	command "import" SectionCommon 
		"move and add files from outside git working copy"
		paramPaths (seek <$$> optParser)

data DuplicateMode = Default | Duplicate | DeDuplicate | CleanDuplicates | SkipDuplicates | ReinjectDuplicates
	deriving (Eq)

data ImportOptions = ImportOptions
	{ importFiles :: CmdParams
	, duplicateMode :: DuplicateMode
	}

optParser :: CmdParamsDesc -> Parser ImportOptions
optParser desc = ImportOptions
	<$> cmdParams desc
	<*> (fromMaybe Default <$> optional duplicateModeParser)

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
seek o = allowConcurrentOutput $ do
	repopath <- liftIO . absPath =<< fromRepo Git.repoPath
	inrepops <- liftIO $ filter (dirContains repopath) <$> mapM absPath (importFiles o)
	unless (null inrepops) $ do
		giveup $ "cannot import files from inside the working tree (use git annex add instead): " ++ unwords inrepops
	largematcher <- largeFilesMatcher
	withPathContents (start largematcher (duplicateMode o)) (importFiles o)

start :: GetFileMatcher -> DuplicateMode -> (FilePath, FilePath) -> CommandStart
start largematcher mode (srcfile, destfile) =
	ifM (liftIO $ isRegularFile <$> getSymbolicLinkStatus srcfile)
		( do
			showStart "import" destfile
			next pickaction
		, stop
		)
  where
	deletedup k = do
		showNote $ "duplicate of " ++ key2file k
		verifyExisting k destfile
			( do
				liftIO $ removeFile srcfile
				next $ return True
			, do
				warning "Could not verify that the content is still present in the annex; not removing from the import location."
				stop
			)
	reinject k = do
		showNote "reinjecting"
		Command.Reinject.perform srcfile k
	importfile ld k = checkdestdir $ do
		ignored <- not <$> Annex.getState Annex.force <&&> checkIgnored destfile
		if ignored
			then do
				warning $ "not importing " ++ destfile ++ " which is .gitignored (use --force to override)"
				stop
			else do
				existing <- liftIO (catchMaybeIO $ getSymbolicLinkStatus destfile)
				case existing of
					Nothing -> importfilechecked ld k
					Just s
						| isDirectory s -> notoverwriting "(is a directory)"
						| isSymbolicLink s -> notoverwriting "(is a symlink)"
						| otherwise -> ifM (Annex.getState Annex.force)
							( do
								liftIO $ nukeFile destfile
								importfilechecked ld k
							, notoverwriting "(use --force to override, or a duplication option such as --deduplicate to clean up)"
							)
	checkdestdir cont = do
		let destdir = parentDir destfile
		existing <- liftIO (catchMaybeIO $ getSymbolicLinkStatus destdir)
		case existing of
			Nothing -> cont
			Just s
				| isDirectory s -> cont
				| otherwise -> do
					warning $ "not importing " ++ destfile ++ " because " ++ destdir ++ " is not a directory"
					stop

	importfilechecked ld k = do
		-- Move or copy the src file to the dest file.
		-- The dest file is what will be ingested.
		liftIO $ createDirectoryIfMissing True (parentDir destfile)
		liftIO $ if mode == Duplicate || mode == SkipDuplicates
			then void $ copyFileExternal CopyAllMetaData srcfile destfile
			else moveFile srcfile destfile
		-- Get the inode cache of the dest file. It should be
		-- weakly the same as the origianlly locked down file's
		-- inode cache. (Since the file may have been copied,
		-- its inodes may not be the same.)
		newcache <- withTSDelta $ liftIO . genInodeCache destfile
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
			( ingestAdd' (Just ld') (Just k)
				>>= maybe
					stop
					(\addedk -> next $ Command.Add.cleanup addedk True)
			, next $ Command.Add.addSmall destfile 
			)
	notoverwriting why = do
		warning $ "not overwriting existing " ++ destfile ++ " " ++ why
		stop
	lockdown a = do
		lockingfile <- not <$> addUnlocked
		-- Minimal lock down with no hard linking so nothing
		-- has to be done to clean up from it.
		let cfg = LockDownConfig
			{ lockingFile = lockingfile
			, hardlinkFileTmp = False
			}
		v <- lockDown cfg srcfile
		case v of
			Just ld -> do
				backend <- chooseBackend destfile
				v' <- genKey (keySource ld) backend
				case v' of
					Just (k, _) -> a (ld, k)
					Nothing -> giveup "failed to generate a key"
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

verifyExisting :: Key -> FilePath -> (CommandPerform, CommandPerform) -> CommandPerform
verifyExisting key destfile (yes, no) = do
	-- Look up the numcopies setting for the file that it would be
	-- imported to, if it were imported.
	need <- getFileNumCopies destfile

	(tocheck, preverified) <- verifiableCopies key []
	verifyEnoughCopiesToDrop [] key Nothing need [] preverified tocheck
		(const yes) no
