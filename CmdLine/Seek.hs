{- git-annex command seeking
 - 
 - These functions find appropriate files or other things based on
 - the values a user passes to a command, and prepare actions operating
 - on them.
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module CmdLine.Seek where

import Annex.Common
import Types.Command
import Types.FileMatcher
import qualified Annex
import qualified Git
import qualified Git.Command
import qualified Git.LsFiles as LsFiles
import qualified Git.LsTree as LsTree
import qualified Git.Types as Git
import Git.FilePath
import qualified Limit
import CmdLine.GitAnnex.Options
import Logs
import Logs.Unused
import Types.Transfer
import Logs.Transfer
import Remote.List
import qualified Remote
import Annex.CatFile
import Git.CatFile
import Annex.CurrentBranch
import Annex.Content
import Annex.Link
import Annex.InodeSentinal
import Annex.Concurrent
import qualified Annex.Branch
import qualified Annex.BranchState
import qualified Database.Keys
import qualified Utility.RawFilePath as R
import Utility.Tuple

import Control.Concurrent.Async
import System.Posix.Types

withFilesInGit :: WarnUnmatchWhen -> (RawFilePath -> CommandSeek) -> [WorkTreeItem] -> CommandSeek
withFilesInGit ww a l = seekFiltered a $
	seekHelper id ww LsFiles.inRepo l

withFilesInGitAnnex :: WarnUnmatchWhen -> (RawFilePath -> Key -> CommandSeek) -> [WorkTreeItem] -> CommandSeek
withFilesInGitAnnex ww a l = seekFilteredKeys a $
	seekHelper fst3 ww LsFiles.inRepoDetails l

withFilesInGitNonRecursive :: WarnUnmatchWhen -> String -> (RawFilePath -> CommandSeek) -> [WorkTreeItem] -> CommandSeek
withFilesInGitNonRecursive ww needforce a l = ifM (Annex.getState Annex.force)
	( withFilesInGit ww a l
	, if null l
		then giveup needforce
		else seekFiltered a (getfiles [] l)
	)
  where
	getfiles c [] = return (reverse c)
	getfiles c ((WorkTreeItem p):ps) = do
		os <- seekOptions ww
		(fs, cleanup) <- inRepo $ LsFiles.inRepo os [toRawFilePath p]
		case fs of
			[f] -> do
				void $ liftIO $ cleanup
				getfiles (f:c) ps
			[] -> do
				void $ liftIO $ cleanup
				getfiles c ps
			_ -> giveup needforce

withFilesNotInGit :: (RawFilePath -> CommandSeek) -> [WorkTreeItem] -> CommandSeek
withFilesNotInGit  a l = go =<< seek
  where
	seek = do
		force <- Annex.getState Annex.force
		g <- gitRepo
		liftIO $ Git.Command.leaveZombie
			<$> LsFiles.notInRepo [] force (map (\(WorkTreeItem f) -> toRawFilePath f) l) g
	go fs = seekFiltered a $
		return $ concat $ segmentPaths id (map (\(WorkTreeItem f) -> toRawFilePath f) l) fs

withPathContents :: ((FilePath, FilePath) -> CommandSeek) -> CmdParams -> CommandSeek
withPathContents a params = do
	matcher <- Limit.getMatcher
	forM_ params $ \p -> do
		fs <- liftIO $ get p
		forM fs $ \f ->
			whenM (checkmatch matcher f) $
				a f
  where
	get p = ifM (isDirectory <$> getFileStatus p)
		( map (\f -> (f, makeRelative (parentDir p) f))
			<$> dirContentsRecursiveSkipping (".git" `isSuffixOf`) True p
		, return [(p, takeFileName p)]
		)
	checkmatch matcher (f, relf) = matcher $ MatchingFile $ FileInfo
		{ currFile = toRawFilePath f
		, matchFile = toRawFilePath relf
		}

withWords :: ([String] -> CommandSeek) -> CmdParams -> CommandSeek
withWords a params = a params

withStrings :: (String -> CommandSeek) -> CmdParams -> CommandSeek
withStrings a params = sequence_ $ map a params

withPairs :: ((String, String) -> CommandSeek) -> CmdParams -> CommandSeek
withPairs a params = sequence_ $ map a $ pairs [] params
  where
	pairs c [] = reverse c
	pairs c (x:y:xs) = pairs ((x,y):c) xs
	pairs _ _ = giveup "expected pairs"

withFilesToBeCommitted :: (RawFilePath -> CommandSeek) -> [WorkTreeItem] -> CommandSeek
withFilesToBeCommitted a l = seekFiltered a $
	seekHelper id WarnUnmatchWorkTreeItems (const LsFiles.stagedNotDeleted) l

{- unlocked pointer files that are staged, and whose content has not been
 - modified-}
withUnmodifiedUnlockedPointers :: WarnUnmatchWhen -> (RawFilePath -> CommandSeek) -> [WorkTreeItem] -> CommandSeek
withUnmodifiedUnlockedPointers ww a l = seekFiltered a unlockedfiles
  where
	unlockedfiles = filterM isUnmodifiedUnlocked 
		=<< seekHelper id ww (const LsFiles.typeChangedStaged) l

isUnmodifiedUnlocked :: RawFilePath -> Annex Bool
isUnmodifiedUnlocked f = catKeyFile f >>= \case
	Nothing -> return False
	Just k -> sameInodeCache f =<< Database.Keys.getInodeCaches k

{- Finds files that may be modified. -}
withFilesMaybeModified :: WarnUnmatchWhen -> (RawFilePath -> CommandSeek) -> [WorkTreeItem] -> CommandSeek
withFilesMaybeModified ww a params = seekFiltered a $
	seekHelper id ww LsFiles.modified params

withKeys :: (Key -> CommandSeek) -> CmdParams -> CommandSeek
withKeys a l = sequence_ $ map (a . parse) l
  where
	parse p = fromMaybe (giveup "bad key") $ deserializeKey p

withNothing :: CommandSeek -> CmdParams -> CommandSeek
withNothing a [] = a
withNothing _ _ = giveup "This command takes no parameters."

{- Handles the --all, --branch, --unused, --failed, --key, and
 - --incomplete options, which specify particular keys to run an
 - action on.
 -
 - In a bare repo, --all is the default.
 -
 - Otherwise falls back to a regular CommandSeek action on
 - whatever params were passed.
 -}
withKeyOptions 
	:: Maybe KeyOptions
	-> Bool
	-> ((Key, ActionItem) -> CommandSeek)
	-> ([WorkTreeItem] -> CommandSeek)
	-> [WorkTreeItem]
	-> CommandSeek
withKeyOptions ko auto keyaction = withKeyOptions' ko auto mkkeyaction
  where
	mkkeyaction = do
		matcher <- Limit.getMatcher
		return $ \v@(k, ai) ->
			let i = case ai of
				ActionItemBranchFilePath (BranchFilePath _ topf) _ ->
					MatchingKey k (AssociatedFile $ Just $ getTopFilePath topf)
				_ -> MatchingKey k (AssociatedFile Nothing)
			in whenM (matcher i) $
				keyaction v

withKeyOptions' 
	:: Maybe KeyOptions
	-> Bool
	-> Annex ((Key, ActionItem) -> Annex ())
	-> ([WorkTreeItem] -> CommandSeek)
	-> [WorkTreeItem]
	-> CommandSeek
withKeyOptions' ko auto mkkeyaction fallbackaction params = do
	bare <- fromRepo Git.repoIsLocalBare
	when (auto && bare) $
		giveup "Cannot use --auto in a bare repository"
	case (null params, ko) of
		(True, Nothing)
			| bare -> noauto runallkeys
			| otherwise -> fallbackaction params
		(False, Nothing) -> fallbackaction params
		(True, Just WantAllKeys) -> noauto runallkeys
		(True, Just WantUnusedKeys) -> noauto $ runkeyaction unusedKeys'
		(True, Just WantFailedTransfers) -> noauto runfailedtransfers
		(True, Just (WantSpecificKey k)) -> noauto $ runkeyaction (return [k])
		(True, Just WantIncompleteKeys) -> noauto $ runkeyaction incompletekeys
		(True, Just (WantBranchKeys bs)) -> noauto $ runbranchkeys bs
		(False, Just _) -> giveup "Can only specify one of file names, --all, --branch, --unused, --failed, --key, or --incomplete"
  where
	noauto a
		| auto = giveup "Cannot use --auto with --all or --branch or --unused or --key or --incomplete"
		| otherwise = a
	
	incompletekeys = staleKeysPrune gitAnnexTmpObjectDir True

	-- List all location log files on the git-annex branch,
	-- and use those to get keys. Pass through cat-file
	-- to get the contents of the location logs, and pre-cache
	-- those. This significantly speeds up typical operations
	-- that need to look at the location log for each key.
	runallkeys = do
		keyaction <- mkkeyaction
		config <- Annex.getGitConfig
		g <- Annex.gitRepo
		
		void Annex.Branch.update
		(l, cleanup) <- inRepo $ LsTree.lsTree
			LsTree.LsTreeRecursive
			Annex.Branch.fullname
		let getk = locationLogFileKey config . getTopFilePath
		let go reader = liftIO reader >>= \case
			Nothing -> return ()
			Just (f, content) -> do
				case getk f of
					Just k -> do
						maybe noop (Annex.BranchState.setCache (getTopFilePath f)) content
						keyaction (k, mkActionItem k)
					Nothing -> return ()
				go reader
		catObjectStreamLsTree l (isJust . getk . LsTree.file) g go
		liftIO $ void cleanup

	runkeyaction getks = do
		keyaction <- mkkeyaction
		ks <- getks
		forM_ ks $ \k -> keyaction (k, mkActionItem k)
	
	runbranchkeys bs = do
		keyaction <- mkkeyaction
		forM_ bs $ \b -> do
			(l, cleanup) <- inRepo $ LsTree.lsTree LsTree.LsTreeRecursive b
			forM_ l $ \i -> catKey (LsTree.sha i) >>= \case
				Nothing -> noop
				Just k -> 
					let bfp = mkActionItem (BranchFilePath b (LsTree.file i), k)
					in keyaction (k, bfp)
			unlessM (liftIO cleanup) $
				error ("git ls-tree " ++ Git.fromRef b ++ " failed")
	
	runfailedtransfers = do
		keyaction <- mkkeyaction
		rs <- remoteList
		ts <- concat <$> mapM (getFailedTransfers . Remote.uuid) rs
		forM_ ts $ \(t, i) ->
			keyaction (transferKey t, mkActionItem (t, i))

seekFiltered :: (RawFilePath -> CommandSeek) -> Annex [RawFilePath] -> Annex ()
seekFiltered a fs = do
	matcher <- Limit.getMatcher
	sequence_ =<< (map (process matcher) <$> fs)
  where
	process matcher f =
		whenM (matcher $ MatchingFile $ FileInfo f f) $ a f

-- This is significantly faster than using lookupKey after seekFiltered.
seekFilteredKeys :: (RawFilePath -> Key -> CommandSeek) -> Annex [(RawFilePath, Git.Sha, FileMode)] -> Annex ()
seekFilteredKeys a fs = do
	g <- Annex.gitRepo
	matcher <- Limit.getMatcher
	catObjectMetaDataStream g $ \mdfeeder mdcloser mdreader ->
		catObjectStream g $ \feeder closer reader -> do
			processertid <- liftIO . async =<< forkState
				(gofeed matcher feeder closer mdfeeder mdcloser)
			mdprocessertid <- liftIO . async =<< forkState
				(mdprocess matcher mdreader feeder)
			goread reader
			join (liftIO (wait mdprocessertid))
			join (liftIO (wait processertid))
  where
	gofeed matcher feeder closer mdfeeder mdcloser = do
		l <- fs
		forM_ l $ process matcher feeder mdfeeder
		liftIO $ void closer
		liftIO $ void mdcloser

	goread reader = liftIO reader >>= \case
		Just (f, content) -> do
			maybe noop (a f) (parseLinkTargetOrPointerLazy =<< content)
			goread reader
		Nothing -> return ()
	
	feedmatches matcher feeder f sha = 
		whenM (matcher $ MatchingFile $ FileInfo f f) $
			liftIO $ feeder (f, sha)

	process matcher feeder mdfeeder (f, sha, mode) = case
		Git.toTreeItemType mode of
			Just Git.TreeSymlink -> 
				feedmatches matcher feeder f sha
			Just Git.TreeSubmodule -> return ()
			-- Might be a pointer file, might be other
			-- file in git, possibly large. Avoid catting
			-- large files by first looking up the size.
			Just _ -> liftIO $ mdfeeder (f, sha)
			Nothing -> return ()

	mdprocess matcher mdreader feeder = liftIO mdreader >>= \case
		Just (f, Just (sha, size, _type))
			| size < maxPointerSz -> do
				feedmatches matcher feeder f sha
				mdprocess matcher mdreader feeder
		Just _ -> mdprocess matcher mdreader feeder
		Nothing -> return ()

seekHelper :: (a -> RawFilePath) -> WarnUnmatchWhen -> ([LsFiles.Options] -> [RawFilePath] -> Git.Repo -> IO ([a], IO Bool)) -> [WorkTreeItem] -> Annex [a]
seekHelper c ww a l = do
	os <- seekOptions ww
	inRepo $ \g ->
		concat . concat <$> forM (segmentXargsOrdered l')
			(runSegmentPaths c (\fs -> Git.Command.leaveZombie <$> a os fs g) . map toRawFilePath)
  where
	l' = map (\(WorkTreeItem f) -> f) l

data WarnUnmatchWhen = WarnUnmatchLsFiles | WarnUnmatchWorkTreeItems

seekOptions :: WarnUnmatchWhen -> Annex [LsFiles.Options]
seekOptions WarnUnmatchLsFiles =
	ifM (annexSkipUnknown <$> Annex.getGitConfig)
		( return [] 
		, return [LsFiles.ErrorUnmatch]
		)
seekOptions WarnUnmatchWorkTreeItems = return []

-- An item in the work tree, which may be a file or a directory.
newtype WorkTreeItem = WorkTreeItem FilePath

-- When in an adjusted branch that hides some files, it may not exist
-- in the current work tree, but in the original branch. This allows
-- seeking for such files.
newtype AllowHidden = AllowHidden Bool

-- git ls-files without --error-unmatch seeks work tree items matching
-- some criteria, and silently skips over anything that does not exist.

-- Also, when two directories are symlinked, referring to a file
-- inside the symlinked directory will be silently skipped by
-- git ls-files without --error-unmatch. 
--
-- Sometimes a command needs to use git-lsfiles that way, perhaps repeatedly.
-- But users expect an error message when one of the files they provided
-- as a command-line parameter doesn't exist, so this checks that each
-- exists when run with WarnUnmatchWorkTreeItems.
--
-- Note that, unlike --error-unmatch, using this does not warn
-- about command-line parameters that exist, but are not checked into git.
workTreeItems :: WarnUnmatchWhen -> CmdParams -> Annex [WorkTreeItem]
workTreeItems = workTreeItems' (AllowHidden False)

workTreeItems' :: AllowHidden -> WarnUnmatchWhen -> CmdParams -> Annex [WorkTreeItem]
workTreeItems' (AllowHidden allowhidden) ww ps = do
	case ww of
		WarnUnmatchWorkTreeItems -> runcheck
		WarnUnmatchLsFiles -> 
			whenM (annexSkipUnknown <$> Annex.getGitConfig)
				runcheck
	return (map WorkTreeItem ps)
  where
	runcheck = do
		currbranch <- getCurrentBranch
		forM_ ps $ \p -> do
			relf <- liftIO $ relPathCwdToFile p
			ifM (not <$> (exists p <||> hidden currbranch relf))
				( prob (p ++ " not found")
				, whenM (viasymlink (upFrom relf)) $
					prob (p ++ " is beyond a symbolic link")
				)
	
	exists p = isJust <$> liftIO (catchMaybeIO $ getSymbolicLinkStatus p)

	viasymlink Nothing = return False
	viasymlink (Just p) =
		ifM (liftIO $ isSymbolicLink <$> getSymbolicLinkStatus p)
			( return True
			, viasymlink (upFrom p)
			)

	hidden currbranch f
		| allowhidden = isJust
			<$> catObjectMetaDataHidden (toRawFilePath f) currbranch
		| otherwise = return False

	prob msg = do
		toplevelWarning False msg
		Annex.incError
	
notSymlink :: RawFilePath -> IO Bool
notSymlink f = liftIO $ not . isSymbolicLink <$> R.getSymbolicLinkStatus f
