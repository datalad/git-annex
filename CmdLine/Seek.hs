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

{-# LANGUAGE TupleSections #-}

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
import qualified Git.Ref
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
import CmdLine.Action

import Control.Concurrent.Async
import System.Posix.Types

data AnnexedFileSeeker = AnnexedFileSeeker
	{ startAction :: RawFilePath -> Key -> CommandStart
	, checkContentPresent :: Maybe Bool
	, usesLocationLog :: Bool
	}

withFilesInGitAnnex :: WarnUnmatchWhen -> AnnexedFileSeeker -> [WorkTreeItem] -> CommandSeek
withFilesInGitAnnex ww a l = seekFilteredKeys a $
	seekHelper fst3 ww LsFiles.inRepoDetails l

withFilesInGitAnnexNonRecursive :: WarnUnmatchWhen -> String -> AnnexedFileSeeker -> [WorkTreeItem] -> CommandSeek
withFilesInGitAnnexNonRecursive ww needforce a l = ifM (Annex.getState Annex.force)
	( withFilesInGitAnnex ww a l
	, if null l
		then giveup needforce
		else seekFilteredKeys a (getfiles [] l)
	)
  where
	getfiles c [] = return (reverse c)
	getfiles c ((WorkTreeItem p):ps) = do
		os <- seekOptions ww
		(fs, cleanup) <- inRepo $ LsFiles.inRepoDetails os [toRawFilePath p]
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
	-> AnnexedFileSeeker
	-> ((Key, ActionItem) -> CommandSeek)
	-> ([WorkTreeItem] -> CommandSeek)
	-> [WorkTreeItem]
	-> CommandSeek
withKeyOptions ko auto seeker keyaction = withKeyOptions' ko auto mkkeyaction
  where
	mkkeyaction = do
		matcher <- Limit.getMatcher
		return $ \v@(k, ai) -> checkseeker k $
			let i = case ai of
				ActionItemBranchFilePath (BranchFilePath _ topf) _ ->
					MatchingKey k (AssociatedFile $ Just $ getTopFilePath topf)
				_ -> MatchingKey k (AssociatedFile Nothing)
			in whenM (matcher i) $
				keyaction v
	checkseeker k a = case checkContentPresent seeker of
		Nothing -> a
		Just v -> do
			present <- inAnnex k
			when (present == v) a

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
		let getk f = fmap (,f) (locationLogFileKey config f)
		let go reader = liftIO reader >>= \case
			Nothing -> return ()
			Just ((k, f), content) -> do
				maybe noop (Annex.BranchState.setCache f) content
				keyaction (k, mkActionItem k)
				go reader
		catObjectStreamLsTree l (getk . getTopFilePath . LsTree.file) g go
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

-- This is significantly faster than using lookupKey after seekFiltered,
-- because of the way data is streamed through git cat-file.
--
-- It can also precache location logs using the same efficient streaming.
seekFilteredKeys :: AnnexedFileSeeker -> Annex [(RawFilePath, Git.Sha, FileMode)] -> Annex ()
seekFilteredKeys seeker listfs = do
	g <- Annex.gitRepo
	matcher <- Limit.getMatcher
	config <- Annex.getGitConfig
	-- Run here, not in the async, because it could throw an exception
	-- The list should be built lazily.
	l <- listfs
	catObjectMetaDataStream g $ \mdfeeder mdcloser mdreader ->
		catObjectStream g $ \ofeeder ocloser oreader -> do
			processertid <- liftIO . async =<< forkState
				(process matcher ofeeder mdfeeder mdcloser False l)
			mdprocessertid <- liftIO . async =<< forkState
				(mdprocess matcher mdreader ofeeder ocloser)
			if usesLocationLog seeker
				then catObjectStream g $ \lfeeder lcloser lreader -> do
					precachertid <- liftIO . async =<< forkState
						(precacher config oreader lfeeder lcloser)
					precachefinisher lreader
					join (liftIO (wait precachertid))
				else finisher oreader
			join (liftIO (wait mdprocessertid))
			join (liftIO (wait processertid))
  where
	checkpresence k cont = case checkContentPresent seeker of
		Just v -> do
			present <- inAnnex k
			when (present == v) cont
		Nothing -> cont

	finisher oreader = liftIO oreader >>= \case
		Just (f, content) -> do
			case parseLinkTargetOrPointerLazy =<< content of
				Just k -> checkpresence k $
					commandAction $
						startAction seeker f k
				Nothing -> noop
			finisher oreader
		Nothing -> return ()

	precachefinisher lreader = liftIO lreader >>= \case
		Just ((logf, f, k), logcontent) -> do
			maybe noop (Annex.BranchState.setCache logf) logcontent
			commandAction $ startAction seeker f k
			precachefinisher lreader
		Nothing -> return ()
	
	precacher config oreader lfeeder lcloser = liftIO oreader >>= \case
		Just (f, content) -> do
			case parseLinkTargetOrPointerLazy =<< content of
				Just k -> checkpresence k $
					let logf = locationLogFile config k
					    ref = Git.Ref.branchFileRef Annex.Branch.fullname logf
					in liftIO $ lfeeder ((logf, f, k), ref)
				Nothing -> noop
			precacher config oreader lfeeder lcloser
		Nothing -> liftIO $ void lcloser
	
	feedmatches matcher ofeeder f sha = 
		whenM (matcher $ MatchingFile $ FileInfo f f) $
			liftIO $ ofeeder (f, sha)

	process matcher ofeeder mdfeeder mdcloser seenpointer ((f, sha, mode):rest) =
		case Git.toTreeItemType mode of
			Just Git.TreeSymlink -> do
				whenM (exists f) $
					-- Once a pointer file has been seen,
					-- symlinks have to be sent via the 
					-- metadata processor too. That is slightly
					-- slower, but preserves the requested
					-- file order.
					if seenpointer
						then liftIO $ mdfeeder (f, sha)
						else feedmatches matcher ofeeder f sha
				process matcher ofeeder mdfeeder mdcloser seenpointer rest
			Just Git.TreeSubmodule ->
				process matcher ofeeder mdfeeder mdcloser seenpointer rest
			-- Might be a pointer file, might be other
			-- file in git, possibly large. Avoid catting
			-- large files by first looking up the size.
			Just _ -> do
				whenM (exists f) $
					liftIO $ mdfeeder (f, sha)
				process matcher ofeeder mdfeeder mdcloser True rest
			Nothing ->
				process matcher ofeeder mdfeeder mdcloser seenpointer rest
	process _ _ _ mdcloser _ [] = liftIO $ void mdcloser
	
	-- Check if files exist, because a deleted file will still be
	-- listed by ls-tree, but should not be processed.
	exists p = isJust <$> liftIO (catchMaybeIO $ R.getSymbolicLinkStatus p)

	mdprocess matcher mdreader ofeeder ocloser = liftIO mdreader >>= \case
		Just (f, Just (sha, size, _type))
			| size < maxPointerSz -> do
				feedmatches matcher ofeeder f sha
				mdprocess matcher mdreader ofeeder ocloser
		Just _ -> mdprocess matcher mdreader ofeeder ocloser
		Nothing -> liftIO $ void ocloser

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
