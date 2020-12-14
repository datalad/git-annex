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
import qualified Git.LsFiles as LsFiles
import qualified Git.LsTree as LsTree
import qualified Git.Types as Git
import qualified Git.Ref
import Git.FilePath
import qualified Limit
import CmdLine.GitAnnex.Options
import CmdLine.Action
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
import Annex.CheckIgnore
import qualified Annex.Branch
import qualified Annex.BranchState
import qualified Database.Keys
import qualified Utility.RawFilePath as R
import Utility.Tuple

import Control.Concurrent.Async
import System.Posix.Types
import Data.IORef
import qualified System.FilePath.ByteString as P

data AnnexedFileSeeker = AnnexedFileSeeker
	{ startAction :: SeekInput -> RawFilePath -> Key -> CommandStart
	, checkContentPresent :: Maybe Bool
	, usesLocationLog :: Bool
	}

withFilesInGitAnnex :: WarnUnmatchWhen -> AnnexedFileSeeker -> WorkTreeItems -> CommandSeek
withFilesInGitAnnex ww a l = seekFilteredKeys a $
	seekHelper fst3 ww LsFiles.inRepoDetails l

withFilesInGitAnnexNonRecursive :: WarnUnmatchWhen -> String -> AnnexedFileSeeker -> WorkTreeItems -> CommandSeek
withFilesInGitAnnexNonRecursive ww needforce a (WorkTreeItems l) = ifM (Annex.getState Annex.force)
	( withFilesInGitAnnex ww a (WorkTreeItems l)
	, if null l
		then giveup needforce
		else seekFilteredKeys a (getfiles [] l)
	)
  where
	getfiles c [] = return (reverse c, pure True)
	getfiles c (p:ps) = do
		os <- seekOptions ww
		(fs, cleanup) <- inRepo $ LsFiles.inRepoDetails os [toRawFilePath p]
		r <- case fs of
			[f] -> do
				void $ liftIO $ cleanup
				fst <$> getfiles ((SeekInput [p], f):c) ps
			[] -> do
				void $ liftIO $ cleanup
				fst <$> getfiles c ps
			_ -> do
				void $ liftIO $ cleanup
				giveup needforce
		return (r, pure True)
withFilesInGitAnnexNonRecursive _ _ _ NoWorkTreeItems = noop

withFilesNotInGit :: CheckGitIgnore -> WarnUnmatchWhen -> ((SeekInput, RawFilePath) -> CommandSeek) -> WorkTreeItems -> CommandSeek
withFilesNotInGit (CheckGitIgnore ci) ww a l = do
	force <- Annex.getState Annex.force
	let include_ignored = force || not ci
	seekFiltered (const (pure True)) a $
		seekHelper id ww (const $ LsFiles.notInRepo [] include_ignored) l

withPathContents :: ((RawFilePath, RawFilePath) -> CommandSeek) -> CmdParams -> CommandSeek
withPathContents a params = do
	matcher <- Limit.getMatcher
	forM_ params $ \p -> do
		fs <- liftIO $ get p
		forM fs $ \f ->
			whenM (checkmatch matcher f) $
				a f
  where
	get p = ifM (isDirectory <$> getFileStatus p)
		( map (\f -> 
			let f' = toRawFilePath f
			in (f', P.makeRelative (P.takeDirectory (P.dropTrailingPathSeparator p')) f'))
			<$> dirContentsRecursiveSkipping (".git" `isSuffixOf`) True p
		, return [(p', P.takeFileName p')]
		)
	  where
		p' = toRawFilePath p

	checkmatch matcher (f, relf) = matcher $ MatchingFile $ FileInfo
		{ contentFile = Just f
		, matchFile = relf
		, matchKey = Nothing
		}

withWords :: ([String] -> CommandSeek) -> CmdParams -> CommandSeek
withWords a params = a params

withStrings :: (String -> CommandSeek) -> CmdParams -> CommandSeek
withStrings a params = sequence_ $ map a params

withPairs :: ((SeekInput, (String, String)) -> CommandSeek) -> CmdParams -> CommandSeek
withPairs a params = sequence_ $ 
	map (\p@(x,y) -> a (SeekInput [x,y], p)) (pairs [] params)
  where
	pairs c [] = reverse c
	pairs c (x:y:xs) = pairs ((x,y):c) xs
	pairs _ _ = giveup "expected pairs"

withFilesToBeCommitted :: ((SeekInput, RawFilePath) -> CommandSeek) -> WorkTreeItems -> CommandSeek
withFilesToBeCommitted a l = seekFiltered (const (pure True)) a $
	seekHelper id WarnUnmatchWorkTreeItems (const LsFiles.stagedNotDeleted) l

{- unlocked pointer files that are staged, and whose content has not been
 - modified-}
withUnmodifiedUnlockedPointers :: WarnUnmatchWhen -> ((SeekInput, RawFilePath) -> CommandSeek) -> WorkTreeItems -> CommandSeek
withUnmodifiedUnlockedPointers ww a l =
	seekFiltered (isUnmodifiedUnlocked . snd) a $
		seekHelper id ww (const LsFiles.typeChangedStaged) l

isUnmodifiedUnlocked :: RawFilePath -> Annex Bool
isUnmodifiedUnlocked f = catKeyFile f >>= \case
	Nothing -> return False
	Just k -> sameInodeCache f =<< Database.Keys.getInodeCaches k

{- Finds files that may be modified. -}
withFilesMaybeModified :: WarnUnmatchWhen -> ((SeekInput, RawFilePath) -> CommandSeek) -> WorkTreeItems -> CommandSeek
withFilesMaybeModified ww a params = seekFiltered (const (pure True)) a $
	seekHelper id ww LsFiles.modified params

withKeys :: ((SeekInput, Key) -> CommandSeek) -> CmdParams -> CommandSeek
withKeys a ls = sequence_ $ map (\l -> a (SeekInput [l], parse l)) ls
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
	-> ((SeekInput, Key, ActionItem) -> CommandSeek)
	-> (WorkTreeItems -> CommandSeek)
	-> WorkTreeItems
	-> CommandSeek
withKeyOptions ko auto seeker keyaction = withKeyOptions' ko auto mkkeyaction
  where
	mkkeyaction = do
		matcher <- Limit.getMatcher
		return $ \v@(_si, k, ai) -> checkseeker k $
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
	-> Annex ((SeekInput, Key, ActionItem) -> Annex ())
	-> (WorkTreeItems -> CommandSeek)
	-> WorkTreeItems
	-> CommandSeek
withKeyOptions' ko auto mkkeyaction fallbackaction worktreeitems = do
	bare <- fromRepo Git.repoIsLocalBare
	when (auto && bare) $
		giveup "Cannot use --auto in a bare repository"
	case (noworktreeitems, ko) of
		(True, Nothing)
			| bare -> noauto runallkeys
			| otherwise -> fallbackaction worktreeitems
		(False, Nothing) -> fallbackaction worktreeitems
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
	
	noworktreeitems = case worktreeitems of
		WorkTreeItems [] -> True
		WorkTreeItems _ -> False
		NoWorkTreeItems -> False

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
				keyaction (SeekInput [], k, mkActionItem k)
				go reader
		catObjectStreamLsTree l (getk . getTopFilePath . LsTree.file) g go
		liftIO $ void cleanup

	runkeyaction getks = do
		keyaction <- mkkeyaction
		ks <- getks
		forM_ ks $ \k -> keyaction (SeekInput [], k, mkActionItem k)
	
	runbranchkeys bs = do
		keyaction <- mkkeyaction
		forM_ bs $ \b -> do
			(l, cleanup) <- inRepo $ LsTree.lsTree LsTree.LsTreeRecursive b
			forM_ l $ \i -> catKey (LsTree.sha i) >>= \case
				Nothing -> noop
				Just k -> 
					let bfp = mkActionItem (BranchFilePath b (LsTree.file i), k)
					in keyaction (SeekInput [], k, bfp)
			unlessM (liftIO cleanup) $
				error ("git ls-tree " ++ Git.fromRef b ++ " failed")
	
	runfailedtransfers = do
		keyaction <- mkkeyaction
		rs <- remoteList
		ts <- concat <$> mapM (getFailedTransfers . Remote.uuid) rs
		forM_ ts $ \(t, i) ->
			keyaction (SeekInput [], transferKey t, mkActionItem (t, i))

seekFiltered :: ((SeekInput, RawFilePath) -> Annex Bool) -> ((SeekInput, RawFilePath) -> CommandSeek) -> Annex ([(SeekInput, RawFilePath)], IO Bool) -> Annex ()
seekFiltered prefilter a listfs = do
	matcher <- Limit.getMatcher
	(fs, cleanup) <- listfs
	sequence_ (map (process matcher) fs)
	liftIO $ void cleanup
  where
	process matcher v@(_si, f) =
		whenM (prefilter v) $
			whenM (matcher $ MatchingFile $ FileInfo (Just f) f Nothing) $
				a v

data MatcherInfo = MatcherInfo
	{ matcherAction :: MatchInfo -> Annex Bool
	, matcherNeedsFileName :: Bool
	, matcherNeedsKey :: Bool
	, matcherNeedsLocationLog :: Bool
	}

checkMatcherWhen :: MatcherInfo -> Bool -> MatchInfo -> Annex () -> Annex ()
checkMatcherWhen mi c i a
	| c = whenM (matcherAction mi i) a
	| otherwise = a

-- This is significantly faster than using lookupKey after seekFiltered,
-- because of the way data is streamed through git cat-file.
--
-- It can also precache location logs using the same efficient streaming.
seekFilteredKeys :: AnnexedFileSeeker -> Annex ([(SeekInput, (RawFilePath, Git.Sha, FileMode))], IO Bool) -> Annex ()
seekFilteredKeys seeker listfs = do
	g <- Annex.gitRepo
	mi <- MatcherInfo
		<$> Limit.getMatcher
		<*> Limit.introspect matchNeedsFileName
		<*> Limit.introspect matchNeedsKey
		<*> Limit.introspect matchNeedsLocationLog
	config <- Annex.getGitConfig
	(l, cleanup) <- listfs
	catObjectMetaDataStream g $ \mdfeeder mdcloser mdreader ->
		catObjectStream g $ \ofeeder ocloser oreader -> do
			processertid <- liftIO . async =<< forkState
				(process mi ofeeder mdfeeder mdcloser False l)
			mdprocessertid <- liftIO . async =<< forkState
				(mdprocess mi mdreader ofeeder ocloser)
			if usesLocationLog seeker || matcherNeedsLocationLog mi
				then catObjectStream g $ \lfeeder lcloser lreader -> do
					precachertid <- liftIO . async =<< forkState
						(precacher mi config oreader lfeeder lcloser)
					precachefinisher mi lreader
					join (liftIO (wait precachertid))
				else finisher mi oreader
			join (liftIO (wait mdprocessertid))
			join (liftIO (wait processertid))
	liftIO $ void cleanup
  where
	finisher mi oreader = liftIO oreader >>= \case
		Just ((si, f), content) -> do
			keyaction f mi content $ 
				commandAction . startAction seeker si f
			finisher mi oreader
		Nothing -> return ()

	precachefinisher mi lreader = liftIO lreader >>= \case
		Just ((logf, (si, f), k), logcontent) -> do
			maybe noop (Annex.BranchState.setCache logf) logcontent
			checkMatcherWhen mi
				(matcherNeedsLocationLog mi && not (matcherNeedsFileName mi))
				(MatchingKey k (AssociatedFile (Just f)))
				(commandAction $ startAction seeker si f k)
			precachefinisher mi lreader
		Nothing -> return ()
	
	precacher mi config oreader lfeeder lcloser = liftIO oreader >>= \case
		Just ((si, f), content) -> do
			keyaction f mi content $ \k -> 
				let logf = locationLogFile config k
				    ref = Git.Ref.branchFileRef Annex.Branch.fullname logf
				in liftIO $ lfeeder ((logf, (si, f), k), ref)
			precacher mi config oreader lfeeder lcloser
		Nothing -> liftIO $ void lcloser
	
	feedmatches mi ofeeder si f sha = checkMatcherWhen mi
		-- When the matcher needs a key or location log 
		-- (and does not need a worktree filename), it will be
		-- checked later, to avoid a slow lookup here.
		(not ((matcherNeedsKey mi || matcherNeedsLocationLog mi) 
			&& not (matcherNeedsFileName mi)))
		(MatchingFile $ FileInfo (Just f) f Nothing)
		(liftIO $ ofeeder ((si, f), sha))

	keyaction f mi content a = 
		case parseLinkTargetOrPointerLazy =<< content of
			Just k -> checkMatcherWhen mi
				(matcherNeedsKey mi && not (matcherNeedsFileName mi || matcherNeedsLocationLog mi))
				(MatchingKey k (AssociatedFile (Just f)))
				(checkpresence k (a k))
			Nothing -> noop
	
	checkpresence k cont = case checkContentPresent seeker of
		Just v -> do
			present <- inAnnex k
			when (present == v) cont
		Nothing -> cont

	process mi ofeeder mdfeeder mdcloser seenpointer ((si, (f, sha, mode)):rest) =
		case Git.toTreeItemType mode of
			Just Git.TreeSymlink -> do
				whenM (exists f) $
					-- Once a pointer file has been seen,
					-- symlinks have to be sent via the 
					-- metadata processor too. That is
					-- slightly slower, but preserves the
					-- requested file order.
					if seenpointer
						then liftIO $ mdfeeder ((si, f), sha)
						else feedmatches mi ofeeder si f sha
				process mi ofeeder mdfeeder mdcloser seenpointer rest
			Just Git.TreeSubmodule ->
				process mi ofeeder mdfeeder mdcloser seenpointer rest
			-- Might be a pointer file, might be other
			-- file in git, possibly large. Avoid catting
			-- large files by first looking up the size.
			Just _ -> do
				whenM (exists f) $
					liftIO $ mdfeeder ((si, f), sha)
				process mi ofeeder mdfeeder mdcloser True rest
			Nothing ->
				process mi ofeeder mdfeeder mdcloser seenpointer rest
	process _ _ _ mdcloser _ [] = liftIO $ void mdcloser
	
	-- Check if files exist, because a deleted file will still be
	-- listed by ls-tree, but should not be processed.
	exists p = isJust <$> liftIO (catchMaybeIO $ R.getSymbolicLinkStatus p)

	mdprocess mi mdreader ofeeder ocloser = liftIO mdreader >>= \case
		Just ((si, f), Just (sha, size, _type))
			| size < maxPointerSz -> do
				feedmatches mi ofeeder si f sha
				mdprocess mi mdreader ofeeder ocloser
		Just _ -> mdprocess mi mdreader ofeeder ocloser
		Nothing -> liftIO $ void ocloser

seekHelper :: (a -> RawFilePath) -> WarnUnmatchWhen -> ([LsFiles.Options] -> [RawFilePath] -> Git.Repo -> IO ([a], IO Bool)) -> WorkTreeItems -> Annex ([(SeekInput, a)], IO Bool)
seekHelper c ww a (WorkTreeItems l) = do
	os <- seekOptions ww
	v <- liftIO $ newIORef []
	r <- inRepo $ \g -> concat . concat <$> forM (segmentXargsOrdered l)
		(runSegmentPaths' mk c (\fs -> go v os fs g) . map toRawFilePath)
	return (r, cleanupall v)
  where
	mk (Just i) f = (SeekInput [fromRawFilePath i], f) 
	-- This is not accurate, but it only happens when there are a
	-- great many input WorkTreeItems.
	mk Nothing f = (SeekInput [fromRawFilePath (c f)], f)

	go v os fs g = do
		(ls, cleanup) <- a os fs g
		liftIO $ modifyIORef' v (cleanup:)
		return ls

	cleanupall v = do
		cleanups <- readIORef v
		and <$> sequence cleanups
seekHelper _ _ _ NoWorkTreeItems = return ([], pure True)

data WarnUnmatchWhen = WarnUnmatchLsFiles | WarnUnmatchWorkTreeItems

seekOptions :: WarnUnmatchWhen -> Annex [LsFiles.Options]
seekOptions WarnUnmatchLsFiles =
	ifM (annexSkipUnknown <$> Annex.getGitConfig)
		( return [] 
		, return [LsFiles.ErrorUnmatch]
		)
seekOptions WarnUnmatchWorkTreeItems = return []

-- Items in the work tree, which may be files or directories.
data WorkTreeItems
	= WorkTreeItems [FilePath]
	-- ^ An empty list often means all files.
	| NoWorkTreeItems
	-- ^ Used when no work tree items should be operated on.
	deriving (Show)

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
workTreeItems :: WarnUnmatchWhen -> CmdParams -> Annex WorkTreeItems
workTreeItems = workTreeItems' (AllowHidden False)

workTreeItems' :: AllowHidden -> WarnUnmatchWhen -> CmdParams -> Annex WorkTreeItems
workTreeItems' (AllowHidden allowhidden) ww ps = case ww of
	WarnUnmatchWorkTreeItems -> runcheck
	WarnUnmatchLsFiles -> 
		ifM (annexSkipUnknown <$> Annex.getGitConfig)
			( runcheck
			, return $ WorkTreeItems ps
			)
  where
	runcheck = do
		currbranch <- getCurrentBranch
		stopattop <- prepviasymlink
		ps' <- flip filterM ps $ \p -> do
			relf <- liftIO $ relPathCwdToFile $ toRawFilePath p
			ifM (not <$> (exists p <||> hidden currbranch relf))
				( prob (p ++ " not found")
				, ifM (viasymlink stopattop (upFrom relf))
					( prob (p ++ " is beyond a symbolic link")
					, return True
					)
				)
		if null ps' && not (null ps)
			then return NoWorkTreeItems
			else return (WorkTreeItems ps')
	
	exists p = isJust <$> liftIO (catchMaybeIO $ getSymbolicLinkStatus p)

	prepviasymlink = do
		repotopst <- inRepo $ 
			maybe
				(pure Nothing)
				(catchMaybeIO . R.getSymbolicLinkStatus) 
			. Git.repoWorkTree
		return $ \st -> case repotopst of
			Nothing -> False
			Just tst -> fileID st == fileID tst
				&& deviceID st == deviceID tst

	viasymlink _ Nothing = return False
	viasymlink stopattop (Just p) = do
		st <- liftIO $ R.getSymbolicLinkStatus p
		if stopattop st
			then return False
			else if isSymbolicLink st
				then return True
				else viasymlink stopattop (upFrom p)

	hidden currbranch f
		| allowhidden = isJust
			<$> catObjectMetaDataHidden f currbranch
		| otherwise = return False

	prob msg = do
		toplevelWarning False msg
		Annex.incError
		return False
	
notSymlink :: RawFilePath -> IO Bool
notSymlink f = liftIO $ not . isSymbolicLink <$> R.getSymbolicLinkStatus f
