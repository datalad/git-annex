{- git-annex command seeking
 - 
 - These functions find appropriate files or other things based on
 - the values a user passes to a command, and prepare actions operating
 - on them.
 -
 - Copyright 2010-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

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
import Git.Types (toTreeItemType, TreeItemType(..))
import Git.FilePath
import qualified Limit
import CmdLine.GitAnnex.Options
import CmdLine.Action
import Logs
import Logs.Unused
import Logs.Location
import Types.Transfer
import Logs.Transfer
import Types.Link
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
import qualified Database.Keys
import qualified Utility.RawFilePath as R
import Utility.Tuple
import Utility.HumanTime

import Control.Concurrent.Async
import Control.Concurrent.STM
import System.Posix.Types
import Data.IORef
import Data.Time.Clock.POSIX
import System.PosixCompat.Files (isDirectory, isSymbolicLink, deviceID, fileID)
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
withFilesInGitAnnexNonRecursive ww needforce a (WorkTreeItems l) = ifM (Annex.getRead Annex.force)
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
				propagateLsFilesError cleanup
				fst <$> getfiles ((SeekInput [p], f):c) ps
			[] -> do
				propagateLsFilesError cleanup
				fst <$> getfiles c ps
			_ -> do
				propagateLsFilesError cleanup
				giveup needforce
		return (r, pure True)
withFilesInGitAnnexNonRecursive _ _ _ NoWorkTreeItems = noop

withFilesNotInGit :: CheckGitIgnore -> WarnUnmatchWhen -> ((SeekInput, RawFilePath) -> CommandSeek) -> WorkTreeItems -> CommandSeek
withFilesNotInGit (CheckGitIgnore ci) ww a l = do
	force <- Annex.getRead Annex.force
	let include_ignored = force || not ci
	seekFiltered (const (pure True)) a $
		seekHelper id ww (const $ LsFiles.notInRepo [] include_ignored) l

withPathContents :: ((RawFilePath, RawFilePath) -> CommandSeek) -> CmdParams -> CommandSeek
withPathContents a params = do
	matcher <- Limit.getMatcher
	checktimelimit <- mkCheckTimeLimit
	go matcher checktimelimit params []
  where
	go _ _ [] [] = return ()
	go matcher checktimelimit (p:ps) [] =
		go matcher checktimelimit ps =<< liftIO (get p)
	go matcher checktimelimit ps (f:fs) = checktimelimit noop $ do
		whenM (checkmatch matcher f) $
			a f
		go matcher checktimelimit ps fs		
	
	-- Using getFileStatus not getSymbolicLinkStatus because it should
	-- fail if the path that the user provided is a broken symlink,
	-- the same as it fails if the path that the user provided does not
	-- exist.
	get p = ifM (isDirectory <$> R.getFileStatus p')
		( map (\f -> 
			let f' = toRawFilePath f
			in (f', P.makeRelative (P.takeDirectory (P.dropTrailingPathSeparator p')) f'))
			<$> dirContentsRecursiveSkipping (".git" `isSuffixOf`) False p
		, return [(p', P.takeFileName p')]
		)
	  where
		p' = toRawFilePath p

	checkmatch matcher (f, relf) = matcher $ MatchingFile $ FileInfo
		{ contentFile = f
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

withFilesToBeCommitted :: WarnUnmatchWhen -> ((SeekInput, RawFilePath) -> CommandSeek) -> WorkTreeItems -> CommandSeek
withFilesToBeCommitted ww a l = seekFiltered (const (pure True)) a $
	seekHelper id ww (const LsFiles.stagedNotDeleted) l

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
		return $ \lt v@(_si, k, ai) -> checkseeker k $
			let i = case ai of
				ActionItemBranchFilePath (BranchFilePath _ topf) _ ->
					ProvidedInfo
						{ providedFilePath = Just $
							getTopFilePath topf
						, providedKey = Just k
						, providedFileSize = Nothing
						, providedMimeType = Nothing
						, providedMimeEncoding = Nothing
						, providedLinkType = lt
						}
				_ -> ProvidedInfo
					{ providedFilePath = Nothing
					, providedKey = Just k
					, providedFileSize = Nothing
					, providedMimeType = Nothing
					, providedMimeEncoding = Nothing
					, providedLinkType = lt
					}
			in whenM (matcher (MatchingInfo i)) $
				keyaction v
	checkseeker k a = case checkContentPresent seeker of
		Nothing -> a
		Just v -> do
			present <- inAnnex k
			when (present == v) a

withKeyOptions' 
	:: Maybe KeyOptions
	-> Bool
	-> Annex (Maybe LinkType -> (SeekInput, Key, ActionItem) -> Annex ())
	-> (WorkTreeItems -> CommandSeek)
	-> WorkTreeItems
	-> CommandSeek
withKeyOptions' ko auto mkkeyaction fallbackaction worktreeitems = do
	bare <- fromRepo Git.repoIsLocalBare
	when (auto && bare) $
		giveup "Cannot use --auto in a bare repository"
	case (nospecifiedworktreeitems, ko) of
		(True, Nothing)
			| bare -> nofilename $ noauto runallkeys
			| otherwise -> fallbackaction worktreeitems
		(False, Nothing) -> fallbackaction worktreeitems
		(True, Just WantAllKeys) -> nofilename $ noauto runallkeys
		(True, Just WantUnusedKeys) -> nofilename $ noauto $ runkeyaction unusedKeys'
		(True, Just WantFailedTransfers) -> nofilename $ noauto runfailedtransfers
		(True, Just (WantSpecificKey k)) -> nofilename $ noauto $ runkeyaction (return [k])
		(True, Just WantIncompleteKeys) -> nofilename $ noauto $ runkeyaction incompletekeys
		(True, Just (WantBranchKeys bs)) -> noauto $ runbranchkeys bs
		(False, Just _) -> giveup "Can only specify one of file names, --all, --branch, --unused, --failed, --key, or --incomplete"
  where
	noauto a
		| auto = giveup "Cannot use --auto with --all or --branch or --unused or --key or --incomplete"
		| otherwise = a
			
	nofilename a = ifM (Limit.introspect matchNeedsFileName)
		( do
			bare <- fromRepo Git.repoIsLocalBare
			if bare
				then giveup "Cannot use options that match on file names in a bare repository."
				else giveup "Cannot use --all or --unused or --key or --incomplete with options that match on file names."
		, a
		)

	nospecifiedworktreeitems = case worktreeitems of
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
		checktimelimit <- mkCheckTimeLimit
		keyaction <- mkkeyaction
		config <- Annex.getGitConfig
		
		let getk = locationLogFileKey config
		let discard reader = reader >>= \case
			Nothing -> noop
			Just _ -> discard reader
		let go reader = reader >>= \case
			Just (k, f, content) -> checktimelimit (discard reader) $ do
				maybe noop (Annex.Branch.precache f) content
				unlessM (checkDead k) $
					keyaction Nothing (SeekInput [], k, mkActionItem k)
				go reader
			Nothing -> return ()
		Annex.Branch.overBranchFileContents getk go >>= \case
			Just r -> return r
			Nothing -> giveup "This repository is read-only, and there are unmerged git-annex branches, which prevents operating on all keys. (Set annex.merge-annex-branches to false to ignore the unmerged git-annex branches.)"

	runkeyaction getks = do
		keyaction <- mkkeyaction
		ks <- getks
		forM_ ks $ \k -> keyaction Nothing (SeekInput [], k, mkActionItem k)
	
	runbranchkeys bs = do
		keyaction <- mkkeyaction
		forM_ bs $ \b -> do
			(l, cleanup) <- inRepo $ LsTree.lsTree LsTree.LsTreeRecursive (LsTree.LsTreeLong False) b
			forM_ l $ \i -> catKey (LsTree.sha i) >>= \case
				Just k -> 
					let bfp = mkActionItem (BranchFilePath b (LsTree.file i), k)
					    lt = case toTreeItemType (LsTree.mode i) of
						Just TreeSymlink -> Just LockedLink
						Just TreeFile -> Just UnlockedLink
						Just TreeExecutable -> Just UnlockedLink
						_ -> Nothing
					in keyaction lt (SeekInput [], k, bfp)
				Nothing -> noop
			unlessM (liftIO cleanup) $
				giveup ("git ls-tree " ++ Git.fromRef b ++ " failed")
	
	runfailedtransfers = do
		keyaction <- mkkeyaction
		rs <- remoteList
		ts <- concat <$> mapM (getFailedTransfers . Remote.uuid) rs
		forM_ ts $ \(t, i) ->
			keyaction Nothing (SeekInput [], transferKey t, mkActionItem (t, i))

seekFiltered :: ((SeekInput, RawFilePath) -> Annex Bool) -> ((SeekInput, RawFilePath) -> CommandSeek) -> Annex ([(SeekInput, RawFilePath)], IO Bool) -> Annex ()
seekFiltered prefilter a listfs = do
	matcher <- Limit.getMatcher
	checktimelimit <- mkCheckTimeLimit
	(fs, cleanup) <- listfs
	go matcher checktimelimit fs
	propagateLsFilesError cleanup
  where
	go _ _ [] = return ()
	go matcher checktimelimit (v@(_si, f):rest) = checktimelimit noop $ do
		whenM (prefilter v) $
			whenM (matcher $ MatchingFile $ FileInfo f f Nothing) $
				a v
		go matcher checktimelimit rest

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
	checktimelimit <- mkCheckTimeLimit
	catObjectMetaDataStream g $ \mdfeeder mdcloser mdreader ->
		catObjectStream g $ \ofeeder ocloser oreader -> do
			processertid <- liftIO . async =<< forkState
				(process mi ofeeder mdfeeder mdcloser False l)
			mdprocessertid <- liftIO . async =<< forkState
				(mdprocess mi mdreader ofeeder ocloser)
			ifM (precachell mi)
				( catObjectStream g $ \lfeeder lcloser lreader -> do
					precachertid <- liftIO . async =<< forkState
						(precacher mi config oreader lfeeder lcloser)
					precachefinisher mi lreader checktimelimit
					join (liftIO (wait precachertid))
				, finisher mi oreader checktimelimit
				)
			join (liftIO (wait mdprocessertid))
			join (liftIO (wait processertid))
	propagateLsFilesError cleanup
  where
	finisher mi oreader checktimelimit = liftIO oreader >>= \case
		Just ((si, f), content) -> checktimelimit (liftIO discard) $ do
			keyaction f mi content $ 
				commandAction . startAction seeker si f
			finisher mi oreader checktimelimit
		Nothing -> return ()
	  where
		discard = oreader >>= \case
			Nothing -> return ()
			Just _ -> discard

	precachefinisher mi lreader checktimelimit = liftIO lreader >>= \case
		Just ((logf, (si, f), k), logcontent) -> checktimelimit (liftIO discard) $ do
			maybe noop (Annex.Branch.precache logf) logcontent
			checkMatcherWhen mi
				(matcherNeedsLocationLog mi && not (matcherNeedsFileName mi))
				(MatchingFile $ FileInfo f f (Just k))
				(commandAction $ startAction seeker si f k)
			precachefinisher mi lreader checktimelimit
		Nothing -> return ()
	  where
		discard = lreader >>= \case
			Nothing -> return ()
			Just _ -> discard
	
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
		(MatchingFile $ FileInfo f f Nothing)
		(liftIO $ ofeeder ((si, f), sha))

	keyaction f mi content a = 
		case parseLinkTargetOrPointerLazy =<< content of
			Just k -> checkMatcherWhen mi
				(matcherNeedsKey mi && not (matcherNeedsFileName mi || matcherNeedsLocationLog mi))
				(MatchingFile $ FileInfo f f (Just k))
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
			| size < fromIntegral maxPointerSz -> do
				feedmatches mi ofeeder si f sha
				mdprocess mi mdreader ofeeder ocloser
		Just _ -> mdprocess mi mdreader ofeeder ocloser
		Nothing -> liftIO $ void ocloser

	-- Precache location logs if it will speed things up.
	--
	-- When there are git-annex branches that are not able to be
	-- merged, the precaching is disabled, since it only looks at the
	-- git-annex branch and not at those.
	precachell mi
		| usesLocationLog seeker || matcherNeedsLocationLog mi =
			null <$> Annex.Branch.getUnmergedRefs
		| otherwise = pure False

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

data WarnUnmatchWhen = WarnUnmatchLsFiles String | WarnUnmatchWorkTreeItems String

seekOptions :: WarnUnmatchWhen -> Annex [LsFiles.Options]
seekOptions (WarnUnmatchLsFiles _) =
	ifM (annexSkipUnknown <$> Annex.getGitConfig)
		( return [] 
		, return [LsFiles.ErrorUnmatch]
		)
seekOptions (WarnUnmatchWorkTreeItems _) = return []

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
	(WarnUnmatchWorkTreeItems action) -> runcheck action
	(WarnUnmatchLsFiles action) -> 
		ifM (annexSkipUnknown <$> Annex.getGitConfig)
			( runcheck action
			, return $ WorkTreeItems ps
			)
  where
	runcheck action = do
		currbranch <- getCurrentBranch
		stopattop <- prepviasymlink
		ps' <- flip filterM ps $ \p -> do
			let p' = toRawFilePath p
			relf <- liftIO $ relPathCwdToFile p'
			ifM (not <$> (exists p' <||> hidden currbranch relf))
				( prob action FileNotFound p' "not found"
				, ifM (viasymlink stopattop (upFrom relf))
					( prob action FileBeyondSymbolicLink p' "is beyond a symbolic link"
					, return True
					)
				)
		if null ps' && not (null ps)
			then return NoWorkTreeItems
			else return (WorkTreeItems ps')
	
	exists p = isJust <$> liftIO (catchMaybeIO $ R.getSymbolicLinkStatus p)

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

	prob action errorid p msg = do
		toplevelFileProblem False errorid msg action p Nothing (SeekInput [fromRawFilePath p])
		Annex.incError
		return False
	
notSymlink :: RawFilePath -> IO Bool
notSymlink f = liftIO $ not . isSymbolicLink <$> R.getSymbolicLinkStatus f

{- Returns an action that, when there's a time limit, can be used
 - to check it before processing a file. The first action is run when
 - over the time limit, otherwise the second action is run one time to
 - clean up. -}
mkCheckTimeLimit :: Annex (Annex () -> Annex () -> Annex ())
mkCheckTimeLimit = Annex.getState Annex.timelimit >>= \case
	Nothing -> return $ \_ a -> a
	Just (duration, cutoff) -> do
		warningshownv <- liftIO $ newTVarIO False
		return $ \cleanup a -> do
			now <- liftIO getPOSIXTime
			if now > cutoff
				then do
					warningshown <- liftIO $ atomically $
						swapTVar warningshownv True
					unless warningshown $ do
						Annex.changeState $ \s -> s { Annex.reachedlimit = True }
						warning $ UnquotedString $ "Time limit (" ++ fromDuration duration ++ ") reached! Shutting down..."
						cleanup
				else a

propagateLsFilesError :: IO Bool -> Annex ()
propagateLsFilesError cleanup =
	unlessM (liftIO cleanup) $
		Annex.incError
