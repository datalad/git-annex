{- git-annex command seeking
 - 
 - These functions find appropriate files or other things based on
 - the values a user passes to a command, and prepare actions operating
 - on them.
 -
 - Copyright 2010-2018 Joey Hess <id@joeyh.name>
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
import Git.FilePath
import qualified Limit
import CmdLine.GitAnnex.Options
import Logs.Location
import Logs.Unused
import Types.Transfer
import Logs.Transfer
import Remote.List
import qualified Remote
import Annex.CatFile
import Annex.CurrentBranch
import Annex.Content
import Annex.InodeSentinal
import qualified Database.Keys
import qualified Utility.RawFilePath as R

withFilesInGit :: (RawFilePath -> CommandSeek) -> [WorkTreeItem] -> CommandSeek
withFilesInGit a l = seekActions $ prepFiltered a $
	seekHelper LsFiles.inRepo l

withFilesInGitNonRecursive :: String -> (RawFilePath -> CommandSeek) -> [WorkTreeItem] -> CommandSeek
withFilesInGitNonRecursive needforce a l = ifM (Annex.getState Annex.force)
	( withFilesInGit a l
	, if null l
		then giveup needforce
		else seekActions $ prepFiltered a (getfiles [] l)
	)
  where
	getfiles c [] = return (reverse c)
	getfiles c ((WorkTreeItem p):ps) = do
		(fs, cleanup) <- inRepo $ LsFiles.inRepo [toRawFilePath p]
		case fs of
			[f] -> do
				void $ liftIO $ cleanup
				getfiles (f:c) ps
			[] -> do
				void $ liftIO $ cleanup
				getfiles c ps
			_ -> giveup needforce

withFilesNotInGit :: Bool -> (RawFilePath -> CommandSeek) -> [WorkTreeItem] -> CommandSeek
withFilesNotInGit skipdotfiles a l
	| skipdotfiles = do
		{- dotfiles are not acted on unless explicitly listed -}
		files <- filter (not . dotfile . fromRawFilePath) <$>
			seekunless (null ps && not (null l)) ps
		dotfiles <- seekunless (null dotps) dotps
		go (files++dotfiles)
	| otherwise = go =<< seekunless False l
  where
	(dotps, ps) = partition (\(WorkTreeItem f) -> dotfile f) l
	seekunless True _ = return []
	seekunless _ l' = do
		force <- Annex.getState Annex.force
		g <- gitRepo
		liftIO $ Git.Command.leaveZombie
			<$> LsFiles.notInRepo force (map (\(WorkTreeItem f) -> toRawFilePath f) l') g
	go fs = seekActions $ prepFiltered a $
		return $ concat $ segmentPaths (map (\(WorkTreeItem f) -> toRawFilePath f) l) fs

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
withWords a params = seekActions $ return [a params]

withStrings :: (String -> CommandSeek) -> CmdParams -> CommandSeek
withStrings a params = seekActions $ return $ map a params

withPairs :: ((String, String) -> CommandSeek) -> CmdParams -> CommandSeek
withPairs a params = seekActions $ return $ map a $ pairs [] params
  where
	pairs c [] = reverse c
	pairs c (x:y:xs) = pairs ((x,y):c) xs
	pairs _ _ = giveup "expected pairs"

withFilesToBeCommitted :: (RawFilePath -> CommandSeek) -> [WorkTreeItem] -> CommandSeek
withFilesToBeCommitted a l = seekActions $ prepFiltered a $
	seekHelper LsFiles.stagedNotDeleted l

isOldUnlocked :: RawFilePath -> Annex Bool
isOldUnlocked f = liftIO (notSymlink f) <&&> 
	(isJust <$> catKeyFile f <||> isJust <$> catKeyFileHEAD f)

{- unlocked pointer files that are staged, and whose content has not been
 - modified-}
withUnmodifiedUnlockedPointers :: (RawFilePath -> CommandSeek) -> [WorkTreeItem] -> CommandSeek
withUnmodifiedUnlockedPointers a l = seekActions $
	prepFiltered a unlockedfiles
  where
	unlockedfiles = filterM isUnmodifiedUnlocked 
		=<< seekHelper LsFiles.typeChangedStaged l

isUnmodifiedUnlocked :: RawFilePath -> Annex Bool
isUnmodifiedUnlocked f = catKeyFile f >>= \case
	Nothing -> return False
	Just k -> sameInodeCache f =<< Database.Keys.getInodeCaches k

{- Finds files that may be modified. -}
withFilesMaybeModified :: (RawFilePath -> CommandSeek) -> [WorkTreeItem] -> CommandSeek
withFilesMaybeModified a params = seekActions $
	prepFiltered a $ seekHelper LsFiles.modified params

withKeys :: (Key -> CommandSeek) -> CmdParams -> CommandSeek
withKeys a l = seekActions $ return $ map (a . parse) l
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
			| bare -> noauto $ runkeyaction finishCheck loggedKeys 
			| otherwise -> fallbackaction params
		(False, Nothing) -> fallbackaction params
		(True, Just WantAllKeys) -> noauto $ runkeyaction finishCheck loggedKeys
		(True, Just WantUnusedKeys) -> noauto $ runkeyaction (pure . Just) unusedKeys'
		(True, Just WantFailedTransfers) -> noauto runfailedtransfers
		(True, Just (WantSpecificKey k)) -> noauto $ runkeyaction (pure . Just) (return [k])
		(True, Just WantIncompleteKeys) -> noauto $ runkeyaction (pure . Just) incompletekeys
		(True, Just (WantBranchKeys bs)) -> noauto $ runbranchkeys bs
		(False, Just _) -> giveup "Can only specify one of file names, --all, --branch, --unused, --failed, --key, or --incomplete"
  where
	noauto a
		| auto = giveup "Cannot use --auto with --all or --branch or --unused or --key or --incomplete"
		| otherwise = a
	incompletekeys = staleKeysPrune gitAnnexTmpObjectDir True
	runkeyaction checker getks = do
		keyaction <- mkkeyaction
		ks <- getks
		forM_ ks $ checker >=> maybe noop 
			(\k -> keyaction (k, mkActionItem k))
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

prepFiltered :: (RawFilePath -> CommandSeek) -> Annex [RawFilePath] -> Annex [CommandSeek]
prepFiltered a fs = do
	matcher <- Limit.getMatcher
	map (process matcher) <$> fs
  where
	process matcher f =
		whenM (matcher $ MatchingFile $ FileInfo f f) $ a f

seekActions :: Annex [CommandSeek] -> Annex ()
seekActions gen = sequence_ =<< gen

seekHelper :: ([RawFilePath] -> Git.Repo -> IO ([RawFilePath], IO Bool)) -> [WorkTreeItem] -> Annex [RawFilePath]
seekHelper a l = inRepo $ \g ->
	concat . concat <$> forM (segmentXargsOrdered l')
		(runSegmentPaths (\fs -> Git.Command.leaveZombie <$> a fs g) . map toRawFilePath)
  where
	l' = map (\(WorkTreeItem f) -> f) l

-- An item in the work tree, which may be a file or a directory.
newtype WorkTreeItem = WorkTreeItem FilePath

-- When in an adjusted branch that hides some files, it may not exist
-- in the current work tree, but in the original branch. This allows
-- seeking for such files.
newtype AllowHidden = AllowHidden Bool

-- Many git commands seek work tree items matching some criteria,
-- and silently skip over anything that does not exist. But users expect
-- an error message when one of the files they provided as a command-line
-- parameter doesn't exist, so this checks that each exists.
workTreeItems :: CmdParams -> Annex [WorkTreeItem]
workTreeItems = workTreeItems' (AllowHidden False)

workTreeItems' :: AllowHidden -> CmdParams -> Annex [WorkTreeItem]
workTreeItems' (AllowHidden allowhidden) ps = do
	currbranch <- getCurrentBranch
	forM_ ps $ \p ->
		unlessM (exists p <||> hidden currbranch p) $ do
			toplevelWarning False (p ++ " not found")
			Annex.incError
	return (map (WorkTreeItem) ps)
  where
	exists p = isJust <$> liftIO (catchMaybeIO $ getSymbolicLinkStatus p)
	hidden currbranch p
		| allowhidden = do
			f <- liftIO $ relPathCwdToFile p
			isJust <$> catObjectMetaDataHidden (toRawFilePath f) currbranch
		| otherwise = return False

notSymlink :: RawFilePath -> IO Bool
notSymlink f = liftIO $ not . isSymbolicLink <$> R.getSymbolicLinkStatus f
