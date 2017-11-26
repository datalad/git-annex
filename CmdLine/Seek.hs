{- git-annex command seeking
 - 
 - These functions find appropriate files or other things based on
 - the values a user passes to a command, and prepare actions operating
 - on them.
 -
 - Copyright 2010-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
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
import CmdLine.Action
import Logs.Location
import Logs.Unused
import Types.Transfer
import Logs.Transfer
import Remote.List
import qualified Remote
import Annex.CatFile
import Annex.Content

withFilesInGit :: (FilePath -> CommandStart) -> [WorkTreeItem] -> CommandSeek
withFilesInGit a l = seekActions $ prepFiltered a $
	seekHelper LsFiles.inRepo l

withFilesInGitNonRecursive :: String -> (FilePath -> CommandStart) -> [WorkTreeItem] -> CommandSeek
withFilesInGitNonRecursive needforce a l = ifM (Annex.getState Annex.force)
	( withFilesInGit a l
	, if null l
		then giveup needforce
		else seekActions $ prepFiltered a (getfiles [] l)
	)
  where
	getfiles c [] = return (reverse c)
	getfiles c ((WorkTreeItem p):ps) = do
		(fs, cleanup) <- inRepo $ LsFiles.inRepo [p]
		case fs of
			[f] -> do
				void $ liftIO $ cleanup
				getfiles (f:c) ps
			[] -> do
				void $ liftIO $ cleanup
				getfiles c ps
			_ -> giveup needforce

withFilesNotInGit :: Bool -> (FilePath -> CommandStart) -> [WorkTreeItem] -> CommandSeek
withFilesNotInGit skipdotfiles a l
	| skipdotfiles = do
		{- dotfiles are not acted on unless explicitly listed -}
		files <- filter (not . dotfile) <$>
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
			<$> LsFiles.notInRepo force (map (\(WorkTreeItem f) -> f) l') g
	go fs = seekActions $ prepFiltered a $
		return $ concat $ segmentPaths (map (\(WorkTreeItem f) -> f) l) fs

withFilesInRefs :: (FilePath -> Key -> CommandStart) -> [Git.Ref] -> CommandSeek
withFilesInRefs a = mapM_ go
  where
	go r = do	
		matcher <- Limit.getMatcher
		(l, cleanup) <- inRepo $ LsTree.lsTree r
		forM_ l $ \i -> do
			let f = getTopFilePath $ LsTree.file i
			v <- catKey (LsTree.sha i)
			case v of
				Nothing -> noop
				Just k -> whenM (matcher $ MatchingKey k) $
					commandAction $ a f k
		liftIO $ void cleanup

withPathContents :: ((FilePath, FilePath) -> CommandStart) -> CmdParams -> CommandSeek
withPathContents a params = do
	matcher <- Limit.getMatcher
	seekActions $ map a <$> (filterM (checkmatch matcher) =<< ps)
  where
	ps = concat <$> liftIO (mapM get params)
	get p = ifM (isDirectory <$> getFileStatus p)
		( map (\f -> (f, makeRelative (parentDir p) f))
			<$> dirContentsRecursiveSkipping (".git" `isSuffixOf`) True p
		, return [(p, takeFileName p)]
		)
	checkmatch matcher (f, relf) = matcher $ MatchingFile $ FileInfo
		{ currFile = f
		, matchFile = relf
		}

withWords :: ([String] -> CommandStart) -> CmdParams -> CommandSeek
withWords a params = seekActions $ return [a params]

withStrings :: (String -> CommandStart) -> CmdParams -> CommandSeek
withStrings a params = seekActions $ return $ map a params

withPairs :: ((String, String) -> CommandStart) -> CmdParams -> CommandSeek
withPairs a params = seekActions $ return $ map a $ pairs [] params
  where
	pairs c [] = reverse c
	pairs c (x:y:xs) = pairs ((x,y):c) xs
	pairs _ _ = giveup "expected pairs"

withFilesToBeCommitted :: (FilePath -> CommandStart) -> [WorkTreeItem] -> CommandSeek
withFilesToBeCommitted a l = seekActions $ prepFiltered a $
	seekHelper LsFiles.stagedNotDeleted l

withFilesOldUnlocked :: (FilePath -> CommandStart) -> [WorkTreeItem] -> CommandSeek
withFilesOldUnlocked = withFilesOldUnlocked' LsFiles.typeChanged

withFilesOldUnlockedToBeCommitted :: (FilePath -> CommandStart) -> [WorkTreeItem] -> CommandSeek
withFilesOldUnlockedToBeCommitted = withFilesOldUnlocked' LsFiles.typeChangedStaged

{- Unlocked files before v6 have changed type from a symlink to a regular file.
 -
 - Furthermore, unlocked files used to be a git-annex symlink,
 - not some other sort of symlink.
 -}
withFilesOldUnlocked' :: ([FilePath] -> Git.Repo -> IO ([FilePath], IO Bool)) -> (FilePath -> CommandStart) -> [WorkTreeItem] -> CommandSeek
withFilesOldUnlocked' typechanged a l = seekActions $
	prepFiltered a unlockedfiles
  where
	unlockedfiles = filterM isOldUnlocked =<< seekHelper typechanged l

isOldUnlocked :: FilePath -> Annex Bool
isOldUnlocked f = liftIO (notSymlink f) <&&> 
	(isJust <$> catKeyFile f <||> isJust <$> catKeyFileHEAD f)

{- Finds files that may be modified. -}
withFilesMaybeModified :: (FilePath -> CommandStart) -> [WorkTreeItem] -> CommandSeek
withFilesMaybeModified a params = seekActions $
	prepFiltered a $ seekHelper LsFiles.modified params

withKeys :: (Key -> CommandStart) -> CmdParams -> CommandSeek
withKeys a l = seekActions $ return $ map (a . parse) l
  where
	parse p = fromMaybe (giveup "bad key") $ file2key p

withNothing :: CommandStart -> CmdParams -> CommandSeek
withNothing a [] = seekActions $ return [a]
withNothing _ _ = giveup "This command takes no parameters."

{- Handles the --all, --branch, --unused, --failed, --key, and
 - --incomplete options, which specify particular keys to run an
 - action on.
 -
 - In a bare repo, --all is the default.
 -
 - Otherwise falls back to a regular CommandSeek action on
 - whatever params were passed. -}
withKeyOptions 
	:: Maybe KeyOptions
	-> Bool
	-> (Key -> ActionItem -> CommandStart)
	-> ([WorkTreeItem] -> CommandSeek)
	-> [WorkTreeItem]
	-> CommandSeek
withKeyOptions ko auto keyaction = withKeyOptions' ko auto mkkeyaction
  where
	mkkeyaction = do
		matcher <- Limit.getMatcher
		return $ \k i ->
			whenM (matcher $ MatchingKey k) $
				commandAction $ keyaction k i

withKeyOptions' 
	:: Maybe KeyOptions
	-> Bool
	-> Annex (Key -> ActionItem -> Annex ())
	-> ([WorkTreeItem] -> CommandSeek)
	-> [WorkTreeItem]
	-> CommandSeek
withKeyOptions' ko auto mkkeyaction fallbackaction params = do
	bare <- fromRepo Git.repoIsLocalBare
	when (auto && bare) $
		giveup "Cannot use --auto in a bare repository"
	case (null params, ko) of
		(True, Nothing)
			| bare -> noauto $ runkeyaction loggedKeys
			| otherwise -> fallbackaction params
		(False, Nothing) -> fallbackaction params
		(True, Just WantAllKeys) -> noauto $ runkeyaction loggedKeys
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
	runkeyaction getks = do
		keyaction <- mkkeyaction
		ks <- getks
		forM_ ks $ \k -> keyaction k (mkActionItem k)
	runbranchkeys bs = do
		keyaction <- mkkeyaction
		forM_ bs $ \b -> do
			(l, cleanup) <- inRepo $ LsTree.lsTree b
			forM_ l $ \i -> do
				let bfp = mkActionItem $ BranchFilePath b (LsTree.file i)
				maybe noop (\k -> keyaction k bfp)
					=<< catKey (LsTree.sha i)
			unlessM (liftIO cleanup) $
				error ("git ls-tree " ++ Git.fromRef b ++ " failed")
	runfailedtransfers = do
		keyaction <- mkkeyaction
		rs <- remoteList
		ts <- concat <$> mapM (getFailedTransfers . Remote.uuid) rs
		forM_ ts $ \(t, i) ->
			keyaction (transferKey t) (mkActionItem (t, i))

prepFiltered :: (FilePath -> CommandStart) -> Annex [FilePath] -> Annex [CommandStart]
prepFiltered a fs = do
	matcher <- Limit.getMatcher
	map (process matcher) <$> fs
  where
	process matcher f = ifM (matcher $ MatchingFile $ FileInfo f f)
		( a f , return Nothing )

seekActions :: Annex [CommandStart] -> Annex ()
seekActions gen = mapM_ commandAction =<< gen

seekHelper :: ([FilePath] -> Git.Repo -> IO ([FilePath], IO Bool)) -> [WorkTreeItem] -> Annex [FilePath]
seekHelper a l = inRepo $ \g ->
	concat . concat <$> forM (segmentXargsOrdered l')
		(runSegmentPaths (\fs -> Git.Command.leaveZombie <$> a fs g))
  where
	l' = map (\(WorkTreeItem f) -> f) l

-- An item in the work tree, which may be a file or a directory.
newtype WorkTreeItem = WorkTreeItem FilePath

-- Many git commands seek work tree items matching some criteria,
-- and silently skip over anything that does not exist. But users expect
-- an error message when one of the files they provided as a command-line
-- parameter doesn't exist, so this checks that each exists.
workTreeItems :: CmdParams -> Annex [WorkTreeItem]
workTreeItems ps = do
	forM_ ps $ \p ->
		unlessM (isJust <$> liftIO (catchMaybeIO $ getSymbolicLinkStatus p)) $ do
			toplevelWarning False (p ++ " not found")
			Annex.incError
	return (map WorkTreeItem ps)

notSymlink :: FilePath -> IO Bool
notSymlink f = liftIO $ not . isSymbolicLink <$> getSymbolicLinkStatus f
