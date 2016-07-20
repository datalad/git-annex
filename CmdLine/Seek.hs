{- git-annex command seeking
 - 
 - These functions find appropriate files or other things based on
 - the values a user passes to a command, and prepare actions operating
 - on them.
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
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
import Annex.CatFile
import Annex.Content

withFilesInGit :: (FilePath -> CommandStart) -> CmdParams -> CommandSeek
withFilesInGit a params = seekActions $ prepFiltered a $
	seekHelper LsFiles.inRepo params

withFilesInGitNonRecursive :: String -> (FilePath -> CommandStart) -> CmdParams -> CommandSeek
withFilesInGitNonRecursive needforce a params = ifM (Annex.getState Annex.force)
	( withFilesInGit a params
	, if null params
		then error needforce
		else seekActions $ prepFiltered a (getfiles [] params)
	)
  where
	getfiles c [] = return (reverse c)
	getfiles c (p:ps) = do
		(fs, cleanup) <- inRepo $ LsFiles.inRepo [p]
		case fs of
			[f] -> do
				void $ liftIO $ cleanup
				getfiles (f:c) ps
			[] -> do
				void $ liftIO $ cleanup
				getfiles c ps
			_ -> error needforce

withFilesNotInGit :: Bool -> (FilePath -> CommandStart) -> CmdParams -> CommandSeek
withFilesNotInGit skipdotfiles a params
	| skipdotfiles = do
		{- dotfiles are not acted on unless explicitly listed -}
		files <- filter (not . dotfile) <$>
			seekunless (null ps && not (null params)) ps
		dotfiles <- seekunless (null dotps) dotps
		go (files++dotfiles)
	| otherwise = go =<< seekunless False params
  where
	(dotps, ps) = partition dotfile params
	seekunless True _ = return []
	seekunless _ l = do
		force <- Annex.getState Annex.force
		g <- gitRepo
		liftIO $ Git.Command.leaveZombie <$> LsFiles.notInRepo force l g
	go l = seekActions $ prepFiltered a $
		return $ concat $ segmentPaths params l

withFilesInRefs :: (FilePath -> Key -> CommandStart) -> CmdParams -> CommandSeek
withFilesInRefs a = mapM_ go
  where
	go r = do	
		matcher <- Limit.getMatcher
		(l, cleanup) <- inRepo $ LsTree.lsTree (Git.Ref r)
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
	pairs _ _ = error "expected pairs"

withFilesToBeCommitted :: (FilePath -> CommandStart) -> CmdParams -> CommandSeek
withFilesToBeCommitted a params = seekActions $ prepFiltered a $
	seekHelper LsFiles.stagedNotDeleted params

withFilesOldUnlocked :: (FilePath -> CommandStart) -> CmdParams -> CommandSeek
withFilesOldUnlocked = withFilesOldUnlocked' LsFiles.typeChanged

withFilesOldUnlockedToBeCommitted :: (FilePath -> CommandStart) -> CmdParams -> CommandSeek
withFilesOldUnlockedToBeCommitted = withFilesOldUnlocked' LsFiles.typeChangedStaged

{- Unlocked files before v6 have changed type from a symlink to a regular file.
 -
 - Furthermore, unlocked files used to be a git-annex symlink,
 - not some other sort of symlink.
 -}
withFilesOldUnlocked' :: ([FilePath] -> Git.Repo -> IO ([FilePath], IO Bool)) -> (FilePath -> CommandStart) -> CmdParams -> CommandSeek
withFilesOldUnlocked' typechanged a params = seekActions $
	prepFiltered a unlockedfiles
  where
	unlockedfiles = filterM isOldUnlocked =<< seekHelper typechanged params

isOldUnlocked :: FilePath -> Annex Bool
isOldUnlocked f = liftIO (notSymlink f) <&&> 
	(isJust <$> catKeyFile f <||> isJust <$> catKeyFileHEAD f)

{- Finds files that may be modified. -}
withFilesMaybeModified :: (FilePath -> CommandStart) -> CmdParams -> CommandSeek
withFilesMaybeModified a params = seekActions $
	prepFiltered a $ seekHelper LsFiles.modified params

withKeys :: (Key -> CommandStart) -> CmdParams -> CommandSeek
withKeys a params = seekActions $ return $ map (a . parse) params
  where
	parse p = fromMaybe (error "bad key") $ file2key p

withNothing :: CommandStart -> CmdParams -> CommandSeek
withNothing a [] = seekActions $ return [a]
withNothing _ _ = error "This command takes no parameters."

{- Handles the --all, --branch, --unused, --key, and --incomplete options,
 - which specify particular keys to run an action on.
 -
 - In a bare repo, --all is the default.
 -
 - Otherwise falls back to a regular CommandSeek action on
 - whatever params were passed. -}
withKeyOptions :: Maybe KeyOptions -> Bool -> (Key -> CommandStart) -> (CmdParams -> CommandSeek) -> CmdParams -> CommandSeek
withKeyOptions ko auto keyaction = withKeyOptions' ko auto mkkeyaction
  where
	mkkeyaction = do
		matcher <- Limit.getMatcher
		return $ \getkeys ->
			seekActions $ map (process matcher) <$> getkeys
	process matcher k = ifM (matcher $ MatchingKey k)
		( keyaction k
		, return Nothing
		)

withKeyOptions' :: Maybe KeyOptions -> Bool -> Annex (Annex [Key] -> Annex ()) -> (CmdParams -> CommandSeek) -> CmdParams -> CommandSeek
withKeyOptions' ko auto mkkeyaction fallbackaction params = do
	bare <- fromRepo Git.repoIsLocalBare
	when (auto && bare) $
		error "Cannot use --auto in a bare repository"
	case (null params, ko) of
		(True, Nothing)
			| bare -> noauto $ runkeyaction loggedKeys
			| otherwise -> fallbackaction params
		(False, Nothing) -> fallbackaction params
		(True, Just WantAllKeys) -> noauto $ runkeyaction loggedKeys
		(True, Just WantUnusedKeys) -> noauto $ runkeyaction unusedKeys'
		(True, Just (WantSpecificKey k)) -> noauto $ runkeyaction (return [k])
		(True, Just WantIncompleteKeys) -> noauto $ runkeyaction incompletekeys
		(True, Just (WantBranchKeys bs)) -> noauto $ runbranchkeys bs
		(False, Just _) -> error "Can only specify one of file names, --all, --branch, --unused, --key, or --incomplete"
  where
	noauto a
		| auto = error "Cannot use --auto with --all or --branch or --unused or --key or --incomplete"
		| otherwise = a
	incompletekeys = staleKeysPrune gitAnnexTmpObjectDir True
	runkeyaction ks = do
		keyaction <- mkkeyaction
		keyaction ks
	runbranchkeys bs = do
		keyaction <- mkkeyaction
		forM_ bs $ \b -> do
			(l, cleanup) <- inRepo $ LsTree.lsTree b
			forM_ l $ \i ->
				maybe noop (\k -> keyaction (return [k]))
					=<< catKey (LsTree.sha i)
			liftIO $ void cleanup

prepFiltered :: (FilePath -> CommandStart) -> Annex [FilePath] -> Annex [CommandStart]
prepFiltered a fs = do
	matcher <- Limit.getMatcher
	map (process matcher) <$> fs
  where
	process matcher f = ifM (matcher $ MatchingFile $ FileInfo f f)
		( a f , return Nothing )

seekActions :: Annex [CommandStart] -> Annex ()
seekActions gen = mapM_ commandAction =<< gen

seekHelper :: ([FilePath] -> Git.Repo -> IO ([FilePath], IO Bool)) -> [FilePath] -> Annex [FilePath]
seekHelper a params = do
	ll <- inRepo $ \g -> concat <$> forM (segmentXargsOrdered params)
		(runSegmentPaths (\fs -> Git.Command.leaveZombie <$> a fs g))
	forM_ (map fst $ filter (null . snd) $ zip params ll) $ \p ->
		unlessM (isJust <$> liftIO (catchMaybeIO $ getSymbolicLinkStatus p)) $ do
			toplevelWarning False (p ++ " not found")
			Annex.incError
	return $ concat ll

notSymlink :: FilePath -> IO Bool
notSymlink f = liftIO $ not . isSymbolicLink <$> getSymbolicLinkStatus f
