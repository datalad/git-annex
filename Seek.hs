{- git-annex command seeking
 - 
 - These functions find appropriate files or other things based on
 - the values a user passes to a command, and prepare actions operating
 - on them.
 -
 - Copyright 2010-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Seek where

import System.PosixCompat.Files

import Common.Annex
import Types.Command
import Types.Key
import Types.FileMatcher
import qualified Annex
import qualified Git
import qualified Git.Command
import qualified Git.LsFiles as LsFiles
import qualified Limit
import qualified Option
import Config
import Logs.Location
import Logs.Unused
import Annex.CatFile

seekHelper :: ([FilePath] -> Git.Repo -> IO ([FilePath], IO Bool)) -> [FilePath] -> Annex [FilePath]
seekHelper a params = do
	ll <- inRepo $ \g ->
		runSegmentPaths (\fs -> Git.Command.leaveZombie <$> a fs g) params
	{- Show warnings only for files/directories that do not exist. -}
	forM_ (map fst $ filter (null . snd) $ zip params ll) $ \p ->
		unlessM (isJust <$> liftIO (catchMaybeIO $ getSymbolicLinkStatus p)) $
			fileNotFound p
	return $ concat ll

withFilesInGit :: (FilePath -> CommandStart) -> CommandSeek
withFilesInGit a params = prepFiltered a $ seekHelper LsFiles.inRepo params

withFilesNotInGit :: (FilePath -> CommandStart) -> CommandSeek
withFilesNotInGit a params = do
	{- dotfiles are not acted on unless explicitly listed -}
	files <- filter (not . dotfile) <$>
		seekunless (null ps && not (null params)) ps
	dotfiles <- seekunless (null dotps) dotps
	prepFiltered a $ return $ concat $ segmentPaths params (files++dotfiles)
  where
	(dotps, ps) = partition dotfile params
	seekunless True _ = return []
	seekunless _ l = do
		force <- Annex.getState Annex.force
		g <- gitRepo
		liftIO $ Git.Command.leaveZombie <$> LsFiles.notInRepo force l g

withPathContents :: ((FilePath, FilePath) -> CommandStart) -> CommandSeek
withPathContents a params = map a . concat <$> liftIO (mapM get params)
  where
	get p = ifM (isDirectory <$> getFileStatus p)
		( map (\f -> (f, makeRelative (parentDir p) f))
			<$> dirContentsRecursiveSkipping (".git" `isSuffixOf`) True p
		, return [(p, takeFileName p)]
		)

withWords :: ([String] -> CommandStart) -> CommandSeek
withWords a params = return [a params]

withStrings :: (String -> CommandStart) -> CommandSeek
withStrings a params = return $ map a params

withPairs :: ((String, String) -> CommandStart) -> CommandSeek
withPairs a params = return $ map a $ pairs [] params
  where
	pairs c [] = reverse c
	pairs c (x:y:xs) = pairs ((x,y):c) xs
	pairs _ _ = error "expected pairs"

withFilesToBeCommitted :: (String -> CommandStart) -> CommandSeek
withFilesToBeCommitted a params = prepFiltered a $
	seekHelper LsFiles.stagedNotDeleted params

withFilesUnlocked :: (FilePath -> CommandStart) -> CommandSeek
withFilesUnlocked = withFilesUnlocked' LsFiles.typeChanged

withFilesUnlockedToBeCommitted :: (FilePath -> CommandStart) -> CommandSeek
withFilesUnlockedToBeCommitted = withFilesUnlocked' LsFiles.typeChangedStaged

{- Unlocked files have changed type from a symlink to a regular file.
 -
 - Furthermore, unlocked files used to be a git-annex symlink,
 - not some other sort of symlink.
 -}
withFilesUnlocked' :: ([FilePath] -> Git.Repo -> IO ([FilePath], IO Bool)) -> (FilePath -> CommandStart) -> CommandSeek
withFilesUnlocked' typechanged a params = prepFiltered a unlockedfiles
  where
  	check f = liftIO (notSymlink f) <&&> 
		(isJust <$> catKeyFile f <||> isJust <$> catKeyFileHEAD f)
	unlockedfiles = filterM check =<< seekHelper typechanged params

{- Finds files that may be modified. -}
withFilesMaybeModified :: (FilePath -> CommandStart) -> CommandSeek
withFilesMaybeModified a params =
	prepFiltered a $ seekHelper LsFiles.modified params

withKeys :: (Key -> CommandStart) -> CommandSeek
withKeys a params = return $ map (a . parse) params
  where
	parse p = fromMaybe (error "bad key") $ file2key p

withValue :: Annex v -> (v -> CommandSeek) -> CommandSeek
withValue v a params = do
	r <- v
	a r params

{- Modifies a seek action using the value of a field option, which is fed into
 - a conversion function, and then is passed into the seek action.
 - This ensures that the conversion function only runs once.
 -}
withField :: Option -> (Maybe String -> Annex a) -> (a -> CommandSeek) -> CommandSeek
withField option converter = withValue $
	converter <=< Annex.getField $ Option.name option

withFlag :: Option -> (Bool -> CommandSeek) -> CommandSeek
withFlag option = withValue $ Annex.getFlag (Option.name option)

withNothing :: CommandStart -> CommandSeek
withNothing a [] = return [a]
withNothing _ _ = error "This command takes no parameters."

{- If --all is specified, or in a bare repo, runs an action on all
 - known keys.
 -
 - If --unused is specified, runs an action on all keys found by
 - the last git annex unused scan.
 -
 - Otherwise, fall back to a regular CommandSeek action on
 - whatever params were passed. -}
withKeyOptions :: (Key -> CommandStart) -> CommandSeek -> CommandSeek
withKeyOptions keyop fallbackop params = do
	bare <- fromRepo Git.repoIsLocalBare
	allkeys <- Annex.getFlag "all"
	unused <- Annex.getFlag "unused"
	auto <- Annex.getState Annex.auto
	case    (allkeys || bare , unused, auto ) of
		(True    , False , False) -> go loggedKeys
		(False   , True  , False) -> go unusedKeys
		(True    , True  , _    )
			| bare && not allkeys -> go unusedKeys
			| otherwise -> error "Cannot use --all with --unused."
		(False   , False , _    ) -> fallbackop params
		(_       , _     , True )
			| bare -> error "Cannot use --auto in a bare repository."
			| otherwise -> error "Cannot use --auto with --all or --unused."
  where
  	go a = do
		unless (null params) $
			error "Cannot mix --all or --unused with file names."
		map keyop <$> a

prepFiltered :: (FilePath -> CommandStart) -> Annex [FilePath] -> Annex [CommandStart]
prepFiltered a fs = do
	matcher <- Limit.getMatcher
	map (process matcher) <$> fs
  where
	process matcher f = ifM (matcher $ MatchingFile $ FileInfo f f)
		( a f , return Nothing )

notSymlink :: FilePath -> IO Bool
notSymlink f = liftIO $ not . isSymbolicLink <$> getSymbolicLinkStatus f

whenNotDirect :: CommandSeek -> CommandSeek
whenNotDirect a params = ifM isDirect ( return [] , a params )

whenDirect :: CommandSeek -> CommandSeek
whenDirect a params = ifM isDirect ( a params, return [] )
