{- git-annex command seeking
 - 
 - These functions find appropriate files or other things based on
 - the values a user passes to a command, and prepare actions operating
 - on them.
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Seek where

import Common.Annex
import Types.Command
import Types.Key
import qualified Annex
import qualified Git
import qualified Git.LsFiles as LsFiles
import qualified Limit
import qualified Option

seekHelper :: ([FilePath] -> Git.Repo -> IO [FilePath]) -> [FilePath] -> Annex [FilePath]
seekHelper a params = inRepo $ \g -> runPreserveOrder (`a` g) params

withFilesInGit :: (FilePath -> CommandStart) -> CommandSeek
withFilesInGit a params = prepFiltered a $ seekHelper LsFiles.inRepo params

withFilesNotInGit :: (FilePath -> CommandStart) -> CommandSeek
withFilesNotInGit a params = do
	{- dotfiles are not acted on unless explicitly listed -}
	files <- filter (not . dotfile) <$>
		seekunless (null ps && not (null params)) ps
	dotfiles <- seekunless (null dotps) dotps
	prepFiltered a $ return $ preserveOrder params (files++dotfiles)
	where
		(dotps, ps) = partition dotfile params
		seekunless True _ = return []
		seekunless _ l = do
			force <- Annex.getState Annex.force
			g <- gitRepo
			liftIO $ (\p -> LsFiles.notInRepo force p g) l

withPathContents :: ((FilePath, FilePath) -> CommandStart) -> CommandSeek
withPathContents a params = map a . concat <$> liftIO (mapM get params)
	where
		get p = ifM (isDirectory <$> getFileStatus p)
			( map (\f -> (f, makeRelative p f)) <$> dirContentsRecursive p
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

withFilesUnlocked' :: ([FilePath] -> Git.Repo -> IO [FilePath]) -> (FilePath -> CommandStart) -> CommandSeek
withFilesUnlocked' typechanged a params = do
	-- unlocked files have changed type from a symlink to a regular file
	typechangedfiles <- seekHelper typechanged params
	let unlockedfiles = liftIO $ filterM notSymlink typechangedfiles
	prepFiltered a unlockedfiles

withKeys :: (Key -> CommandStart) -> CommandSeek
withKeys a params = return $ map (a . parse) params
	where
		parse p = fromMaybe (error "bad key") $ readKey p

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
	converter =<< Annex.getField (Option.name option)

withFlag :: Option -> (Bool -> CommandSeek) -> CommandSeek
withFlag option = withValue $ Annex.getFlag (Option.name option)

withNothing :: CommandStart -> CommandSeek
withNothing a [] = return [a]
withNothing _ _ = error "This command takes no parameters."


prepFiltered :: (FilePath -> CommandStart) -> Annex [FilePath] -> Annex [CommandStart]
prepFiltered a fs = do
	matcher <- Limit.getMatcher
	map (proc matcher) <$> fs
	where
		proc matcher f = do
			ok <- matcher f
			if ok then a f else return Nothing

notSymlink :: FilePath -> IO Bool
notSymlink f = liftIO $ not . isSymbolicLink <$> getSymbolicLinkStatus f
