{- git-annex command seeking
 - 
 - These functions find appropriate files or other things based on
 - the values a user passes to a command, and prepare actions operating
 - on them.
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Seek where

import Common.Annex
import Types.Command
import Types.Key
import Backend
import qualified Annex
import qualified Git
import qualified Git.LsFiles as LsFiles
import qualified Limit

withFilesInGit :: (FilePath -> CommandStart) -> CommandSeek
withFilesInGit a params = do
	repo <- gitRepo
	prepFiltered a $ liftIO $ runPreserveOrder (LsFiles.inRepo repo) params

withAttrFilesInGit :: String -> ((FilePath, String) -> CommandStart) -> CommandSeek
withAttrFilesInGit attr a params = do
	repo <- gitRepo
	files <- liftIO $ runPreserveOrder (LsFiles.inRepo repo) params
	prepFilteredGen a fst $ liftIO $ Git.checkAttr repo attr files

withNumCopies :: (FilePath -> Maybe Int -> CommandStart) -> CommandSeek
withNumCopies a params = withAttrFilesInGit "annex.numcopies" go params
	where
		go (file, v) = a file (readMaybe v)

withBackendFilesInGit :: (BackendFile -> CommandStart) -> CommandSeek
withBackendFilesInGit a params = do
	repo <- gitRepo
	files <- liftIO $ runPreserveOrder (LsFiles.inRepo repo) params
	prepBackendPairs a files

withFilesMissing :: (String -> CommandStart) -> CommandSeek
withFilesMissing a params = prepFiltered a $ liftIO $ filterM missing params
	where
		missing = liftM not . doesFileExist

withFilesNotInGit :: (BackendFile -> CommandStart) -> CommandSeek
withFilesNotInGit a params = do
	repo <- gitRepo
	force <- Annex.getState Annex.force
	newfiles <- liftIO $ runPreserveOrder (LsFiles.notInRepo repo force) params
	prepBackendPairs a newfiles

withWords :: ([String] -> CommandStart) -> CommandSeek
withWords a params = return [a params]

withStrings :: (String -> CommandStart) -> CommandSeek
withStrings a params = return $ map a params

withFilesToBeCommitted :: (String -> CommandStart) -> CommandSeek
withFilesToBeCommitted a params = do
	repo <- gitRepo
	prepFiltered a $
		liftIO $ runPreserveOrder (LsFiles.stagedNotDeleted repo) params

withFilesUnlocked :: (BackendFile -> CommandStart) -> CommandSeek
withFilesUnlocked = withFilesUnlocked' LsFiles.typeChanged

withFilesUnlockedToBeCommitted :: (BackendFile -> CommandStart) -> CommandSeek
withFilesUnlockedToBeCommitted = withFilesUnlocked' LsFiles.typeChangedStaged

withFilesUnlocked' :: (Git.Repo -> [FilePath] -> IO [FilePath]) -> (BackendFile -> CommandStart) -> CommandSeek
withFilesUnlocked' typechanged a params = do
	-- unlocked files have changed type from a symlink to a regular file
	repo <- gitRepo
	typechangedfiles <- liftIO $ runPreserveOrder (typechanged repo) params
	unlockedfiles <- liftIO $ filterM notSymlink $
		map (\f -> Git.workTree repo ++ "/" ++ f) typechangedfiles
	prepBackendPairs a unlockedfiles

withKeys :: (Key -> CommandStart) -> CommandSeek
withKeys a params = return $ map (a . parse) params
	where
		parse p = fromMaybe (error "bad key") $ readKey p

withNothing :: CommandStart -> CommandSeek
withNothing a [] = return [a]
withNothing _ _ = error "This command takes no parameters."


prepFiltered :: (FilePath -> CommandStart) -> Annex [FilePath] -> Annex [CommandStart]
prepFiltered a = prepFilteredGen a id

prepBackendPairs :: (BackendFile -> CommandStart) -> CommandSeek
prepBackendPairs a fs = prepFilteredGen a snd (chooseBackends fs)

prepFilteredGen :: (b -> CommandStart) -> (b -> FilePath) -> Annex [b] -> Annex [CommandStart]
prepFilteredGen a d fs = do
	matcher <- Limit.getMatcher
	prepStart (proc matcher) fs
	where
		proc matcher v = do
			let f = d v
			ok <- matcher f
			if ok then a v else return Nothing

{- Generates a list of CommandStart actions that will be run to perform a
 - command, using a list (ie of files) coming from an action. The list
 - will be produced and consumed lazily. -}
prepStart :: (b -> CommandStart) -> Annex [b] -> Annex [CommandStart]
prepStart a fs = liftM (map a) fs

notSymlink :: FilePath -> IO Bool
notSymlink f = liftIO $ not . isSymbolicLink <$> getSymbolicLinkStatus f
