{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Migrate where

import Control.Monad.State (liftIO)
import System.Posix.Files
import System.Directory
import System.FilePath

import Command
import qualified Annex
import qualified Backend
import qualified Types.Key
import Locations
import Types
import Content
import Messages
import Utility
import qualified Command.Add

command :: [Command]
command = [repoCommand "migrate" paramPath seek "switch data to different backend"]

seek :: [CommandSeek]
seek = [withBackendFilesInGit start]

start :: CommandStartBackendFile
start (file, b) = isAnnexed file $ \(key, oldbackend) -> do
	exists <- inAnnex key
	newbackend <- choosebackend b
	if (newbackend /= oldbackend || upgradableKey key) && exists
		then do
			showStart "migrate" file
			next $ perform file key newbackend
		else stop
	where
		choosebackend Nothing = return . head =<< Backend.orderedList
		choosebackend (Just backend) = return backend

{- Checks if a key is upgradable to a newer representation. -}
{- Ideally, all keys have file size metadata. Old keys may not. -}
upgradableKey :: Key -> Bool
upgradableKey key = Types.Key.keySize key == Nothing

perform :: FilePath -> Key -> Backend Annex -> CommandPerform
perform file oldkey newbackend = do
	g <- Annex.gitRepo

	-- Store the old backend's cached key in the new backend
	-- (the file can't be stored as usual, because it's already a symlink).
	-- The old backend's key is not dropped from it, because there may
	-- be other files still pointing at that key.
	let src = gitAnnexLocation g oldkey
	let tmpfile = gitAnnexTmpDir g </> takeFileName file
	liftIO $ createLink src tmpfile
	k <- Backend.genKey tmpfile $ Just newbackend
	liftIO $ cleantmp tmpfile
	case k of
		Nothing -> stop
		Just (newkey, _) -> do
			ok <- getViaTmpUnchecked newkey $ \t -> do
				-- Make a hard link to the old backend's
				-- cached key, to avoid wasting disk space.
				liftIO $ unlessM (doesFileExist t) $ createLink src t
				return True
			if ok
				then do
					-- Update symlink to use the new key.
					liftIO $ removeFile file
					next $ Command.Add.cleanup file newkey
				else stop
	where
		cleantmp t = whenM (doesFileExist t) $ removeFile t
