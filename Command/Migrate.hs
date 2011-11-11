{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Migrate where

import Common.Annex
import Command
import qualified Backend
import qualified Types.Key
import Annex.Content
import qualified Command.Add
import Logs.Web

def :: [Command]
def = [command "migrate" paramPaths seek "switch data to different backend"]

seek :: [CommandSeek]
seek = [withBackendFilesInGit $ \(b, f) -> whenAnnexed (start b) f]

start :: Maybe (Backend Annex) -> FilePath -> (Key, Backend Annex) -> CommandStart
start b file (key, oldbackend) = do
	exists <- inAnnex key
	newbackend <- choosebackend b
	if (newbackend /= oldbackend || upgradableKey key) && exists
		then do
			showStart "migrate" file
			next $ perform file key newbackend
		else stop
	where
		choosebackend Nothing = head <$> Backend.orderedList
		choosebackend (Just backend) = return backend

{- Checks if a key is upgradable to a newer representation. -}
{- Ideally, all keys have file size metadata. Old keys may not. -}
upgradableKey :: Key -> Bool
upgradableKey key = isNothing $ Types.Key.keySize key

perform :: FilePath -> Key -> Backend Annex -> CommandPerform
perform file oldkey newbackend = do
	-- Store the old backend's cached key in the new backend
	-- (the file can't be stored as usual, because it's already a symlink).
	-- The old backend's key is not dropped from it, because there may
	-- be other files still pointing at that key.
	src <- fromRepo $ gitAnnexLocation oldkey
	tmp <- fromRepo gitAnnexTmpDir
	let tmpfile = tmp </> takeFileName file
	liftIO $ createLink src tmpfile
	k <- Backend.genKey tmpfile $ Just newbackend
	liftIO $ cleantmp tmpfile
	case k of
		Nothing -> stop
		Just (newkey, _) -> do
			ok <- link src newkey
			if ok
				then do
					-- Update symlink to use the new key.
					liftIO $ removeFile file

					-- If the old key had some
					-- associated urls, record them for
					-- the new key as well.
					urls <- getUrls oldkey
					unless (null urls) $
						mapM_ (setUrlPresent newkey) urls

					next $ Command.Add.cleanup file newkey True
				else stop
	where
		cleantmp t = whenM (doesFileExist t) $ removeFile t
		link src newkey = getViaTmpUnchecked newkey $ \t -> do
			-- Make a hard link to the old backend's
			-- cached key, to avoid wasting disk space.
			liftIO $ unlessM (doesFileExist t) $ createLink src t
			return True	
