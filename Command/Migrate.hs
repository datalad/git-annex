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
import qualified Command.ReKey

def :: [Command]
def = [command "migrate" paramPaths seek "switch data to different backend"]

seek :: [CommandSeek]
seek = [withFilesInGit $ whenAnnexed start]

start :: FilePath -> (Key, Backend) -> CommandStart
start file (key, oldbackend) = do
	exists <- inAnnex key
	newbackend <- choosebackend =<< Backend.chooseBackend file
	if (newbackend /= oldbackend || upgradableKey key) && exists
		then do
			showStart "migrate" file
			next $ perform file key newbackend
		else stop
	where
		choosebackend Nothing = Prelude.head <$> Backend.orderedList
		choosebackend (Just backend) = return backend

{- Checks if a key is upgradable to a newer representation. -}
{- Ideally, all keys have file size metadata. Old keys may not. -}
upgradableKey :: Key -> Bool
upgradableKey key = isNothing $ Types.Key.keySize key

{- Store the old backend's key in the new backend
 - The old backend's key is not dropped from it, because there may
 - be other files still pointing at that key.
 -
 - Use the same filename as the file for the temp file name, to support
 - backends that allow the filename to influence the keys they
 - generate.
 -}
perform :: FilePath -> Key -> Backend -> CommandPerform
perform file oldkey newbackend = do
	src <- inRepo $ gitAnnexLocation oldkey
	tmp <- fromRepo gitAnnexTmpDir
	let tmpfile = tmp </> takeFileName file
	cleantmp tmpfile
	liftIO $ createLink src tmpfile
	k <- Backend.genKey tmpfile $ Just newbackend
	cleantmp tmpfile
	case k of
		Nothing -> stop
		Just (newkey, _) ->
			stopUnless (Command.ReKey.linkKey oldkey newkey) $
				next $ Command.ReKey.cleanup file oldkey newkey
	where
		cleantmp t = liftIO $ whenM (doesFileExist t) $ removeFile t
