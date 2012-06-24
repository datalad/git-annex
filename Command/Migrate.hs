{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Migrate where

import Common.Annex
import Command
import Backend
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
	newbackend <- choosebackend =<< chooseBackend file
	if (newbackend /= oldbackend || upgradableKey key) && exists
		then do
			showStart "migrate" file
			next $ perform file key newbackend
		else stop
	where
		choosebackend Nothing = Prelude.head <$> orderedList
		choosebackend (Just backend) = return backend

{- Checks if a key is upgradable to a newer representation. -}
{- Ideally, all keys have file size metadata. Old keys may not. -}
upgradableKey :: Key -> Bool
upgradableKey key = isNothing $ Types.Key.keySize key

{- Store the old backend's key in the new backend
 - The old backend's key is not dropped from it, because there may
 - be other files still pointing at that key. -}
perform :: FilePath -> Key -> Backend -> CommandPerform
perform file oldkey newbackend = maybe stop go =<< genkey
	where
		go newkey = stopUnless (Command.ReKey.linkKey oldkey newkey) $
			next $ Command.ReKey.cleanup file oldkey newkey
		genkey = do
			content <- inRepo $ gitAnnexLocation oldkey
			let source = KeySource { keyFilename = file, contentLocation = content }
			liftM fst <$> genKey source (Just newbackend)
