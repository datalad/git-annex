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
import qualified Types.Backend
import Types.KeySource
import Annex.Content
import qualified Command.ReKey
import qualified Command.Fsck

def :: [Command]
def = [notDirect $ 
	command "migrate" paramPaths seek
		SectionUtility "switch data to different backend"]

seek :: [CommandSeek]
seek = [withFilesInGit $ whenAnnexed start]

start :: FilePath -> (Key, Backend) -> CommandStart
start file (key, oldbackend) = do
	exists <- inAnnex key
	newbackend <- choosebackend =<< chooseBackend file
	if (newbackend /= oldbackend || upgradableKey oldbackend key) && exists
		then do
			showStart "migrate" file
			next $ perform file key oldbackend newbackend
		else stop
  where
	choosebackend Nothing = Prelude.head <$> orderedList
	choosebackend (Just backend) = return backend

{- Checks if a key is upgradable to a newer representation.
 - 
 - Reasons for migration:
 -  - Ideally, all keys have file size metadata. Old keys may not.
 -  - Something has changed in the backend, such as a bug fix.
 -}
upgradableKey :: Backend -> Key -> Bool
upgradableKey backend key = isNothing (Types.Key.keySize key) || backendupgradable
  where
	backendupgradable = maybe False (\a -> a key)
		(Types.Backend.canUpgradeKey backend)

{- Store the old backend's key in the new backend
 - The old backend's key is not dropped from it, because there may
 - be other files still pointing at that key.
 -
 - To ensure that the data we have for the old key is valid, it's
 - fscked here. First we generate the new key. This ensures that the
 - data cannot get corrupted after the fsck but before the new key is
 - generated.
 -}
perform :: FilePath -> Key -> Backend -> Backend -> CommandPerform
perform file oldkey oldbackend newbackend = go =<< genkey
  where
  	go Nothing = stop
	go (Just newkey) = stopUnless checkcontent $ finish newkey
	checkcontent = Command.Fsck.checkBackend oldbackend oldkey $ Just file
	finish newkey = stopUnless (Command.ReKey.linkKey oldkey newkey) $
		next $ Command.ReKey.cleanup file oldkey newkey
	genkey = do
		content <- calcRepo $ gitAnnexLocation oldkey
		let source = KeySource
			{ keyFilename = file
			, contentLocation = content
			, inodeCache = Nothing
			}
		liftM fst <$> genKey source (Just newbackend)
