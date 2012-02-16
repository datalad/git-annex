{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.ReKey where

import Common.Annex
import Command
import qualified Annex
import Types.Key
import Annex.Content
import qualified Command.Migrate

def :: [Command]
def = [command "rekey"
	(paramOptional $ paramRepeating $ paramPair paramPath paramKey)
	seek "change keys used for files"]

seek :: [CommandSeek]
seek = [withPairs start]

start :: (FilePath, String) -> CommandStart
start (file, keyname) = ifAnnexed file go stop
	where
		newkey = fromMaybe (error "bad key") $ readKey keyname
		go (oldkey, _)
			| oldkey == newkey = stop
			| otherwise = do
				showStart "rekey" file
				next $ perform file oldkey newkey

perform :: FilePath -> Key -> Key -> CommandPerform
perform file oldkey newkey = do
	present <- inAnnex oldkey
	_ <- if present
		then do
			src <- inRepo $ gitAnnexLocation oldkey
			Command.Migrate.linkKey src newkey
		else do
			unlessM (Annex.getState Annex.force) $
				error $ file ++ " is not available (use --force to override)"
			return True
	next $ Command.Migrate.cleanup file oldkey newkey
