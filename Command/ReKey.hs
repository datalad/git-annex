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
import qualified Command.Add
import Logs.Web

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
		then linkKey oldkey newkey
		else do
			unlessM (Annex.getState Annex.force) $
				error $ file ++ " is not available (use --force to override)"
			return True
	next $ cleanup file oldkey newkey

{- Make a hard link to the old key content, to avoid wasting disk space. -}
linkKey :: Key -> Key -> Annex Bool
linkKey oldkey newkey = getViaTmpUnchecked newkey $ \tmp -> do
	src <- inRepo $ gitAnnexLocation oldkey
	liftIO $ unlessM (doesFileExist tmp) $ createLink src tmp
	return True

cleanup :: FilePath -> Key -> Key -> CommandCleanup
cleanup file oldkey newkey = do
	-- If the old key had some associated urls, record them for
	-- the new key as well.
	urls <- getUrls oldkey
	unless (null urls) $
		mapM_ (setUrlPresent newkey) urls

	-- Update symlink to use the new key.
	liftIO $ removeFile file
	Command.Add.cleanup file newkey True
