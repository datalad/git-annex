{- git-annex command
 -
 - Copyright 2010, 2015 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.SetKey where

import Common.Annex
import Command
import Logs.Location
import Annex.Content
import Types.Key

cmd :: Command
cmd = command "setkey" SectionPlumbing "sets annexed content for a key"
	(paramPair paramKey paramPath)
	(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start (keyname:file:[]) = do
	showStart "setkey" file
	next $ perform file (mkKey keyname)
start _ = error "specify a key and a content file"

mkKey :: String -> Key
mkKey = fromMaybe (error "bad key") . file2key

perform :: FilePath -> Key -> CommandPerform
perform file key = do
	-- the file might be on a different filesystem, so moveFile is used
	-- rather than simply calling moveAnnex; disk space is also
	-- checked this way.
	ok <- getViaTmp DefaultVerify key $ \dest ->
		if dest /= file
			then liftIO $ catchBoolIO $ do
				moveFile file dest
				return True
		else return True
	if ok
		then next $ cleanup key
		else error "mv failed!"

cleanup :: Key -> CommandCleanup
cleanup key = do
	logStatus key InfoPresent
	return True
