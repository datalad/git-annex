{- git-annex command
 -
 - Copyright 2010, 2015 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.SetKey where

import Command
import Logs.Location
import Annex.Content

cmd :: Command
cmd = command "setkey" SectionPlumbing "sets annexed content for a key"
	(paramPair paramKey paramPath)
	(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start ps@(keyname:file:[]) = starting "setkey" ai si $
	perform file (keyOpt keyname)
  where
	ai = ActionItemOther (Just file)
	si = SeekInput ps
start _ = giveup "specify a key and a content file"

keyOpt :: String -> Key
keyOpt = fromMaybe (giveup "bad key") . deserializeKey

perform :: FilePath -> Key -> CommandPerform
perform file key = do
	-- the file might be on a different filesystem, so moveFile is used
	-- rather than simply calling moveAnnex; disk space is also
	-- checked this way.
	ok <- getViaTmp RetrievalAllKeysSecure DefaultVerify key $ \dest -> unVerified $
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
