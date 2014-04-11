{- git-annex command
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.RmUrl where

import Common.Annex
import Command
import Logs.Web

def :: [Command]
def = [notBareRepo $
	command "rmurl" (paramPair paramFile paramUrl) seek
		SectionCommon "record file is not available at url"]

seek :: CommandSeek
seek = withPairs start

start :: (FilePath, String) -> CommandStart
start (file, url) = flip whenAnnexed file $ \_ (key, _) -> do
	showStart "rmurl" file
	next $ next $ cleanup url key

cleanup :: String -> Key -> CommandCleanup
cleanup url key = do
	setUrlMissing key url
	return True
