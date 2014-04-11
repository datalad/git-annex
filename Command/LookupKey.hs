{- git-annex command
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.LookupKey where

import Common.Annex
import Command
import Annex.CatFile
import Types.Key

def :: [Command]
def = [notBareRepo $ noCommit $ noMessages $
	command "lookupkey" (paramRepeating paramFile) seek
		SectionPlumbing "looks up key used for file"]

seek :: CommandSeek
seek = withStrings start

start :: String -> CommandStart
start file = do
	liftIO . maybe exitFailure (putStrLn . key2file) =<< catKeyFile file
	stop
