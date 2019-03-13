{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.ReadPresentKey where

import Command
import Logs.Location

cmd :: Command
cmd = noCommit $ 
	command "readpresentkey" SectionPlumbing
		"read records of where key is present"
		(paramPair paramKey paramUUID)
		(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start (ks:us:[]) = do
	ls <- loggedLocations k
	if toUUID us `elem` ls
		then liftIO exitSuccess
		else liftIO exitFailure
  where
	k = fromMaybe (giveup "bad key") (deserializeKey ks)
start _ = giveup "Wrong number of parameters"
