{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.SetPresentKey where

import Command
import Logs.Location
import Logs.Presence.Pure

cmd :: Command
cmd = noCommit $ 
	command "setpresentkey" SectionPlumbing
		"change records of where key is present"
		(paramPair paramKey (paramPair paramUUID "[1|0]"))
		(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start (ks:us:vs:[]) = do
	showStart' "setpresentkey" k Nothing
	next $ perform k (toUUID us) s
  where
	k = fromMaybe (error "bad key") (file2key ks)
	s = fromMaybe (error "bad value") (parseStatus vs)
start _ = error "Wrong number of parameters"

perform :: Key -> UUID -> LogStatus -> CommandPerform
perform k u s = next $ do
	logChange k u s
	return True
