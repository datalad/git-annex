{- git-annex command
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.SetPresentKey where

import Common.Annex
import Command
import qualified Annex
import Logs.Location
import Logs.Presence.Pure
import Types.Key

cmd :: [Command]
cmd = [noCommit $ command "setpresentkey" (paramPair paramKey "[1|0]") seek
	SectionPlumbing "change records of where key is present"] 

seek :: CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start (ks:us:vs:[]) = do
	showStart' "setpresentkey" k Nothing
	next $ perform k (toUUID us) status
  where
	k = fromMaybe (error "bad key") (file2key ks)
	status = fromMaybe (error "bad value") (parseStatus vs)
start _ = error "Wrong number of parameters"

perform :: Key -> UUID -> LogStatus -> CommandPerform
perform k u status = next $ do
	logChange k u status
	return True
