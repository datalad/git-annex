{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.InAnnex where

import Common.Annex
import Command
import Annex.Content

def :: [Command]
def = [noCommit $ command "inannex" (paramRepeating paramKey) seek
	SectionPlumbing "checks if keys are present in the annex"]

seek :: [CommandSeek]
seek = [withKeys start]

start :: Key -> CommandStart
start key = inAnnexSafe key >>= dispatch
  where
	dispatch (Just True) = stop
	dispatch (Just False) = exit 1
	dispatch Nothing = exit 100
	exit n = liftIO $ exitWith $ ExitFailure n
