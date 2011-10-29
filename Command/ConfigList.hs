{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.ConfigList where

import Common.Annex
import Command
import Annex.UUID

def :: [Command]
def = [command "configlist" paramNothing seek
	"outputs relevant git configuration"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = do
	u <- getUUID
	liftIO $ putStrLn $ "annex.uuid=" ++ u
	stop
