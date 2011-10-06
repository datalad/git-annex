{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.ConfigList where

import Common.Annex
import Command
import UUID

command :: [Command]
command = [repoCommand "configlist" paramNothing seek
		"outputs relevant git configuration"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = do
	g <- gitRepo
	u <- getUUID g
	liftIO $ putStrLn $ "annex.uuid=" ++ u
	stop
