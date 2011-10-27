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

command :: [Command]
command = [Command "inannex" (paramRepeating paramKey) defaultChecks seek
		"checks if keys are present in the annex"]

seek :: [CommandSeek]
seek = [withKeys start]

start :: Key -> CommandStart
start key = do
	present <- inAnnex key
	if present
		then stop
		else liftIO exitFailure
