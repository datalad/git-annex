{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.DropKey where

import Command
import qualified Annex
import LocationLog
import Types
import Content
import Messages

command :: [Command]
command = [repoCommand "dropkey" (paramRepeating paramKey) seek
	"drops annexed content for specified keys"] 

seek :: [CommandSeek]
seek = [withKeys start]

start :: CommandStartKey
start key = do
	present <- inAnnex key
	force <- Annex.getState Annex.force
	if not present
		then stop
		else if not force
			then error "dropkey is can cause data loss; use --force if you're sure you want to do this"
			else do
				showStart "dropkey" (show key)
				next $ perform key

perform :: Key -> CommandPerform
perform key = do
	removeAnnex key
	next $ cleanup key

cleanup :: Key -> CommandCleanup
cleanup key = do
	logStatus key InfoMissing
	return True
