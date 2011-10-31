{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.DropKey where

import Common.Annex
import Command
import qualified Annex
import Logs.Location
import Annex.Content

def :: [Command]
def = [command "dropkey" (paramRepeating paramKey) seek
	"drops annexed content for specified keys"] 

seek :: [CommandSeek]
seek = [withKeys start]

start :: Key -> CommandStart
start key = do
	present <- inAnnex key
	if not present
		then stop
		else do
			checkforced
			showStart "dropkey" (show key)
			next $ perform key
	where
		checkforced = 
			unlessM (Annex.getState Annex.force) $
				error "dropkey can cause data loss; use --force if you're sure you want to do this"

perform :: Key -> CommandPerform
perform key = do
	removeAnnex key
	next $ cleanup key

cleanup :: Key -> CommandCleanup
cleanup key = do
	logStatus key InfoMissing
	return True
