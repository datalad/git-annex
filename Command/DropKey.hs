{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.DropKey where

import Common.Annex
import Command
import qualified Annex
import Logs.Location
import Annex.Content

cmd :: Command
cmd = noCommit $ 
	command "dropkey" SectionPlumbing
		"drops annexed content for specified keys"
		(paramRepeating paramKey)
		(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withKeys start

start :: Key -> CommandStart
start key = stopUnless (inAnnex key) $ do
	unlessM (Annex.getState Annex.force) $
		error "dropkey can cause data loss; use --force if you're sure you want to do this"
	showStart' "dropkey" key Nothing
	next $ perform key

perform :: Key -> CommandPerform
perform key = lockContent key $ \contentlock -> do
	removeAnnex contentlock
	next $ cleanup key

cleanup :: Key -> CommandCleanup
cleanup key = do
	logStatus key InfoMissing
	return True
