{- git-annex command
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Forget where

import Common.Annex
import Command
import qualified Annex.Branch as Branch
import Logs.Transitions
import qualified Annex
import qualified Option

import Data.Time.Clock.POSIX

def :: [Command]
def = [withOptions forgetOptions $ command "forget" paramNothing seek
		SectionMaintenance "prune git-annex branch history"]

forgetOptions :: [Option]
forgetOptions = [dropDeadOption]

dropDeadOption :: Option
dropDeadOption = Option.flag [] "drop-dead" "drop references to dead repositories"

seek :: [CommandSeek]
seek = [withFlag dropDeadOption $ \dropdead ->
	withNothing $ start dropdead]

start :: Bool -> CommandStart
start dropdead = do
	showStart "forget" "git-annex"
	now <- liftIO getPOSIXTime
	let basets = addTransition now ForgetGitHistory noTransitions
	let ts = if dropdead
		then addTransition now ForgetDeadRemotes basets
		else basets
	next $ perform ts =<< Annex.getState Annex.force

perform :: Transitions -> Bool -> CommandPerform
perform ts True = do
	recordTransitions Branch.change ts
	-- get branch committed before contining with the transition
	Branch.update
	void $ Branch.performTransitions ts True []
	next $ return True
perform _ False = do
	showLongNote "To forget git-annex branch history, you must specify --force. This deletes metadata!"
	stop
