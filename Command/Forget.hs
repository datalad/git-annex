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

import Data.Time.Clock.POSIX

def :: [Command]
def = [command "forget" paramNothing seek
		SectionMaintenance "prune git-annex branch history"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = do
	showStart "forget" "git-annex"
	next $ perform =<< Annex.getState Annex.force

perform :: Bool -> CommandPerform
perform True = do
	now <- liftIO getPOSIXTime
	let ts = addTransition now ForgetGitHistory noTransitions
	recordTransitions Branch.change ts
	-- get branch committed before contining with the transition
	Branch.update
	void $ Branch.performTransitions ts True
	next $ return True
perform False = do
	showLongNote "To forget git-annex branch history, you must specify --force. This deletes metadata!"
	stop
