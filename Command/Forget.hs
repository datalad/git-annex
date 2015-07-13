{- git-annex command
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
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

cmd :: Command
cmd = command "forget" SectionMaintenance 
	"prune git-annex branch history"
	paramNothing (seek <$$> optParser)

data ForgetOptions = ForgetOptions
	{ dropDead :: Bool
	}

optParser :: CmdParamsDesc -> Parser ForgetOptions
optParser _ = ForgetOptions
	<$> switch
		( long "drop-dead"
		<> help "drop references to dead repositories"
		)

seek :: ForgetOptions -> CommandSeek
seek = commandAction . start

start :: ForgetOptions -> CommandStart
start o = do
	showStart "forget" "git-annex"
	now <- liftIO getPOSIXTime
	let basets = addTransition now ForgetGitHistory noTransitions
	let ts = if dropDead o
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
