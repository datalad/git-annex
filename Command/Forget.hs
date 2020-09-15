{- git-annex command
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Forget where

import Command
import qualified Annex.Branch as Branch
import Logs.Transitions
import qualified Annex
import Annex.VectorClock
import Git.Types

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
start o = starting "forget" ai si $ do
	c <- liftIO currentVectorClock
	let basets = addTransition c ForgetGitHistory noTransitions
	let ts = if dropDead o
		then addTransition c ForgetDeadRemotes basets
		else basets
	perform ts =<< Annex.getState Annex.force
  where
	ai = ActionItemOther (Just (fromRef Branch.name))
	si = SeekInput []

perform :: Transitions -> Bool -> CommandPerform
perform ts True = do
	recordTransitions Branch.change ts
	-- get branch committed before contining with the transition
	_ <- Branch.update
	void $ Branch.performTransitions ts True []
	next $ return True
perform _ False = do
	showLongNote "To forget git-annex branch history, you must specify --force. This deletes metadata!"
	stop
