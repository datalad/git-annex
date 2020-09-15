{- git-annex command
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Commit where

import Command
import qualified Annex.Branch
import qualified Git
import Git.Types

cmd :: Command
cmd = command "commit" SectionPlumbing 
	"commits any staged changes to the git-annex branch"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing (commandAction start)

start :: CommandStart
start = starting "commit" ai si $ do
	Annex.Branch.commit =<< Annex.Branch.commitMessage
	_ <- runhook <=< inRepo $ Git.hookPath "annex-content"
	next $ return True
  where
	runhook (Just hook) = liftIO $ boolSystem hook []
	runhook Nothing = return True
	ai = ActionItemOther (Just (fromRef Annex.Branch.name))
	si = SeekInput []
