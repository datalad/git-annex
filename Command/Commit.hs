{- git-annex command
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Commit where

import Common.Annex
import Command
import qualified Annex.Branch
import qualified Git

cmd :: Command
cmd = command "commit" SectionPlumbing 
	"commits any staged changes to the git-annex branch"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing start

start :: CommandStart
start = next $ next $ do
	Annex.Branch.commit "update"
	_ <- runhook <=< inRepo $ Git.hookPath "annex-content"
	return True
  where
	runhook (Just hook) = liftIO $ boolSystem hook []
	runhook Nothing = return True
