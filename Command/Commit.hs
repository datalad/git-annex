{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Commit where

import Command
import qualified Annex.Branch

def :: [Command]
def = [command "commit" paramNothing seek
	"commits any staged changes to the git-annex branch"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = next $ next $ do
	Annex.Branch.commit "update"
	return True
