{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Merge where

import Common.Annex
import Command
import qualified Annex.Branch

def :: [Command]
def = [command "merge" paramNothing seek
		"auto-merge remote changes into git-annex branch"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = do
	showStart "merge" "."
	next perform

perform :: CommandPerform
perform = do
	Annex.Branch.update
	-- commit explicitly, in case no remote branches were merged
	Annex.Branch.commit "update"
	next $ return True
