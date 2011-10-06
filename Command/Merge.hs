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

command :: [Command]
command = [repoCommand "merge" paramNothing seek
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
	next $ return True
