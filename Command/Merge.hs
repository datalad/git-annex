{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Merge where

import Command
import qualified Branch
import Messages

command :: [Command]
command = [repoCommand "merge" paramNothing seek
		"auto-merges remote changes into the git-annex branch"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStartNothing
start = do
	showStart "merge" "."
	next perform

perform :: CommandPerform
perform = do
	Branch.update
	next $ return True
