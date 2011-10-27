{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.PreCommit where

import Command
import qualified Command.Add
import qualified Command.Fix
import Backend

command :: [Command]
command = [Command "pre-commit" paramPaths defaultChecks seek
	"run by git pre-commit hook"]

{- The pre-commit hook needs to fix symlinks to all files being committed.
 - And, it needs to inject unlocked files into the annex. -}
seek :: [CommandSeek]
seek = [withFilesToBeCommitted Command.Fix.start,
	withFilesUnlockedToBeCommitted start]

start :: BackendFile -> CommandStart
start p = next $ perform p

perform :: BackendFile -> CommandPerform
perform pair@(_, file) = do
	ok <- doCommand $ Command.Add.start pair
	if ok
		then next $ return True
		else error $ "failed to add " ++ file ++ "; canceling commit"
