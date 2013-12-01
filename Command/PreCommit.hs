{- git-annex command
 -
 - Copyright 2010, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.PreCommit where

import Common.Annex
import Command
import qualified Command.Add
import qualified Command.Fix
import Annex.Direct

def :: [Command]
def = [command "pre-commit" paramPaths seek SectionPlumbing
	"run by git pre-commit hook"]

seek :: [CommandSeek]
seek =
	-- fix symlinks to files being committed
	[ whenNotDirect $ withFilesToBeCommitted $ whenAnnexed Command.Fix.start
	-- inject unlocked files into the annex
	, whenNotDirect $ withFilesUnlockedToBeCommitted startIndirect
	-- update direct mode mappings for committed files
	, whenDirect $ withWords startDirect
	]

startIndirect :: FilePath -> CommandStart
startIndirect file = next $ do
	unlessM (doCommand $ Command.Add.start file) $
		error $ "failed to add " ++ file ++ "; canceling commit"
	next $ return True

startDirect :: [String] -> CommandStart
startDirect _ = next $ next $ preCommitDirect
