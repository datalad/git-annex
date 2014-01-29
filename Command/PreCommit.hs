{- git-annex command
 -
 - Copyright 2010, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.PreCommit where

import Common.Annex
import Command
import Config
import qualified Command.Add
import qualified Command.Fix
import Annex.Direct

def :: [Command]
def = [command "pre-commit" paramPaths seek SectionPlumbing
	"run by git pre-commit hook"]

seek :: CommandSeek
seek ps = ifM isDirect
	-- update direct mode mappings for committed files
	( withWords startDirect ps
	, do
		-- fix symlinks to files being committed
		withFilesToBeCommitted (whenAnnexed Command.Fix.start) ps
		-- inject unlocked files into the annex
		withFilesUnlockedToBeCommitted startIndirect ps
	)

startIndirect :: FilePath -> CommandStart
startIndirect file = next $ do
	unlessM (callCommandAction $ Command.Add.start file) $
		error $ "failed to add " ++ file ++ "; canceling commit"
	next $ return True

startDirect :: [String] -> CommandStart
startDirect _ = next $ next $ preCommitDirect
