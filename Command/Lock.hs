{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Lock where

import Common.Annex
import Command
import qualified Annex.Queue
import qualified Annex
	
cmd :: Command
cmd = notDirect $ withGlobalOptions annexedMatchingOptions $
	command "lock" SectionCommon
		"undo unlock command"
		paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek ps = do
	withFilesUnlocked start ps
	withFilesUnlockedToBeCommitted start ps

start :: FilePath -> CommandStart
start file = do
	showStart "lock" file
	unlessM (Annex.getState Annex.force) $
		error "Locking this file would discard any changes you have made to it. Use 'git annex add' to stage your changes. (Or, use --force to override)"
	next $ perform file

perform :: FilePath -> CommandPerform
perform file = do
	Annex.Queue.addCommand "checkout" [Param "--"] [file]
	next $ return True -- no cleanup needed
