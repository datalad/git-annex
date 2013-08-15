{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Lock where

import Common.Annex
import Command
import qualified Annex.Queue
	
def :: [Command]
def = [notDirect $ command "lock" paramPaths seek SectionCommon
	"undo unlock command"]

seek :: [CommandSeek]
seek = [withFilesUnlocked start, withFilesUnlockedToBeCommitted start]

start :: FilePath -> CommandStart
start file = do
	showStart "lock" file
	next $ perform file

perform :: FilePath -> CommandPerform
perform file = do
	Annex.Queue.addCommand "checkout" [Param "--"] [file]
	next $ return True -- no cleanup needed
