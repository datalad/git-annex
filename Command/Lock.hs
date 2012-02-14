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
def = [command "lock" paramPaths seek "undo unlock command"]

seek :: [CommandSeek]
seek = [withFilesUnlocked start, withFilesUnlockedToBeCommitted start]

start :: FilePath -> CommandStart
start file = do
	showStart "lock" file
	next $ perform file

perform :: FilePath -> CommandPerform
perform file = do
	liftIO $ removeFile file
	-- Checkout from HEAD to get rid of any changes that might be 
	-- staged in the index, and get back to the previous symlink to
	-- the content.
	Annex.Queue.add "checkout" [Param "HEAD", Param "--"] [file]
	next $ return True -- no cleanup needed
