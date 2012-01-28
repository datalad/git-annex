{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.RecvKey where

import Common.Annex
import Command
import CmdLine
import Annex.Content
import Utility.RsyncFile

def :: [Command]
def = [command "recvkey" paramKey seek
	"runs rsync in server mode to receive content"]

seek :: [CommandSeek]
seek = [withKeys start]

start :: Key -> CommandStart
start key = do
	whenM (inAnnex key) $ error "key is already present in annex"
	
	ok <- getViaTmp key (liftIO . rsyncServerReceive)
	if ok
		then do
			-- forcibly quit after receiving one key,
			-- and shutdown cleanly
			_ <- shutdown True
			liftIO exitSuccess
		else liftIO exitFailure
