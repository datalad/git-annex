{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.RecvKey where

import Control.Monad.State (liftIO)
import System.Exit

import Command
import CmdLine
import Content
import Utility
import Utility.RsyncFile

command :: [Command]
command = [repoCommand "recvkey" paramKey seek
	"runs rsync in server mode to receive content"]

seek :: [CommandSeek]
seek = [withKeys start]

start :: CommandStartKey
start key = do
	whenM (inAnnex key) $ error "key is already present in annex"
	
	ok <- getViaTmp key (liftIO . rsyncServerReceive)
	if ok
		then do
			-- forcibly quit after receiving one key,
			-- and shutdown cleanly so queued git commands run
			_ <- shutdown
			liftIO exitSuccess
		else liftIO exitFailure
