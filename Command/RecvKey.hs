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
import Logs.Transfer

def :: [Command]
def = [oneShot $ command "recvkey" paramKey seek
	"runs rsync in server mode to receive content"]

seek :: [CommandSeek]
seek = [withKeys start]

start :: Key -> CommandStart
start key = ifM (inAnnex key)
	( error "key is already present in annex"
	, fieldTransfer Download key $ do
		ifM (getViaTmp key $ liftIO . rsyncServerReceive)
			( do
				-- forcibly quit after receiving one key,
				-- and shutdown cleanly
				_ <- shutdown True
				liftIO exitSuccess
			, liftIO exitFailure
			)
	)
