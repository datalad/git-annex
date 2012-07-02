{- git-annex command
 -
 - Copyright 2010,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.SendKey where

import Common.Annex
import Command
import Annex.Content
import Utility.RsyncFile
import Logs.Transfer

def :: [Command]
def = [oneShot $ command "sendkey" paramKey seek
	"runs rsync in server mode to send content"]

seek :: [CommandSeek]
seek = [withKeys start]

start :: Key -> CommandStart
start key = ifM (inAnnex key)
	( fieldTransfer Upload key $ do
		file <- inRepo $ gitAnnexLocation key
		liftIO $ ifM (rsyncServerSend file)
			( exitSuccess , exitFailure )
	, do
		warning "requested key is not present"
		liftIO exitFailure
	)
