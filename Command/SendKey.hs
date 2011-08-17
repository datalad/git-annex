{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.SendKey where

import Control.Monad.State (liftIO)
import System.Exit

import Locations
import qualified Annex
import Command
import Content
import Utility
import Utility.RsyncFile
import Messages

command :: [Command]
command = [repoCommand "sendkey" paramKey seek
	"runs rsync in server mode to send content"]

seek :: [CommandSeek]
seek = [withKeys start]

start :: CommandStartKey
start key = do
	g <- Annex.gitRepo
	let file = gitAnnexLocation g key
	whenM (inAnnex key) $
		liftIO $ rsyncServerSend file -- does not return
	warning "requested key is not present"
	liftIO exitFailure
