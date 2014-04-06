{- git-annex command
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.RemoteDaemon where

import Common.Annex
import Command
import RemoteDaemon.Core

def :: [Command]
def = [noCommit $ command "remotedaemon" paramNothing seek SectionPlumbing
	"detects when remotes have changed, and fetches from them"]

seek :: CommandSeek
seek = withNothing start

start :: CommandStart
start = do
	liftIO runForeground
	stop
