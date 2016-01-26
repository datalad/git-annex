{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.RemoteDaemon where

import Command
import RemoteDaemon.Core

cmd :: Command
cmd = noCommit $ 
	command "remotedaemon" SectionPlumbing
		"detects when remotes have changed, and fetches from them"
		paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing start

start :: CommandStart
start = do
	liftIO runForeground
	stop
