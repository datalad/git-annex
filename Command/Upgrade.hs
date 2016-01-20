{- git-annex command
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Upgrade where

import Command
import Upgrade

cmd :: Command
cmd = dontCheck repoExists $ -- because an old version may not seem to exist
	noDaemonRunning $ -- avoid upgrading repo out from under daemon
	command "upgrade" SectionMaintenance "upgrade repository layout"
		paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing start

start :: CommandStart
start = do
	showStart "upgrade" "."
	r <- upgrade False
	next $ next $ return r
