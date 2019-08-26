{- git-annex command
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Direct where

import Command

cmd :: Command
cmd = notBareRepo $ noDaemonRunning $
	command "direct" SectionSetup "switch repository to direct mode (deprecated)"
		paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing (commandAction start)

start :: CommandStart
start = giveup "Direct mode is not supported by this repository version. Use git-annex unlock instead."
