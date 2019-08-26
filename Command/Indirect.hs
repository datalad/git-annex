{- git-annex command
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Indirect where

import Command

cmd :: Command
cmd = notBareRepo $ noDaemonRunning $
	command "indirect" SectionSetup "switch repository to indirect mode (deprecated)"
		paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing (commandAction start)

start :: CommandStart
start = stop
