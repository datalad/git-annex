{- git-annex watch command
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Watch where

import Common.Annex
import Assistant
import Command
import Utility.HumanTime

cmd :: Command
cmd = notBareRepo $
	command "watch" SectionCommon 
		"watch for changes and autocommit"
		paramNothing (seek <$$> const parseDaemonOptions)

seek :: DaemonOptions -> CommandSeek
seek o = commandAction $ start False o Nothing

start :: Bool -> DaemonOptions -> Maybe Duration -> CommandStart
start assistant o startdelay = do
	if stopDaemonOption o
		then stopDaemon
		else startDaemon assistant (foregroundDaemonOption o) startdelay Nothing Nothing Nothing -- does not return
	stop
