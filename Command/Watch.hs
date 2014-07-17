{- git-annex watch command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Watch where

import Common.Annex
import Assistant
import Command
import Utility.HumanTime

def :: [Command]
def = [notBareRepo $ withOptions [foregroundOption, stopOption] $ 
	command "watch" paramNothing seek SectionCommon "watch for changes"]

seek :: CommandSeek
seek ps = do
	stopdaemon <- getOptionFlag stopOption
	foreground <- getOptionFlag foregroundOption
	withNothing (start False foreground stopdaemon Nothing) ps

foregroundOption :: Option
foregroundOption = flagOption [] "foreground" "do not daemonize"

stopOption :: Option
stopOption = flagOption [] "stop" "stop daemon"

start :: Bool -> Bool -> Bool -> Maybe Duration -> CommandStart
start assistant foreground stopdaemon startdelay = do
	if stopdaemon
		then stopDaemon
		else startDaemon assistant foreground startdelay Nothing Nothing Nothing -- does not return
	stop
