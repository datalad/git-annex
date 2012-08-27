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
import Option

def :: [Command]
def = [withOptions [foregroundOption, stopOption] $ 
	command "watch" paramNothing seek "watch for changes"]

seek :: [CommandSeek]
seek = [withFlag stopOption $ \stopdaemon -> 
	withFlag foregroundOption $ \foreground ->
	withNothing $ start False foreground stopdaemon]

foregroundOption :: Option
foregroundOption = Option.flag [] "foreground" "do not daemonize"

stopOption :: Option
stopOption = Option.flag [] "stop" "stop daemon"

start :: Bool -> Bool -> Bool -> CommandStart
start assistant foreground stopdaemon = notBareRepo $ do
	if stopdaemon
		then stopDaemon
		else startDaemon assistant foreground Nothing -- does not return
	stop
