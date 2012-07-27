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

mkSeek :: Bool -> [CommandSeek]
mkSeek assistant = [withFlag stopOption $ \stopdaemon -> 
	withFlag foregroundOption $ \foreground ->
	withNothing $ start assistant foreground stopdaemon]

seek :: [CommandSeek]
seek = mkSeek False

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
