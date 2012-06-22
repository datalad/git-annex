{- git-annex assistant
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Assistant where

import Command
import qualified Command.Watch

def :: [Command]
def = [withOptions [Command.Watch.foregroundOption, Command.Watch.stopOption] $ 
	command "assistant" paramNothing seek "automatically handle changes"]

seek :: [CommandSeek]
seek = Command.Watch.mkSeek True
