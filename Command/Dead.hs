{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Dead where

import Command
import Types.TrustLevel
import Command.Trust (trustCommand)

def :: [Command]
def = [command "dead" (paramRepeating paramRemote) seek
	SectionSetup "hide a lost repository"]

seek :: CommandSeek
seek = trustCommand "dead" DeadTrusted
