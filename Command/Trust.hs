{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Trust where

import Common.Annex
import Command
import qualified Remote
import Logs.Trust

def :: [Command]
def = [command "trust" (paramRepeating paramRemote) seek
	SectionSetup "trust a repository"]

seek :: CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start ws = do
	let name = unwords ws
	showStart "trust" name
	u <- Remote.nameToUUID name
	next $ perform u

perform :: UUID -> CommandPerform
perform uuid = do
	trustSet uuid Trusted
	next $ return True
