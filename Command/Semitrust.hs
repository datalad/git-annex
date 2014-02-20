{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Semitrust where

import Common.Annex
import Command
import qualified Remote
import Logs.Trust

def :: [Command]
def = [command "semitrust" (paramRepeating paramRemote) seek
	SectionSetup "return repository to default trust level"]

seek :: CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start ws = do
	let name = unwords ws
	showStart "semitrust" name
	u <- Remote.nameToUUID name
	next $ perform u

perform :: UUID -> CommandPerform
perform uuid = do
	trustSet uuid SemiTrusted
	next $ return True
