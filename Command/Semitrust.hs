{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Semitrust where

import Command
import qualified Remote
import UUID
import Trust
import Messages

command :: [Command]
command = [repoCommand "semitrust" (paramRepeating paramRemote) seek
	"return repository to default trust level"]

seek :: [CommandSeek]
seek = [withWords start]

start :: CommandStartWords
start ws = do
	let name = unwords ws
	showStart "semitrust" name
	u <- Remote.nameToUUID name
	next $ perform u

perform :: UUID -> CommandPerform
perform uuid = do
	trustSet uuid SemiTrusted
	next $ return True
