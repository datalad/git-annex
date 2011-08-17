{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Trust where

import Command
import qualified Remote
import Trust
import UUID
import Messages

command :: [Command]
command = [repoCommand "trust" (paramRepeating paramRemote) seek
	"trust a repository"]

seek :: [CommandSeek]
seek = [withWords start]

start :: CommandStartWords
start ws = do
	let name = unwords ws
	showStart "trust" name
	u <- Remote.nameToUUID name
	next $ perform u

perform :: UUID -> CommandPerform
perform uuid = do
	trustSet uuid Trusted
	next $ return True
