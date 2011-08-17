{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Describe where

import Command
import qualified Remote
import UUID
import Messages

command :: [Command]
command = [repoCommand "describe" (paramPair paramRemote paramDesc) seek
	"change description of a repository"]

seek :: [CommandSeek]
seek = [withWords start]

start :: CommandStartWords
start ws = do
	let (name, description) =
		case ws of
			(n:d) -> (n,unwords d)
			_ -> error "Specify a repository and a description."
	
	showStart "describe" name
	u <- Remote.nameToUUID name
	next $ perform u description

perform :: UUID -> String -> CommandPerform
perform u description = do
	describeUUID u description
	next $ return True
