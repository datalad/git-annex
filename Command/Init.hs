{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Init where

import Command
import qualified Annex
import UUID
import Messages
import Init
	
command :: [Command]
command = [standaloneCommand "init" paramDesc seek
		"initialize git-annex"]

seek :: [CommandSeek]
seek = [withWords start]

start :: CommandStartWords
start ws = do
	showStart "init" description
	next $ perform description
	where
		description = unwords ws

perform :: String -> CommandPerform
perform description = do
	initialize
	g <- Annex.gitRepo
	u <- getUUID g
	describeUUID u description
	next $ return True
