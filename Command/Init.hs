{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Init where

import Common.Annex
import Command
import UUID
import Init
	
command :: [Command]
command = [standaloneCommand "init" paramDesc seek
		"initialize git-annex"]

seek :: [CommandSeek]
seek = [withWords start]

start :: [String] -> CommandStart
start ws = do
	showStart "init" description
	next $ perform description
	where
		description = unwords ws

perform :: String -> CommandPerform
perform description = do
	initialize
	u <- getUUID
	describeUUID u description
	next $ return True
