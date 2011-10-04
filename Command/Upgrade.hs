{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Upgrade where

import AnnexCommon
import Command
import Upgrade
import Version

command :: [Command]
command = [standaloneCommand "upgrade" paramNothing seek
	"upgrade repository layout"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = do
	showStart "upgrade" "."
	r <- upgrade
	setVersion
	next $ next $ return r
