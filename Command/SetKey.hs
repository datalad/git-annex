{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.SetKey where

import Common.Annex
import Command
import Logs.Location
import Annex.Content

command :: [Command]
command = [Command "setkey" paramPath defaultChecks seek
	"sets annexed content for a key using a temp file"]

seek :: [CommandSeek]
seek = [withStrings start]

{- Sets cached content for a key. -}
start :: FilePath -> CommandStart
start file = do
	showStart "setkey" file
	next $ perform file

perform :: FilePath -> CommandPerform
perform file = do
	key <- cmdlineKey
	-- the file might be on a different filesystem, so mv is used
	-- rather than simply calling moveToObjectDir; disk space is also
	-- checked this way.
	ok <- getViaTmp key $ \dest ->
		if dest /= file
			then liftIO $
				boolSystem "mv" [File file, File dest]
			else return True
	if ok
		then next cleanup
		else error "mv failed!"

cleanup :: CommandCleanup
cleanup = do
	key <- cmdlineKey
	logStatus key InfoPresent
	return True
