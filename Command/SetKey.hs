{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.SetKey where

import Control.Monad.State (liftIO)

import Command
import Utility
import LocationLog
import Content
import Messages

command :: [Command]
command = [repoCommand "setkey" paramPath seek
	"sets annexed content for a key using a temp file"]

seek :: [CommandSeek]
seek = [withTempFile start]

{- Sets cached content for a key. -}
start :: CommandStartString
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
