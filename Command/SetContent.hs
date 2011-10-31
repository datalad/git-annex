{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.SetContent where

import Common.Annex
import Command
import Logs.Location
import Annex.Content
import qualified Backend
import qualified Command.Fsck

def :: [Command]
def = [command "setcontent" (paramPair paramPath paramPath) seek
	"sets content of annexed file"]

seek :: [CommandSeek]
seek = [withWords start]

start :: [FilePath] -> CommandStart
start (src:dest:[]) = do
	showStart "setkey" dest
	next $ perform src dest
start _ = error "specify a src file and a dest file"

perform :: FilePath -> FilePath -> CommandPerform
perform src dest = go =<< Backend.lookupFile dest
	where
		go Nothing = error "dest file is not in annex"
		go (Just (key, backend)) = do
			-- the file might be on a different filesystem,
			-- so mv is used rather than simply calling
			-- moveToObjectDir; disk space is also
			-- checked this way.
			ok <- getViaTmp key $ \tmp ->
				if dest /= src
					then liftIO $
						boolSystem "mv" [File src, File tmp]
					else return True
			if ok
				then next $ cleanup key backend
				else error "mv failed!"

cleanup :: Key -> Backend Annex -> CommandCleanup
cleanup key backend = do
	logStatus key InfoPresent

	-- fsck the new content
	size_ok <- Command.Fsck.checkKeySize key
	backend_ok <- Command.Fsck.checkBackend backend key

	return $ size_ok && backend_ok
