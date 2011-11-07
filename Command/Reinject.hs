{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Reinject where

import Common.Annex
import Command
import Logs.Location
import Annex.Content
import qualified Command.Fsck

def :: [Command]
def = [command "reinject" (paramPair "SRC" "DEST") seek
	"sets content of annexed file"]

seek :: [CommandSeek]
seek = [withWords start]

start :: [FilePath] -> CommandStart
start (src:dest:[])
	| src == dest = stop
	| otherwise = do
		showStart "reinject" dest
		next $ perform src dest
start _ = error "specify a src file and a dest file"

perform :: FilePath -> FilePath -> CommandPerform
perform src dest = isAnnexed dest $ \(key, backend) -> do
	unlessM (move key) $ error "mv failed!"
	next $ cleanup key backend
	where
		-- the file might be on a different filesystem,
		-- so mv is used rather than simply calling
		-- moveToObjectDir; disk space is also
		-- checked this way.
		move key = getViaTmp key $ \tmp ->
			liftIO $ boolSystem "mv" [File src, File tmp]

cleanup :: Key -> Backend Annex -> CommandCleanup
cleanup key backend = do
	logStatus key InfoPresent

	-- fsck the new content
	size_ok <- Command.Fsck.checkKeySize key
	backend_ok <- Command.Fsck.checkBackend backend key

	return $ size_ok && backend_ok
