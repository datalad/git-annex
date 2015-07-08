{- git-annex command
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Reinject where

import Common.Annex
import Command
import Logs.Location
import Annex.Content
import qualified Command.Fsck
import qualified Backend

cmd :: Command
cmd = command "reinject" SectionUtility 
	"sets content of annexed file"
	(paramPair "SRC" "DEST") (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [FilePath] -> CommandStart
start (src:dest:[])
	| src == dest = stop
	| otherwise =
		ifAnnexed src
			(error $ "cannot used annexed file as src: " ++ src)
			go
  where
	go = do
		showStart "reinject" dest
		next $ whenAnnexed (perform src) dest
start _ = error "specify a src file and a dest file"

perform :: FilePath -> FilePath -> Key -> CommandPerform
perform src dest key = do
	{- Check the content before accepting it. -}
	v <- Backend.getBackend dest key
	case v of
		Nothing -> stop
		Just backend ->
			ifM (Command.Fsck.checkKeySizeOr reject key src
				<&&> Command.Fsck.checkBackendOr reject backend key src)
				( do
					unlessM move $ error "mv failed!"
					next $ cleanup key
				, error "not reinjecting"
				)
  where
	-- the file might be on a different filesystem,
	-- so moveFile is used rather than simply calling
	-- moveToObjectDir; disk space is also
	-- checked this way.
	move = getViaTmp key $ \tmp ->
		liftIO $ catchBoolIO $ do
			moveFile src tmp
			return True
	reject = const $ return "wrong file?"

cleanup :: Key -> CommandCleanup
cleanup key = do
	logStatus key InfoPresent
	return True
