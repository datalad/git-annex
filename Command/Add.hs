{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Add where

import Common.Annex
import Annex.Exception
import Command
import qualified Annex
import qualified Annex.Queue
import qualified Backend
import Logs.Location
import Annex.Content
import Utility.Touch
import Backend

def :: [Command]
def = [command "add" paramPaths seek "add files to annex"]

{- Add acts on both files not checked into git yet, and unlocked files. -}
seek :: [CommandSeek]
seek = [withFilesNotInGit start, withFilesUnlocked start]

{- The add subcommand annexes a file, storing it in a backend, and then
 - moving it into the annex directory and setting up the symlink pointing
 - to its content. -}
start :: BackendFile -> CommandStart
start p@(_, file) = notBareRepo $ notAnnexed file $ do
	s <- liftIO $ getSymbolicLinkStatus file
	if isSymbolicLink s || not (isRegularFile s)
		then stop
		else do
			showStart "add" file
			next $ perform p

perform :: BackendFile -> CommandPerform
perform (backend, file) = do
	k <- Backend.genKey file backend
	case k of
		Nothing -> stop
		Just (key, _) -> do
			handle (undo file key) $ moveAnnex key file
			next $ cleanup file key True

{- On error, put the file back so it doesn't seem to have vanished.
 - This can be called before or after the symlink is in place. -}
undo :: FilePath -> Key -> IOException -> Annex a
undo file key e = do
	unlessM (inAnnex key) rethrow -- no cleanup to do
	liftIO $ whenM (doesFileExist file) $ removeFile file
	handle tryharder $ fromAnnex key file
	logStatus key InfoMissing
	rethrow
	where
		rethrow = throw e

		-- fromAnnex could fail if the file ownership is weird
		tryharder :: IOException -> Annex ()
		tryharder _ = do
			g <- gitRepo
			liftIO $ renameFile (gitAnnexLocation g key) file

cleanup :: FilePath -> Key -> Bool -> CommandCleanup
cleanup file key hascontent = do
	handle (undo file key) $ do
		link <- calcGitLink file key
		liftIO $ createSymbolicLink link file

		when hascontent $ do
			logStatus key InfoPresent
	
			-- touch the symlink to have the same mtime as the
			-- file it points to
			s <- liftIO $ getFileStatus file
			let mtime = modificationTime s
			liftIO $ touch file (TimeSpec mtime) False

	force <- Annex.getState Annex.force
	if force
		then Annex.Queue.add "add" [Param "-f", Param "--"] [file]
		else Annex.Queue.add "add" [Param "--"] [file]
	return True
