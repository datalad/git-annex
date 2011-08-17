{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Add where

import Control.Monad.State (liftIO)
import System.Posix.Files
import System.Directory
import Control.Exception.Control (handle)
import Control.Exception.Base (throwIO)
import Control.Exception.Extensible (IOException)

import Command
import qualified Annex
import qualified AnnexQueue
import qualified Backend
import LocationLog
import Types
import Content
import Messages
import Utility
import Touch
import Locations

command :: [Command]
command = [repoCommand "add" paramPath seek "add files to annex"]

{- Add acts on both files not checked into git yet, and unlocked files. -}
seek :: [CommandSeek]
seek = [withFilesNotInGit start, withFilesUnlocked start]

{- The add subcommand annexes a file, storing it in a backend, and then
 - moving it into the annex directory and setting up the symlink pointing
 - to its content. -}
start :: CommandStartBackendFile
start pair@(file, _) = notAnnexed file $ do
	s <- liftIO $ getSymbolicLinkStatus file
	if isSymbolicLink s || not (isRegularFile s)
		then stop
		else do
			showStart "add" file
			next $ perform pair

perform :: BackendFile -> CommandPerform
perform (file, backend) = do
	k <- Backend.genKey file backend
	case k of
		Nothing -> stop
		Just (key, _) -> do
			handle (undo file key) $ moveAnnex key file
			next $ cleanup file key

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
		rethrow = liftIO $ throwIO e

		-- fromAnnex could fail if the file ownership is weird
		tryharder :: IOException -> Annex ()
		tryharder _ = do
			g <- Annex.gitRepo
			liftIO $ renameFile (gitAnnexLocation g key) file

cleanup :: FilePath -> Key -> CommandCleanup
cleanup file key = do
	handle (undo file key) $ do
		link <- calcGitLink file key
		liftIO $ createSymbolicLink link file
		logStatus key InfoPresent
	
		-- touch the symlink to have the same mtime as the
		-- file it points to
		s <- liftIO $ getFileStatus file
		let mtime = modificationTime s
		liftIO $ touch file (TimeSpec mtime) False

	force <- Annex.getState Annex.force
	if force
		then AnnexQueue.add "add" [Param "-f", Param "--"] [file]
		else AnnexQueue.add "add" [Param "--"] [file]
	return True
