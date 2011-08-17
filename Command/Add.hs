{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Add where

import Control.Monad.State (liftIO)
import System.Posix.Files

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
			moveAnnex key file
			next $ cleanup file key

cleanup :: FilePath -> Key -> CommandCleanup
cleanup file key = do
	link <- calcGitLink file key
	liftIO $ createSymbolicLink link file

	logStatus key InfoPresent

	-- touch the symlink to have the same mtime as the file it points to
	s <- liftIO $ getFileStatus file
	let mtime = modificationTime s
	liftIO $ touch file (TimeSpec mtime) False

	force <- Annex.getState Annex.force
	if force
		then AnnexQueue.add "add" [Param "-f", Param "--"] [file]
		else AnnexQueue.add "add" [Param "--"] [file]
	return True
