{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Fix where

import Common.Annex
import Command
import qualified Annex.Queue
import Annex.Content

command :: [Command]
command = [repoCommand "fix" paramPaths seek
	"fix up symlinks to point to annexed content"]

seek :: [CommandSeek]
seek = [withFilesInGit start]

{- Fixes the symlink to an annexed file. -}
start :: FilePath -> CommandStart
start file = isAnnexed file $ \(key, _) -> do
	link <- calcGitLink file key
	l <- liftIO $ readSymbolicLink file
	if link == l
		then stop
		else do
			showStart "fix" file
			next $ perform file link

perform :: FilePath -> FilePath -> CommandPerform
perform file link = do
	liftIO $ createDirectoryIfMissing True (parentDir file)
	liftIO $ removeFile file
	liftIO $ createSymbolicLink link file
	next $ cleanup file

cleanup :: FilePath -> CommandCleanup
cleanup file = do
	Annex.Queue.add "add" [Param "--"] [file]
	return True
