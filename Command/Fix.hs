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

def :: [Command]
def = [command "fix" paramPaths seek
	"fix up symlinks to point to annexed content"]

seek :: [CommandSeek]
seek = [withFilesInGit $ whenAnnexed start]

{- Fixes the symlink to an annexed file. -}
start :: FilePath -> (Key, Backend) -> CommandStart
start file (key, _) = do
	link <- calcGitLink file key
	stopUnless ((/=) link <$> liftIO (readSymbolicLink file)) $ do
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
	Annex.Queue.addCommand "add" [Param "--force", Param "--"] [file]
	return True
