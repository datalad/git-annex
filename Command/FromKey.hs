{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.FromKey where

import Common.Annex
import Command
import qualified Annex.Queue
import Annex.Content
import Types.Key

command :: [Command]
command = [repoCommand "fromkey" paramPath seek
	"adds a file using a specific key"]

seek :: [CommandSeek]
seek = [withFilesMissing start]

start :: FilePath -> CommandStart
start file = notBareRepo $ do
	key <- cmdlineKey
	inbackend <- inAnnex key
	unless inbackend $ error $
		"key ("++keyName key++") is not present in backend"
	showStart "fromkey" file
	next $ perform file

perform :: FilePath -> CommandPerform
perform file = do
	key <- cmdlineKey
	link <- calcGitLink file key
	liftIO $ createDirectoryIfMissing True (parentDir file)
	liftIO $ createSymbolicLink link file
	next $ cleanup file

cleanup :: FilePath -> CommandCleanup
cleanup file = do
	Annex.Queue.add "add" [Param "--"] [file]
	return True
