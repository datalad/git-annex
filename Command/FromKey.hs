{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.FromKey where

import Control.Monad.State (liftIO)
import System.Posix.Files
import System.Directory
import Control.Monad (unless)

import Command
import qualified AnnexQueue
import Utility
import Content
import Messages
import Types.Key

command :: [Command]
command = [repoCommand "fromkey" paramPath seek
	"adds a file using a specific key"]

seek :: [CommandSeek]
seek = [withFilesMissing start]

start :: CommandStartString
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
	AnnexQueue.add "add" [Param "--"] [file]
	return True
