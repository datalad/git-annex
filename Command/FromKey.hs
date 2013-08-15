{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.FromKey where

import System.PosixCompat.Files

import Common.Annex
import Command
import qualified Annex.Queue
import Annex.Content
import Types.Key

def :: [Command]
def = [notDirect $ notBareRepo $
	command "fromkey" (paramPair paramKey paramPath) seek
		SectionPlumbing "adds a file using a specific key"]

seek :: [CommandSeek]
seek = [withWords start]

start :: [String] -> CommandStart
start (keyname:file:[]) = do
	let key = fromMaybe (error "bad key") $ file2key keyname
	inbackend <- inAnnex key
	unless inbackend $ error $
		"key ("++ keyname ++") is not present in backend"
	showStart "fromkey" file
	next $ perform key file
start _ = error "specify a key and a dest file"

perform :: Key -> FilePath -> CommandPerform
perform key file = do
	link <- inRepo $ gitAnnexLink file key
	liftIO $ createDirectoryIfMissing True (parentDir file)
	liftIO $ createSymbolicLink link file
	next $ cleanup file

cleanup :: FilePath -> CommandCleanup
cleanup file = do
	Annex.Queue.addCommand "add" [Param "--"] [file]
	return True
