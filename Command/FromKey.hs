{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.FromKey where

import Common.Annex
import Command
import qualified Annex.Queue
import Annex.Content
import Types.Key
import qualified Annex

cmd :: [Command]
cmd = [notDirect $ notBareRepo $
	command "fromkey" (paramPair paramKey paramPath) seek
		SectionPlumbing "adds a file using a specific key"]

seek :: CommandSeek
seek ps = do
	force <- Annex.getState Annex.force
	withWords (start force) ps

start :: Bool -> [String] -> CommandStart
start force (keyname:file:[]) = do
	let key = fromMaybe (error "bad key") $ file2key keyname
	unless force $ do
		inbackend <- inAnnex key
		unless inbackend $ error $
			"key ("++ keyname ++") is not present in backend (use --force to override this sanity check)"
	showStart "fromkey" file
	next $ perform key file
start _ _ = error "specify a key and a dest file"

perform :: Key -> FilePath -> CommandPerform
perform key file = do
	link <- calcRepo $ gitAnnexLink file key
	liftIO $ createDirectoryIfMissing True (parentDir file)
	liftIO $ createSymbolicLink link file
	next $ cleanup file

cleanup :: FilePath -> CommandCleanup
cleanup file = do
	Annex.Queue.addCommand "add" [Param "--"] [file]
	return True
