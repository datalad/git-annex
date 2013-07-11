{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.Fix where

import System.PosixCompat.Files

import Common.Annex
import Command
import qualified Annex.Queue
#ifndef __ANDROID__
import Utility.Touch
#endif

def :: [Command]
def = [notDirect $ noCommit $ command "fix" paramPaths seek
	SectionMaintenance "fix up symlinks to point to annexed content"]

seek :: [CommandSeek]
seek = [withFilesInGit $ whenAnnexed start]

{- Fixes the symlink to an annexed file. -}
start :: FilePath -> (Key, Backend) -> CommandStart
start file (key, _) = do
	link <- inRepo $ gitAnnexLink file key
	stopUnless ((/=) (Just link) <$> liftIO (catchMaybeIO $ readSymbolicLink file)) $ do
		showStart "fix" file
		next $ perform file link

perform :: FilePath -> FilePath -> CommandPerform
perform file link = do
	liftIO $ do
#ifndef __ANDROID__
		-- preserve mtime of symlink
		mtime <- catchMaybeIO $ TimeSpec . modificationTime
			<$> getSymbolicLinkStatus file
#endif
		createDirectoryIfMissing True (parentDir file)
		removeFile file
		createSymbolicLink link file
#ifndef __ANDROID__
		maybe noop (\t -> touch file t False) mtime
#endif
	next $ cleanup file

cleanup :: FilePath -> CommandCleanup
cleanup file = do
	Annex.Queue.addCommand "add" [Param "--force", Param "--"] [file]
	return True
