{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.Fix where

import Common.Annex
import Command
import qualified Annex.Queue
#ifdef WITH_CLIBS
#ifndef __ANDROID__
import Utility.Touch
#endif
#endif

cmd :: Command
cmd = notDirect $ noCommit $ withGlobalOptions annexedMatchingOptions $
	command "fix" SectionMaintenance
		"fix up symlinks to point to annexed content"
		paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withFilesInGit $ whenAnnexed start

{- Fixes the symlink to an annexed file. -}
start :: FilePath -> Key -> CommandStart
start file key = do
	link <- calcRepo $ gitAnnexLink file key
	stopUnless ((/=) (Just link) <$> liftIO (catchMaybeIO $ readSymbolicLink file)) $ do
		showStart "fix" file
		next $ perform file link

perform :: FilePath -> FilePath -> CommandPerform
perform file link = do
	liftIO $ do
#ifdef WITH_CLIBS
#ifndef __ANDROID__
		-- preserve mtime of symlink
		mtime <- catchMaybeIO $ TimeSpec . modificationTime
			<$> getSymbolicLinkStatus file
#endif
#endif
		createDirectoryIfMissing True (parentDir file)
		removeFile file
		createSymbolicLink link file
#ifdef WITH_CLIBS
#ifndef __ANDROID__
		maybe noop (\t -> touch file t False) mtime
#endif
#endif
	next $ cleanup file

cleanup :: FilePath -> CommandCleanup
cleanup file = do
	Annex.Queue.addCommand "add" [Param "--force", Param "--"] [file]
	return True
