{- git-annex command
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Unannex where

import Command
import qualified Annex
import Annex.Perms
import qualified Annex.Queue
import Utility.CopyFile
import qualified Database.Keys
import Git.FilePath
import qualified Utility.RawFilePath as R

cmd :: Command
cmd = withGlobalOptions [annexedMatchingOptions] $
	command "unannex" SectionUtility
		"undo accidental add command"
		paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek ps = withFilesInGitAnnex ww seeker =<< workTreeItems ww ps
  where
	ww = WarnUnmatchLsFiles

seeker :: AnnexedFileSeeker
seeker = AnnexedFileSeeker
	{ startAction = start
	, checkContentPresent = Just True
	, usesLocationLog = False
	}

start :: SeekInput -> RawFilePath -> Key -> CommandStart
start si file key = 
	starting "unannex" (mkActionItem (key, file)) si $
		perform file key

perform :: RawFilePath -> Key -> CommandPerform
perform file key = do
	liftIO $ removeFile (fromRawFilePath file)
	Annex.Queue.addCommand [] "rm"
		[ Param "--cached"
		, Param "--force"
		, Param "--quiet"
		, Param "--"
		]
		[fromRawFilePath file]
	next $ cleanup file key

cleanup :: RawFilePath -> Key -> CommandCleanup
cleanup file key = do
	Database.Keys.removeAssociatedFile key =<< inRepo (toTopFilePath file)
	src <- calcRepo (gitAnnexLocation key)
	ifM (Annex.getState Annex.fast)
		( do
			-- Only make a hard link if the annexed file does not
			-- already have other hard links pointing at it.
			-- This avoids unannexing (and uninit) ending up
			-- hard linking files together, which would be
			-- surprising.
			s <- liftIO $ R.getFileStatus src
			if linkCount s > 1
				then copyfrom src
				else hardlinkfrom src
		, copyfrom src
		)
  where
	copyfrom src = 
		thawContent file `after` liftIO 
			(copyFileExternal CopyAllMetaData
				(fromRawFilePath src)
				(fromRawFilePath file))
	hardlinkfrom src =
		-- creating a hard link could fall; fall back to copying
		ifM (liftIO $ catchBoolIO $ R.createLink src file >> return True)
			( return True
			, copyfrom src
			)
