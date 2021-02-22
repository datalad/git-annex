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
import Annex.Link
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
	Annex.Queue.addCommand [] "rm"
		[ Param "--cached"
		, Param "--force"
		, Param "--quiet"
		, Param "--"
		]
		[fromRawFilePath file]
	isAnnexLink file >>= \case
		-- If the file is locked, it needs to be replaced with
		-- the content from the annex. Note that it's possible
		-- for key' (read from the symlink) to differ from key
		-- (cached in git).
		Just key' -> do
			removeassociated
			next $ cleanup file key'
		-- If the file is unlocked, it can be unmodified or not and
		-- does not need to be replaced either way.
		Nothing -> do
			removeassociated
			next $ return True
  where
	removeassociated = 
		Database.Keys.removeAssociatedFile key
			=<< inRepo (toTopFilePath file)

cleanup :: RawFilePath -> Key -> CommandCleanup
cleanup file key = do
	liftIO $ removeFile (fromRawFilePath file)
	src <- calcRepo (gitAnnexLocation key)
	ifM (Annex.getState Annex.fast)
		( do
			-- Only make a hard link if the annexed file does not
			-- already have other hard links pointing at it. This
			-- avoids unannexing (and uninit) ending up hard
			-- linking files together, which would be surprising.
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
