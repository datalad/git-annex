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
import Utility.InodeCache
import Annex.InodeSentinal
import Git.FilePath
import qualified Utility.RawFilePath as R

import System.PosixCompat.Files (linkCount)

cmd :: Command
cmd = withAnnexOptions [jsonOptions, annexedMatchingOptions] $
	command "unannex" SectionUtility
		"undo accidental add command"
		paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek ps = withFilesInGitAnnex ww (seeker False) =<< workTreeItems ww ps
  where
	ww = WarnUnmatchLsFiles "unannex"

seeker :: Bool -> AnnexedFileSeeker
seeker fast = AnnexedFileSeeker
	{ startAction = const $ start fast
	, checkContentPresent = Just True
	, usesLocationLog = False
	}

start :: Bool -> SeekInput -> OsPath -> Key -> CommandStart
start fast si file key = 
	starting "unannex" (mkActionItem (key, file)) si $
		perform fast file key

perform :: Bool -> OsPath -> Key -> CommandPerform
perform fast file key = do
	Annex.Queue.addCommand [] "rm"
		[ Param "--cached"
		, Param "--force"
		, Param "--quiet"
		, Param "--"
		]
		[fromOsPath file]
	isAnnexLink file >>= \case
		-- If the file is locked, it needs to be replaced with
		-- the content from the annex. Note that it's possible
		-- for key' (read from the symlink) to differ from key
		-- (cached in git).
		Just key' -> do
			cleanupdb
			next $ cleanup fast file key'
		-- If the file is unlocked, it can be unmodified or not and
		-- does not need to be replaced either way.
		Nothing -> do
			cleanupdb
			next $ return True
  where
	cleanupdb = do
		Database.Keys.removeAssociatedFile key
			=<< inRepo (toTopFilePath file)
		maybe noop Database.Keys.removeInodeCache
			=<< withTSDelta (liftIO . genInodeCache file)

cleanup :: Bool -> OsPath -> Key -> CommandCleanup
cleanup fast file key = do
	liftIO $ removeFile file
	src <- calcRepo (gitAnnexLocation key)
	ifM (pure fast <||> Annex.getRead Annex.fast)
		( do
			-- Only make a hard link if the annexed file does not
			-- already have other hard links pointing at it. This
			-- avoids unannexing (and uninit) ending up hard
			-- linking files together, which would be surprising.
			s <- liftIO $ R.getFileStatus (fromOsPath src)
			if linkCount s > 1
				then copyfrom src
				else hardlinkfrom src
		, copyfrom src
		)
  where
	copyfrom src = 
		thawContent file `after`
			liftIO (copyFileExternal CopyAllMetaData src file)
	hardlinkfrom src =
		-- creating a hard link could fall; fall back to copying
		ifM (liftIO $ tryhardlink src file)
			( return True
			, copyfrom src
			)
	tryhardlink src dest = catchBoolIO $ do
		R.createLink (fromOsPath src) (fromOsPath dest)
		return True
