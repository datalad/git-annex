{- git-annex command
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.Fix where

import Command
import Config
import qualified Annex
import Annex.ReplaceFile
import Annex.Content
import Annex.Perms
import Annex.Link
import qualified Database.Keys
import qualified Utility.RawFilePath as R

import System.PosixCompat.Files (fileMode, linkCount)
#if ! defined(mingw32_HOST_OS)
import qualified System.Posix.Files as Posix
import Utility.Touch
#endif

cmd :: Command
cmd = noCommit $ withAnnexOptions [annexedMatchingOptions, jsonOptions] $
	command "fix" SectionMaintenance
		"fix up links to annexed content"
		paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek ps = unlessM crippledFileSystem $
	withFilesInGitAnnex ww seeker =<< workTreeItems ww ps
  where
	ww = WarnUnmatchLsFiles "fix"
	seeker = AnnexedFileSeeker
		{ startAction = const $ start FixAll
		, checkContentPresent = Nothing
		, usesLocationLog = False
		}

data FixWhat = FixSymlinks | FixAll

start :: FixWhat -> SeekInput -> RawFilePath -> Key -> CommandStart
start fixwhat si file key = do
	currlink <- liftIO $ catchMaybeIO $ R.readSymbolicLink file
	wantlink <- calcRepo $ gitAnnexLink file key
	case currlink of
		Just l
			| l /=  wantlink -> fixby $ fixSymlink file wantlink
			| otherwise -> stop
		Nothing -> case fixwhat of
			FixAll -> fixthin
			FixSymlinks -> stop
  where
	fixby = starting "fix" (mkActionItem (key, file)) si
	fixthin = do
		obj <- calcRepo (gitAnnexLocation key)
		stopUnless (isUnmodified key file <&&> isUnmodified key obj) $ do
			thin <- annexThin <$> Annex.getGitConfig
			fs <- liftIO $ catchMaybeIO $ R.getFileStatus file
			os <- liftIO $ catchMaybeIO $ R.getFileStatus obj
			case (linkCount <$> fs, linkCount <$> os, thin) of
				(Just 1, Just 1, True) ->
					fixby $ makeHardLink file key
				(Just n, Just n', False) | n > 1 && n == n' ->
					fixby $ breakHardLink file key obj
				_ -> stop

breakHardLink :: RawFilePath -> Key -> RawFilePath -> CommandPerform
breakHardLink file key obj = do
	replaceWorkTreeFile file $ \tmp -> do
		mode <- liftIO $ catchMaybeIO $ fileMode <$> R.getFileStatus file
		unlessM (checkedCopyFile key obj tmp mode) $
			giveup "unable to break hard link"
		thawContent tmp
		Database.Keys.storeInodeCaches key [tmp]
		modifyContentDir obj $ freezeContent obj
	next $ return True

makeHardLink :: RawFilePath -> Key -> CommandPerform
makeHardLink file key = do
	replaceWorkTreeFile file $ \tmp -> do
		mode <- liftIO $ catchMaybeIO $ fileMode <$> R.getFileStatus file
		linkFromAnnex' key tmp mode >>= \case
			LinkAnnexFailed -> giveup "unable to make hard link"
			_ -> noop
	next $ return True

fixSymlink :: RawFilePath -> RawFilePath -> CommandPerform
fixSymlink file link = do
#if ! defined(mingw32_HOST_OS)
	-- preserve mtime of symlink
	mtime <- liftIO $ catchMaybeIO $ Posix.modificationTimeHiRes
		<$> R.getSymbolicLinkStatus file
#endif
	replaceWorkTreeFile file $ \tmpfile -> do
		liftIO $ R.createSymbolicLink link tmpfile
#if ! defined(mingw32_HOST_OS)
		liftIO $ maybe noop (\t -> touch tmpfile t False) mtime
#endif
	stageSymlink file =<< hashSymlink link
	next $ return True
