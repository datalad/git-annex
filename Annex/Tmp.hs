{- git-annex tmp files
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Tmp where

import Annex.Common
import qualified Annex
import Annex.LockFile
import Annex.Perms
import Types.CleanupActions

import Data.Time.Clock.POSIX

-- | For creation of tmp files, other than for key's contents.
--
-- The action should normally clean up whatever files it writes to the temp
-- directory that is passed to it. However, once the action is done,
-- any files left in that directory may be cleaned up by another process at
-- any time.
withOtherTmp :: (FilePath -> Annex a) -> Annex a
withOtherTmp a = do
	Annex.addCleanup OtherTmpCleanup cleanupOtherTmp
	tmpdir <- fromRepo gitAnnexTmpOtherDir
	tmplck <- fromRepo gitAnnexTmpOtherLock
	withSharedLock (const tmplck) $ do
		void $ createAnnexDirectory tmpdir
		a tmpdir

-- | This uses an alternate temp directory. The action should normally
-- clean up whatever files it writes there, but if it leaves files
-- there (perhaps due to being interrupted), the files will be eventually
-- cleaned up by another git-annex process (after they're a week old).
--
-- Unlike withOtherTmp, this does not rely on locking working.
-- Its main use is in situations where the state of lockfile is not
-- determined yet, eg during initialization.
withEventuallyCleanedOtherTmp :: (FilePath -> Annex a) -> Annex a
withEventuallyCleanedOtherTmp = bracket setup cleanup
  where
	setup = do
		tmpdir <- fromRepo gitAnnexTmpOtherDirOld
		void $ createAnnexDirectory tmpdir
		return tmpdir
	cleanup = liftIO . void . tryIO . removeDirectory 

-- | Cleans up any tmp files that were left by a previous
-- git-annex process that got interrupted or failed to clean up after
-- itself for some other reason.
--
-- Does not do anything if withOtherTmp is running.
cleanupOtherTmp :: Annex ()
cleanupOtherTmp = do
	tmplck <- fromRepo gitAnnexTmpOtherLock
	void $ tryIO $ tryExclusiveLock (const tmplck) $ do
		tmpdir <- fromRepo gitAnnexTmpOtherDir
		void $ liftIO $ tryIO $ removeDirectoryRecursive tmpdir
		oldtmp <- fromRepo gitAnnexTmpOtherDirOld
		liftIO $ mapM_ cleanold =<< dirContentsRecursive oldtmp
		liftIO $ void $ tryIO $ removeDirectory oldtmp -- when empty
  where
	cleanold f = do
		now <- liftIO getPOSIXTime
		let oldenough = now - (60 * 60 * 24 * 7)
		catchMaybeIO (modificationTime <$> getSymbolicLinkStatus f) >>= \case
			Just mtime | realToFrac mtime <= oldenough -> 
				void $ tryIO $ nukeFile f
			_ -> return ()
