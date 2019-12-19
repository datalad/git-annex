{- git-annex v7 -> v8 upgrade support
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Upgrade.V7 where

import Annex.Common
import Annex.CatFile
import qualified Database.Keys
import qualified Database.Keys.SQL
import qualified Git.LsFiles as LsFiles
import qualified Git
import Git.FilePath

upgrade :: Bool -> Annex Bool
upgrade automatic = do
	unless automatic $
		showAction "v7 to v8"

	-- The fsck databases are not transitioned here; any running
	-- incremental fsck can continue to write to the old database.
	-- The next time an incremental fsck is started, it will delete the
	-- old database, and just re-fsck the files.
	
	-- The old content identifier database is deleted here, but the
	-- new database is not populated. It will be automatically
	-- populated from the git-annex branch the next time it is used.
	removeOldDb gitAnnexContentIdentifierDbDirOld
	liftIO . nukeFile =<< fromRepo gitAnnexContentIdentifierLockOld

	-- The export databases are deleted here. The new databases
	-- will be populated by the next thing that needs them, the same
	-- way as they would be in a fresh clone.
	removeOldDb gitAnnexExportDir

	populateKeysDb
	removeOldDb gitAnnexKeysDbOld
	liftIO . nukeFile =<< fromRepo gitAnnexKeysDbIndexCacheOld
	liftIO . nukeFile =<< fromRepo gitAnnexKeysDbLockOld
	
	return True

gitAnnexKeysDbOld :: Git.Repo -> FilePath
gitAnnexKeysDbOld r = fromRawFilePath (gitAnnexDir r) </> "keys"

gitAnnexKeysDbLockOld :: Git.Repo -> FilePath
gitAnnexKeysDbLockOld r = gitAnnexKeysDbOld r ++ ".lck"

gitAnnexKeysDbIndexCacheOld :: Git.Repo -> FilePath
gitAnnexKeysDbIndexCacheOld r = gitAnnexKeysDbOld r ++ ".cache"

gitAnnexContentIdentifierDbDirOld :: Git.Repo -> FilePath
gitAnnexContentIdentifierDbDirOld r = fromRawFilePath (gitAnnexDir r) </> "cids"

gitAnnexContentIdentifierLockOld :: Git.Repo -> FilePath
gitAnnexContentIdentifierLockOld r = gitAnnexContentIdentifierDbDirOld r ++ ".lck"

removeOldDb :: (Git.Repo -> FilePath) -> Annex ()
removeOldDb getdb = do
	db <- fromRepo getdb
	whenM (liftIO $ doesDirectoryExist db) $ do
		v <- liftIO $ tryNonAsync $
#if MIN_VERSION_directory(1,2,7)
			removePathForcibly db
#else
			removeDirectoryRecursive db
#endif
		case v of
			Left ex -> giveup $ "Failed removing old database directory " ++ db ++ " during upgrade (" ++ show ex ++ ") -- delete that and re-run git-annex to finish the upgrade."
			Right () -> return ()

-- Populate the new keys database with associated files and inode caches.
--
-- The information is queried from git. The index contains inode cache
-- information for all staged files, so that is used.
--
-- Note that typically the inode cache of annex objects is also stored in
-- the keys database. This does not add it though, because it's possible
-- that any annex object has gotten modified. The most likely way would be
-- due to annex.thin having been set at some point in the past, bypassing
-- the usual safeguards against object modification. When a worktree file
-- is still a hardlink to an annex object, then they have the same inode
-- cache, so using the inode cache from the git index will get the right
-- thing added in that case. But there are cases where the annex object's
-- inode cache is not added here, most notably when it's not unlocked. 
-- The result will be more work needing to be done by isUnmodified and
-- by inAnnex (the latter only when annex.thin is set) to verify the
-- annex object. That work is only done once, and then the object will
-- finally get its inode cached.
populateKeysDb :: Annex ()
populateKeysDb = do
	top <- fromRepo Git.repoPath
	(l, cleanup) <- inRepo $ LsFiles.inodeCaches [top]
	forM_ l $ \case
		(_f, Nothing) -> giveup "Unable to parse git ls-files --debug output while upgrading git-annex sqlite databases."
		(f, Just ic) -> unlessM (liftIO $ isSymbolicLink <$> getSymbolicLinkStatus f) $ do
			catKeyFile (toRawFilePath f) >>= \case
				Nothing -> noop
				Just k -> do
					topf <- inRepo $ toTopFilePath $ toRawFilePath f
					Database.Keys.runWriter $ \h -> liftIO $ do
						Database.Keys.SQL.addAssociatedFileFast k topf h
						Database.Keys.SQL.addInodeCaches k [ic] h
	liftIO $ void cleanup
	Database.Keys.closeDb

