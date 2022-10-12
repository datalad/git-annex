{- git-annex v7 -> v8 upgrade support
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Upgrade.V7 where

import qualified Annex
import Annex.Common
import Types.Upgrade
import Annex.CatFile
import qualified Database.Keys
import qualified Database.Keys.SQL
import Database.Keys.Tables
import qualified Git.LsFiles as LsFiles
import qualified Git
import Git.FilePath
import Config
import qualified Utility.RawFilePath as R

import qualified System.FilePath.ByteString as P

upgrade :: Bool -> Annex UpgradeResult
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
	removeOldDb . fromRawFilePath =<< fromRepo gitAnnexContentIdentifierDbDirOld
	liftIO . removeWhenExistsWith R.removeLink
		=<< fromRepo gitAnnexContentIdentifierLockOld

	-- The export databases are deleted here. The new databases
	-- will be populated by the next thing that needs them, the same
	-- way as they would be in a fresh clone.
	removeOldDb . fromRawFilePath =<< calcRepo' gitAnnexExportDir

	populateKeysDb
	removeOldDb . fromRawFilePath =<< fromRepo gitAnnexKeysDbOld
	liftIO . removeWhenExistsWith R.removeLink
		=<< fromRepo gitAnnexKeysDbIndexCacheOld
	liftIO . removeWhenExistsWith R.removeLink
		=<< fromRepo gitAnnexKeysDbLockOld
	
	updateSmudgeFilter

	return UpgradeSuccess

gitAnnexKeysDbOld :: Git.Repo -> RawFilePath
gitAnnexKeysDbOld r = gitAnnexDir r P.</> "keys"

gitAnnexKeysDbLockOld :: Git.Repo -> RawFilePath
gitAnnexKeysDbLockOld r = gitAnnexKeysDbOld r <> ".lck"

gitAnnexKeysDbIndexCacheOld :: Git.Repo -> RawFilePath
gitAnnexKeysDbIndexCacheOld r = gitAnnexKeysDbOld r <> ".cache"

gitAnnexContentIdentifierDbDirOld :: Git.Repo -> RawFilePath
gitAnnexContentIdentifierDbDirOld r = gitAnnexDir r P.</> "cids"

gitAnnexContentIdentifierLockOld :: Git.Repo -> RawFilePath
gitAnnexContentIdentifierLockOld r = gitAnnexContentIdentifierDbDirOld r <> ".lck"

removeOldDb :: FilePath -> Annex ()
removeOldDb db =
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
populateKeysDb = unlessM isBareRepo $ do
	top <- fromRepo Git.repoPath
	(l, cleanup) <- inRepo $ LsFiles.inodeCaches [top]
	forM_ l $ \case
		(_f, Nothing) -> giveup "Unable to parse git ls-files --debug output while upgrading git-annex sqlite databases."
		(f, Just ic) -> unlessM (liftIO $ catchBoolIO $ isSymbolicLink <$> getSymbolicLinkStatus f) $ do
			catKeyFile (toRawFilePath f) >>= \case
				Nothing -> noop
				Just k -> do
					topf <- inRepo $ toTopFilePath $ toRawFilePath f
					Database.Keys.runWriter AssociatedTable $ \h -> liftIO $
						Database.Keys.SQL.addAssociatedFile k topf h
					Database.Keys.runWriter ContentTable $ \h -> liftIO $
						Database.Keys.SQL.addInodeCaches k [ic] h
	liftIO $ void cleanup
	Database.Keys.closeDb

-- The gitatrributes used to have a line that prevented filtering dotfiles,
-- but now they are filtered and annex.dotfiles controls whether they get
-- added to the annex.
--
-- Only done on local gitattributes, not any gitatrributes that might be
-- checked into the repository.
updateSmudgeFilter :: Annex ()
updateSmudgeFilter = do
	lf <- fromRawFilePath <$> Annex.fromRepo Git.attributesLocal
	ls <- liftIO $ lines <$> catchDefaultIO "" (readFileStrict lf)
	let ls' = removedotfilter ls
	when (ls /= ls') $
		liftIO $ writeFile lf (unlines ls')
  where
	removedotfilter ("* filter=annex":".* !filter":rest) =
		"* filter=annex" : removedotfilter rest
	removedotfilter (l:ls) = l : removedotfilter ls
	removedotfilter [] = []
