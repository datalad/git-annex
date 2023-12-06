{- git-annex migration logs
 -
 - To record a migration in the git-annex branch as space efficiently as
 - possible, it is stored as a tree which contains two subtrees 'old' and 'new'.
 - The subtrees each contain the same filenames, which point to the old
 - and new keys respectively.
 -
 - When the user commits the migrated files to their HEAD branch, that will
 - store pointers to the new keys in git. And pointers to the old keys
 - already exist in git. So recording the migration this way avoids
 - injecting any new objects into git, besides the two trees. Note that for
 - this to be the case, care has to be taken to record the migration 
 - using the same symlink targets or pointer file contents as are used in
 - the HEAD branch.
 -
 - The filenames used in the trees are not the original filenames, to avoid
 - running migrate in a throwaway branch unexpectedly recording that
 - branch's contents.
-
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Logs.Migrate (
	MigrationRecord(..),
	logMigration,
	commitMigration,
) where

import Annex.Common
import qualified Git
import qualified Annex
import qualified Annex.Branch
import Git.Types
import Git.Tree
import Git.FilePath
import Logs.File
import Logs

import qualified Data.ByteString.Lazy as L
import Control.Concurrent.STM

-- | What to use to record a migration. This should be the same Sha that is
-- used to as the content of the annexed file in the HEAD branch.
newtype MigrationRecord = MigrationRecord { fromMigrationRecord :: Git.Sha }

-- | Logs a migration from an old to a new key.
--
-- This writes to a log file, which can later be committed. That allows an
-- interrupted migration to be resumed later.
logMigration :: MigrationRecord -> MigrationRecord -> Annex ()
logMigration old new = do
	logf <- fromRepo gitAnnexMigrateLog
	lckf <- fromRepo gitAnnexMigrateLock
	appendLogFile logf lckf $ L.fromStrict $
		Git.fromRef' (fromMigrationRecord old)
			<> " "
			<> Git.fromRef' (fromMigrationRecord new)

-- | Commits a migration to the git-annex branch.
commitMigration :: Annex ()
commitMigration = do
	logf <- fromRawFilePath <$> fromRepo gitAnnexMigrateLog
	lckf <- fromRepo gitAnnexMigrateLock
	nv <- liftIO $ newTVarIO (0 :: Integer)
	g <- Annex.gitRepo
	withMkTreeHandle g $ \oldh ->
		withMkTreeHandle g $ \newh ->
			streamLogFile logf lckf 
				(finalizer nv oldh newh g)
				(processor nv oldh newh)
  where
	processor nv oldh newh s = case words s of
		(old:new:[]) -> do
			fn <- liftIO $ atomically $ do
				n <- readTVar nv
				let !n' = succ n
				writeTVar nv n'
				return (asTopFilePath (encodeBS (show n')))
			let rec h r = liftIO $ sendMkTree h
				(fromTreeItemType TreeFile)
				BlobObject
				(Git.Ref (encodeBS r))
				fn
			rec oldh old
			rec newh new
		_ -> error "migrate.log parse error"
	finalizer nv oldh newh g = do
		oldt <- liftIO $ finishMkTree oldh
		newt <- liftIO $ finishMkTree newh
		n <- liftIO $ atomically $ readTVar nv
		when (n > 0) $ do
			treesha <- liftIO $ flip recordTree g $ Tree
				[ RecordedSubTree (asTopFilePath "old") oldt []
				, RecordedSubTree (asTopFilePath "new") newt []
				]
			Annex.Branch.rememberTreeish treesha
				(asTopFilePath migrationTreeGraftPoint)
