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
 - There are two local log files:
 - * migrate.log contains pairs of old and new keys, and is used while
 -   performing a new migration, to build up a migration to commit.
 -   This allows an interrupted migration to be resumed later.
 - * migrations.log has as its first line a commit to the git-annex branch
 -   up to which all migrations have been performed locally (including any
 -   migrations in parent commits). Or the first line may be a null sha when
 -   this has not been done yet. The rest of the lines in the file
 -   are commits that have been made for locally performed migrations,
 -   but whose parent commits have not necessarily been checked for
 -   migrations yet.
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
	streamNewDistributedMigrations,
) where

import Annex.Common
import qualified Git
import qualified Annex
import qualified Annex.Branch
import Git.Types
import Git.Tree
import Git.FilePath
import Git.Ref
import Git.Sha
import Git.Log
import Logs.File
import Logs
import Annex.CatFile

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Control.Concurrent.STM
import System.FilePath.ByteString as P

-- | What to use to record a migration. This should be the same Sha that is
-- used to as the content of the annexed file in the HEAD branch.
newtype MigrationRecord = MigrationRecord { fromMigrationRecord :: Git.Sha }

-- | Logs a migration from an old to a new key.
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
	logf <- fromRepo gitAnnexMigrateLog
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
			commitsha <- Annex.Branch.rememberTreeish treesha
				(asTopFilePath migrationTreeGraftPoint)
			committedMigration commitsha

-- Streams distributed migrations from the git-annex branch,
-- and runs the provided action on each old and new key pair.
--
-- With the incremental option, only scans as far as the last recorded
-- migration that this has handled before.
streamNewDistributedMigrations :: Bool -> (Key -> Key -> Annex ()) -> Annex ()
streamNewDistributedMigrations incremental a = do
	void Annex.Branch.update
	branchsha <- Annex.Branch.getBranch
	(stoppoint, toskip) <- getPerformedMigrations
	(l, cleanup) <- inRepo $ getGitLog branchsha
		(if incremental then stoppoint else Nothing)
		[fromRawFilePath migrationTreeGraftPoint]
		-- Need to follow because migrate.tree is grafted in 
		-- and then deleted, and normally git log stops when a file
		-- gets deleted.
		([Param "--reverse", Param "--follow"])
		(\commit _file -> Just commit)
	forM_ l (go toskip)
	liftIO $ void cleanup
	recordPerformedMigrations branchsha toskip
  where
	go toskip c
		| newref c `elem` nullShas = return ()
		| changed c `elem` toskip = return ()
		| not ("/new/" `B.isInfixOf` newfile) = return ()
		| otherwise = 
			catKey (newref c) >>= \case
				Nothing -> return ()
				Just newkey -> catKey oldfileref >>= \case
					Nothing -> return ()
					Just oldkey -> a oldkey newkey
	  where
		newfile = toRawFilePath (changedfile c)
		oldfile = migrationTreeGraftPoint 
			P.</> "old" 
			P.</> P.takeBaseName (fromInternalGitPath newfile)
		oldfileref = branchFileRef (changed c) oldfile

getPerformedMigrations :: Annex (Maybe Sha, [Sha])
getPerformedMigrations = do
	logf <- fromRepo gitAnnexMigrationsLog
	lckf <- fromRepo gitAnnexMigrationsLock
	ls <- calcLogFile logf lckf [] (:)
	return $ case reverse ls of
		[] -> (Nothing, [])
		(stoppoint:toskip) ->
			let stoppoint' = conv stoppoint
			in 
				( if stoppoint' `elem` nullShas
					then Nothing
					else Just stoppoint'
				, map conv toskip
				)
  where
	conv = Git.Ref . L.toStrict

-- Record locally that migrations have been performed up to the given
-- commit. The list is additional commits that can be removed from the
-- log file if present.
recordPerformedMigrations :: Sha -> [Sha] -> Annex ()
recordPerformedMigrations commit toremove = do
	logf <- fromRepo gitAnnexMigrationsLog
	lckf <- fromRepo gitAnnexMigrationsLock
	modifyLogFile logf lckf (update . drop 1)
  where
	update l = L.fromStrict (fromRef' commit) : filter (`notElem` toremove') l
	toremove' = map (L.fromStrict . fromRef') toremove

-- Record that a migration was performed locally and committed.
-- Since committing a migration may result in parent migrations that have
-- not yet been processed locally, that commit cannot be the first line of
-- the log file, which is reserved for commits whose parents have also had
-- their migrations handled. So if the log file does not exist or is empty,
-- make the first line a null sha.
committedMigration :: Sha -> Annex ()
committedMigration commitsha = do
	logf <- fromRepo gitAnnexMigrationsLog
	lckf <- fromRepo gitAnnexMigrationsLock
	modifyLogFile logf lckf update
  where
	update [] = [conv deleteSha, conv commitsha]
	update logged = logged ++ [conv commitsha]
	conv = L.fromStrict . fromRef'
