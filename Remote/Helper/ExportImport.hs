{- Helper to make remotes support export and import (or not).
 -
 - Copyright 2017-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Remote.Helper.ExportImport where

import Annex.Common
import Types.Remote
import Types.Backend
import Types.Key
import Backend
import Remote.Helper.Encryptable (isEncrypted)
import qualified Database.Export as Export
import qualified Database.ContentIdentifier as ContentIdentifier
import Annex.Export
import Annex.Import
import Annex.LockFile
import Config
import Git.Types (fromRef)
import Logs.Export
import Logs.ContentIdentifier (recordContentIdentifier)

import qualified Data.Map as M
import Control.Concurrent.STM

-- | Use for remotes that do not support exports.
class HasExportUnsupported a where
	exportUnsupported :: a

instance HasExportUnsupported (RemoteConfig -> RemoteGitConfig -> Annex Bool) where
	exportUnsupported = \_ _ -> return False

instance HasExportUnsupported (ExportActions Annex) where
	exportUnsupported = ExportActions
		{ storeExport = \_ _ _ _ -> do
			warning "store export is unsupported"
			return False
		, retrieveExport = \_ _ _ _ -> return False
		, checkPresentExport = \_ _ -> return False
		, removeExport = \_ _ -> return False
		, removeExportDirectory = Just $ \_ -> return False
		, renameExport = \_ _ _ -> return False
		}

-- | Use for remotes that do not support imports.
class HasImportUnsupported a where
	importUnsupported :: a

instance HasImportUnsupported (RemoteConfig -> RemoteGitConfig -> Annex Bool) where
	importUnsupported = \_ _ -> return False

instance HasImportUnsupported (ImportActions Annex) where
	importUnsupported = ImportActions
		{ listImportableContents = return Nothing
		, retrieveExportWithContentIdentifier = \_ _ _ _ _ -> return Nothing
		, storeExportWithContentIdentifier = \_ _ _ _ _ -> return Nothing
		}

exportIsSupported :: RemoteConfig -> RemoteGitConfig -> Annex Bool
exportIsSupported = \_ _ -> return True

importIsSupported :: RemoteConfig -> RemoteGitConfig -> Annex Bool
importIsSupported = \_ _ -> return True

-- | Prevent or allow exporttree=yes and importtree=yes when
-- setting up a new remote, depending on exportSupported and importSupported.
adjustExportImportRemoteType :: RemoteType -> RemoteType
adjustExportImportRemoteType rt = rt { setup = setup' }
  where
	setup' st mu cp c gc =
		let checkconfig supported configured setting cont =
			ifM (supported rt c gc)
				( case st of
					Init
						| configured c && isEncrypted c ->
							giveup $ "cannot enable both encryption and " ++ setting
						| otherwise -> cont
					Enable oldc
						| configured c /= configured oldc ->
							giveup $ "cannot change " ++ setting ++ " of existing special remote"
						| otherwise -> cont
				, if configured c
					then giveup $ setting ++ " is not supported by this special remote"
					else cont
				)
		in checkconfig exportSupported exportTree "exporttree" $
			checkconfig importSupported importTree "importtree" $
				if importTree c && not (exportTree c)
					then giveup "cannot enable importtree=yes without also enabling exporttree=yes"
					else setup rt st mu cp c gc

-- | Adjust a remote to support exporttree=yes and importree=yes.
--
-- Note that all remotes with importree=yes also have exporttree=yes.
adjustExportImport :: Remote -> Annex Remote
adjustExportImport r = case M.lookup "exporttree" (config r) of
	Nothing -> return $ notexport r
	Just c -> case yesNo c of
		Just True -> ifM (isExportSupported r)
			( do
				exportdbv <- liftIO $ newTVarIO Nothing
				r' <- isexport exportdbv
				if importTree (config r)
					then isimport r' exportdbv
					else return r'
			, return $ notexport r
			)
		Just False -> return $ notexport r
		Nothing -> do
			warning $ "bad exporttree value for " ++ name r ++ ", assuming not an export"
			return $ notexport r
  where
	notexport r' = notimport r'
		{ exportActions = exportUnsupported
		, remotetype = (remotetype r')
			{ exportSupported = exportUnsupported
			}
		}
	
	notimport r' = r'
		{ importActions = importUnsupported
		, remotetype = (remotetype r')
			{ importSupported = importUnsupported
			}
		}
	
	isimport r' exportdbv = do
		lcklckv <- liftIO newEmptyTMVarIO
		dbtv <- liftIO newEmptyTMVarIO
		let store f k loc p = do
			-- Only open the database once it's needed,
			-- and take an exclusive write lock.
			-- The write lock will then remain held while the
			-- process is running.
			db <- liftIO (atomically (tryReadTMVar dbtv)) >>= \case
				Just (db, _lck) -> return db
				-- let only one thread take the lock
				Nothing -> ifM (liftIO $ atomically $ tryPutTMVar lcklckv ())
					( do
						lck <- takeExclusiveLock gitAnnexContentIdentifierLock
						db <- ContentIdentifier.openDb
						liftIO $ atomically (putTMVar dbtv (db, lck))
						return db
					-- loser waits for winner to open
					-- the db and can then also use its
					-- handle
					, liftIO $ fst <$> atomically (readTMVar dbtv)
					)

			exportdb <- getexportdb exportdbv
			ks <- liftIO $ Export.getExportedKey exportdb loc
			oldcids <- liftIO $ concat
				<$> mapM (ContentIdentifier.getContentIdentifiers db (uuid r')) ks
			storeExportWithContentIdentifier (importActions r') f k loc oldcids p >>= \case
				Nothing -> return False
				Just newcid -> do
					liftIO $ ContentIdentifier.recordContentIdentifier db (uuid r') newcid k
					recordContentIdentifier (uuid r') newcid k
					return True

		return $ r'
			{ exportActions = (exportActions r')
				{ storeExport = store
				}
			}
	
	getexportdb dbv = liftIO (atomically (readTVar dbv)) >>= \case
		Just db -> return db
		Nothing -> do
			db <- Export.openDb (uuid r)
			liftIO $ atomically $ writeTVar dbv $ Just db
			return db

	isexport dbv = do
		updateflag <- liftIO $ newTVarIO Nothing

		-- When multiple threads run this, all except the first
		-- will block until the first runs doneupdateonce.
		-- Returns True when an update should be done and False
		-- when the update has already been done.
		let startupdateonce = liftIO $ atomically $
			readTVar updateflag >>= \case
				Nothing -> do
					writeTVar updateflag (Just True)
					return True
				Just True -> retry
				Just False -> return False
		let doneupdateonce = \updated ->
			when updated $
				liftIO $ atomically $
					writeTVar updateflag (Just False)
		
		exportinconflict <- liftIO $ newTVarIO False

		-- Get export locations for a key. Checks once
		-- if the export log is different than the database and
		-- updates the database, to notice when an export has been
		-- updated from another repository.
		let getexportlocs = \k -> do
			db <- getexportdb dbv
			bracket startupdateonce doneupdateonce $ \updatenow ->
				when updatenow $
					Export.updateExportTreeFromLog db >>= \case
						Export.ExportUpdateSuccess -> return ()
						Export.ExportUpdateConflict -> do
							warnExportConflict r
							liftIO $ atomically $
								writeTVar exportinconflict True
			liftIO $ Export.getExportTree db k

		return $ r
			-- Storing a key on an export could be implemented,
			-- but it would perform unncessary work
			-- when another repository has already stored the
			-- key, and the local repository does not know
			-- about it. To avoid unnecessary costs, don't do it.
			{ storeKey = \_ _ _ -> do
				warning "remote is configured with exporttree=yes; use `git-annex export` to store content on it"
				return False
			-- Keys can be retrieved using retrieveExport, 
			-- but since that retrieves from a path in the
			-- remote that another writer could have replaced
			-- with content not of the requested key,
			-- the content has to be strongly verified.
			--
			-- appendonly remotes have a key/value store,
			-- so don't need to use retrieveExport. However,
			-- fall back to it if retrieveKeyFile fails.
			, retrieveKeyFile = \k af dest p ->
				let retrieveexport = retrieveKeyFileFromExport getexportlocs exportinconflict k af dest p
				in if appendonly r
					then do
						ret@(ok, _v) <- retrieveKeyFile r k af dest p
						if ok
							then return ret
							else retrieveexport
					else retrieveexport
			, retrieveKeyFileCheap = if appendonly r
				then retrieveKeyFileCheap r
				else \_ _ _ -> return False
			-- Removing a key from an export would need to
			-- change the tree in the export log to not include
			-- the file. Otherwise, conflicts when removing
			-- files would not be dealt with correctly.
			-- There does not seem to be a good use case for
			-- removing a key from an export in any case.
			, removeKey = \_k -> do
				warning "dropping content from an export is not supported; use `git annex export` to export a tree that lacks the files you want to remove"
				return False
			-- Can't lock content on exports, since they're
			-- not key/value stores, and someone else could
			-- change what's exported to a file at any time.
			--
			-- (except for appendonly remotes)
			, lockContent = if appendonly r
				then lockContent r
				else Nothing
			-- Check if any of the files a key was exported to
			-- are present. This doesn't guarantee the export
			-- contains the right content, which is why export
			-- remotes are untrusted.
			--
			-- (but appendonly remotes work the same as any
			-- non-export remote)
			, checkPresent = if appendonly r
				then checkPresent r
				else \k -> anyM (checkPresentExport (exportActions r) k)
					=<< getexportlocs k
			-- checkPresent from an export is more expensive
			-- than otherwise, so not cheap. Also, this
			-- avoids things that look at checkPresentCheap and
			-- silently skip non-present files from behaving
			-- in confusing ways when there's an export
			-- conflict.
			, checkPresentCheap = False
			, mkUnavailable = return Nothing
			, getInfo = do
				ts <- map fromRef . exportedTreeishes
					<$> getExport (uuid r)
				is <- getInfo r
				return (is++[("export", "yes"), ("exportedtree", unwords ts)])
			}
	retrieveKeyFileFromExport getexportlocs exportinconflict k _af dest p = unVerified $
		if maybe False (isJust . verifyKeyContent) (maybeLookupBackendVariety (keyVariety k))
			then do
				locs <- getexportlocs k
				case locs of
					[] -> do
						ifM (liftIO $ atomically $ readTVar exportinconflict)
							( warning "unknown export location, likely due to the export conflict"
							, warning "unknown export location"
							)
						return False
					(l:_) -> retrieveExport (exportActions r) k l dest p
			else do
				warning $ "exported content cannot be verified due to using the " ++ decodeBS (formatKeyVariety (keyVariety k)) ++ " backend"
				return False
