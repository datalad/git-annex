{- Helper to make remotes support export and import (or not).
 -
 - Copyright 2017-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Remote.Helper.ExportImport where

import Annex.Common
import Types.Remote
import Types.Key
import Types.ProposedAccepted
import Backend
import Remote.Helper.Encryptable (encryptionIsEnabled)
import qualified Database.Export as Export
import qualified Database.ContentIdentifier as ContentIdentifier
import Annex.Export
import Annex.LockFile
import Annex.SpecialRemote.Config
import Git.Types (fromRef)
import Logs.Export
import Logs.ContentIdentifier (recordContentIdentifier)

import Control.Concurrent.STM

-- | Use for remotes that do not support exports.
class HasExportUnsupported a where
	exportUnsupported :: a

instance HasExportUnsupported (ParsedRemoteConfig -> RemoteGitConfig -> Annex Bool) where
	exportUnsupported = \_ _ -> return False

instance HasExportUnsupported (ExportActions Annex) where
	exportUnsupported = ExportActions
		{ storeExport = nope
		, retrieveExport = nope
		, checkPresentExport = \_ _ -> return False
		, removeExport = nope
		, removeExportDirectory = nope
		, renameExport = \_ _ _ -> return Nothing
		}
	 where
	 	nope = giveup "export not supported"

-- | Use for remotes that do not support imports.
class HasImportUnsupported a where
	importUnsupported :: a

instance HasImportUnsupported (ParsedRemoteConfig -> RemoteGitConfig -> Annex Bool) where
	importUnsupported = \_ _ -> return False

instance HasImportUnsupported (ImportActions Annex) where
	importUnsupported = ImportActions
		{ listImportableContents = return Nothing
		, importKey = Nothing
		, retrieveExportWithContentIdentifier = nope
		, storeExportWithContentIdentifier = nope
		, removeExportWithContentIdentifier = nope
		, removeExportDirectoryWhenEmpty = nope
		, checkPresentExportWithContentIdentifier = \_ _ _ -> return False
		}
	  where
		nope = giveup "import not supported"

exportIsSupported :: ParsedRemoteConfig -> RemoteGitConfig -> Annex Bool
exportIsSupported = \_ _ -> return True

importIsSupported :: ParsedRemoteConfig -> RemoteGitConfig -> Annex Bool
importIsSupported = \_ _ -> return True

-- | Prevent or allow exporttree=yes and importtree=yes when
-- setting up a new remote, depending on exportSupported and importSupported.
adjustExportImportRemoteType :: RemoteType -> RemoteType
adjustExportImportRemoteType rt = rt { setup = setup' }
  where
	setup' st mu cp c gc = do
		pc <- either giveup return . parseRemoteConfig c
			=<< configParser rt c
		let checkconfig supported configured configfield cont =
			ifM (supported rt pc gc)
				( case st of
					Init
						| configured pc && encryptionIsEnabled pc ->
							giveup $ "cannot enable both encryption and " ++ fromProposedAccepted configfield
						| otherwise -> cont
					Enable oldc -> do
						oldpc <- parsedRemoteConfig rt oldc
						if configured pc /= configured oldpc
							then giveup $ "cannot change " ++ fromProposedAccepted configfield ++ " of existing special remote"
							else cont
				, if configured pc
					then giveup $ fromProposedAccepted configfield ++ " is not supported by this special remote"
					else cont
				)
		checkconfig exportSupported exportTree exportTreeField $
			checkconfig importSupported importTree importTreeField $
				setup rt st mu cp c gc

-- | Adjust a remote to support exporttree=yes and/or importree=yes.
adjustExportImport :: Remote -> RemoteStateHandle -> Annex Remote
adjustExportImport rmt rs = do
	dbv <- prepdbv
	case (exportTree (config rmt), importTree (config rmt)) of
		(True, True) -> isimport dbv =<< isexport dbv rmt
		(True, False) -> notimport =<< isexport dbv rmt
		(False, True) -> notexport =<< isimport dbv rmt
		(False, False) -> notimport =<< notexport rmt
  where
	notexport r = return $ r
		{ exportActions = exportUnsupported
		, remotetype = (remotetype r)
			{ exportSupported = exportUnsupported
			}
		}
	
	notimport r = return $ r
		{ importActions = importUnsupported
		, remotetype = (remotetype r)
			{ importSupported = importUnsupported
			}
		}
	
	isimport dbv r = do
		ciddbv <- prepciddb

		let keycids k = do
			db <- getciddb ciddbv
			liftIO $ ContentIdentifier.getContentIdentifiers db rs k

		let checkpresent k loc = 
			checkPresentExportWithContentIdentifier
				(importActions r)
				k loc 
				=<< keycids k

		return $ r
			{ exportActions = (exportActions r)
				{ storeExport = \f k loc p -> do
					db <- getciddb ciddbv
					exportdb <- getexportdb r dbv
					oldks <- liftIO $ Export.getExportTreeKey exportdb loc
					oldcids <- liftIO $ concat
						<$> mapM (ContentIdentifier.getContentIdentifiers db rs) oldks
					newcid <- storeExportWithContentIdentifier (importActions r) f k loc oldcids p
					withExclusiveLock gitAnnexContentIdentifierLock $ do
						liftIO $ ContentIdentifier.recordContentIdentifier db rs newcid k
						liftIO $ ContentIdentifier.flushDbQueue db
					recordContentIdentifier rs newcid k
				, removeExport = \k loc ->
					removeExportWithContentIdentifier (importActions r) k loc
						=<< keycids k
				, removeExportDirectory = removeExportDirectoryWhenEmpty (importActions r)
				-- renameExport is optional, and the
				-- remote's implementation may
				-- lose modifications to the file
				-- (by eg copying and then deleting)
				-- so don't use it
				, renameExport = \_ _ _ -> return Nothing
				, checkPresentExport = checkpresent
				}
			, checkPresent = if appendonly r
				then checkPresent r
				else \k -> anyM (checkpresent k)
					=<< getexportlocs r dbv k
			, getInfo = do
				is <- getInfo r
				return (is++[("import", "yes")])
			}
	
	isexport dbv r = ifM (isExportSupported r)
		( isexport' dbv r
		, notexport r
		)

	isexport' dbv r = return $ r
		-- Storing a key on an export could be implemented,
		-- but it would perform unncessary work
		-- when another repository has already stored the
		-- key, and the local repository does not know
		-- about it. To avoid unnecessary costs, don't do it.
		{ storeKey = \_ _ _ ->
			giveup "remote is configured with exporttree=yes; use `git-annex export` to store content on it"
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
			let retrieveexport = retrieveKeyFileFromExport r dbv k af dest p
			in if appendonly r
				then retrieveKeyFile r k af dest p
					`catchNonAsync` const retrieveexport
				else retrieveexport
		, retrieveKeyFileCheap = if appendonly r
			then retrieveKeyFileCheap r
			else Nothing
		-- Removing a key from an export would need to
		-- change the tree in the export log to not include
		-- the file. Otherwise, conflicts when removing
		-- files would not be dealt with correctly.
		-- There does not seem to be a good use case for
		-- removing a key from an export in any case.
		, removeKey = \_k -> giveup "dropping content from an export is not supported; use `git annex export` to export a tree that lacks the files you want to remove"
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
				=<< getexportlocs r dbv k
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

	prepciddb = do
		lcklckv <- liftIO newEmptyTMVarIO
		dbtv <- liftIO newEmptyTMVarIO
		return (dbtv, lcklckv)
	
	prepdbv = do
		lcklckv <- liftIO newEmptyTMVarIO
		dbv <- liftIO newEmptyTMVarIO
		exportinconflict <- liftIO $ newTVarIO False
		return (dbv, lcklckv, exportinconflict)

	-- Only open the database once it's needed.
	getciddb (dbtv, lcklckv) =
		liftIO (atomically (tryReadTMVar dbtv)) >>= \case
			Just db -> return db
			-- let only one thread take the lock
			Nothing -> ifM (liftIO $ atomically $ tryPutTMVar lcklckv ())
				( do
					db <- ContentIdentifier.openDb
					ContentIdentifier.needsUpdateFromLog db >>= \case
						Just v -> withExclusiveLock gitAnnexContentIdentifierLock $
							ContentIdentifier.updateFromLog db v
						Nothing -> noop
					liftIO $ atomically $ putTMVar dbtv db
					return db
				-- loser waits for winner to open the db and
				-- can then also use its handle
				, liftIO $ atomically (readTMVar dbtv)
				)
	
	-- Only open the database once it's needed.
	--
	-- After opening the database, check if the export log is
	-- different than the database, and update the database, to notice
	-- when an export has been updated from another repository.
	getexportdb r (dbv, lcklckv, exportinconflict) = 
		liftIO (atomically (tryReadTMVar dbv)) >>= \case
			Just db -> return db
			-- let only one thread take the lock
			Nothing -> ifM (liftIO $ atomically $ tryPutTMVar lcklckv ())
				( do
					db <- Export.openDb (uuid r)
					updateexportdb db exportinconflict r
					liftIO $ atomically $ putTMVar dbv db
					return db
				-- loser waits for winner to open the db and
				-- can then also use its handle
				, liftIO $ atomically (readTMVar dbv)
				)

	getexportinconflict (_, _, v) = v

	updateexportdb db exportinconflict r =
		Export.updateExportTreeFromLog db >>= \case
			Export.ExportUpdateSuccess -> return ()
			Export.ExportUpdateConflict -> do
				warnExportImportConflict r
				liftIO $ atomically $
					writeTVar exportinconflict True
		
	getexportlocs r dbv k = do
		db <- getexportdb r dbv
		liftIO $ Export.getExportTree db k

	retrieveKeyFileFromExport r dbv k _af dest p = ifM (isVerifiable k)
		( do
			locs <- getexportlocs r dbv k
			case locs of
				[] -> ifM (liftIO $ atomically $ readTVar $ getexportinconflict dbv)
					( giveup "unknown export location, likely due to the export conflict"
					, giveup "unknown export location"
					)
				(l:_) -> do
					retrieveExport (exportActions r) k l dest p
					return UnVerified
		, giveup $ "exported content cannot be verified due to using the " ++ decodeBS (formatKeyVariety (fromKey keyVariety k)) ++ " backend"
		)
