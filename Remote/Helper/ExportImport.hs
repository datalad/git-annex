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
		{ listImportableContents = nope
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
-- setting up a new remote, depending on the remote's capabilities.
adjustExportImportRemoteType :: RemoteType -> RemoteType
adjustExportImportRemoteType rt = rt { setup = setup' }
  where
	setup' st mu cp c gc = do
		pc <- either giveup return . parseRemoteConfig c
			=<< configParser rt c
		let checkconfig supported configured configfield cont =
			ifM (supported rt pc gc <&&> pure (not (thirdPartyPopulated rt)))
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
adjustExportImport r rs = do
	isexport <- pure (exportTree (config r))
		<&&> isExportSupported r
	-- When thirdPartyPopulated is True, the remote
	-- does not need to be configured with importTree to support
	-- imports.
	isimport <- pure (importTree (config r) || thirdPartyPopulated (remotetype r))
		<&&> isImportSupported r
	let r' = r
		{ remotetype = (remotetype r)
			{ exportSupported = if isexport
				then exportSupported (remotetype r)
				else exportUnsupported
			, importSupported = if isimport
				then importSupported (remotetype r)
				else importUnsupported
			}
		}
	if not isexport && not isimport
		then return r'
		else adjustExportImport' isexport isimport r' rs

adjustExportImport' :: Bool -> Bool -> Remote -> RemoteStateHandle -> Annex Remote
adjustExportImport' isexport isimport r rs = do
	dbv <- prepdbv
	ciddbv <- prepciddb
	let normal = not isexport && not isimport
	let iskeyvaluestore = normal || appendonly r
	return $ r
		{ exportActions = if isexport
			then if isimport
				then exportActionsForImport dbv ciddbv (exportActions r)
				else exportActions r
			else exportUnsupported
		, importActions = if isimport
			then importActions r
			else importUnsupported
		, storeKey = \k af p ->
			-- Storing a key on an export could be implemented,
			-- but it would perform unncessary work
			-- when another repository has already stored the
			-- key, and the local repository does not know
			-- about it. To avoid unnecessary costs, don't do it.
			if mergeable
				then if isexport
					then giveup "remote is configured with exporttree=yes; use `git-annex export` to store content on it"
					else if isimport
						then giveup "remote is configured with importtree=yes and without exporttree=yes; cannot modify content stored on it"
						else storeKey r k af p
			else storeKey r k af p
		, removeKey = \k -> 
			-- Removing a key from an export would need to
			-- change the tree in the export log to not include
			-- the file. Otherwise, conflicts when removing
			-- files would not be dealt with correctly.
			-- There does not seem to be a good use case for
			-- removing a key from an export in any case.
			if mergeable
				then if isexport
					then giveup "dropping content from an export is not supported; use `git annex export` to export a tree that lacks the files you want to remove"
					else if isimport
						then giveup "dropping content from this remote is not supported because it is configured with importtree=yes"
						else removeKey r k
				else removeKey r k
		, lockContent = if iskeyvaluestore || not mergeable
			then lockContent r
			else Nothing
		, retrieveKeyFile = \k af dest p ->
			if isimport
				then supportappendonlyretrieve k af dest p $
					retrieveKeyFileFromImport dbv ciddbv k af dest p
				else if isexport
					then supportappendonlyretrieve k af dest p $
						retrieveKeyFileFromExport dbv k af dest p
					else retrieveKeyFile r k af dest p
		, retrieveKeyFileCheap = if iskeyvaluestore
			then retrieveKeyFileCheap r
			else Nothing
		, checkPresent = \k -> if appendonly r
			then checkPresent r k
			else if isimport
				then anyM (checkPresentImport ciddbv k)
					=<< getexportlocs dbv k
				else if isexport
					-- Check if any of the files a key
					-- was exported to are present. This 
					-- doesn't guarantee the export
					-- contains the right content,
					-- if the remote is an export,
					-- or if something else can write
					-- to it. Remotes that have such 
					-- problems are made untrusted,
					-- so it's not worried about here.
					then anyM (checkPresentExport (exportActions r) k)
						=<< getexportlocs dbv k
					else checkPresent r k
		-- checkPresent from an export is more expensive
		-- than otherwise, so not cheap. Also, this
		-- avoids things that look at checkPresentCheap and
		-- silently skip non-present files from behaving
		-- in confusing ways when there's an export
		-- conflict (or an import conflict).
		, checkPresentCheap = if normal
			then checkPresentCheap r
			else False
		-- git-annex testremote cannot be used to test
		-- import/export since it stores keys.
		, mkUnavailable = if normal
			then mkUnavailable r
			else return Nothing
		, getInfo = do
			is <- getInfo r
			is' <- if isexport && not mergeable
				then do
					ts <- map fromRef . exportedTreeishes
						<$> getExport (uuid r)
					return (is++[("exporttree", "yes"), ("exportedtree", unwords ts)])
				else return is
			return $ if isimport && not mergeable
				then (is'++[("importtree", "yes")])
				else is'
		}
  where
	-- When a remote is populated by a third party, a tree can be
	-- imported from it, but that tree is not mergeable into the
	-- user's own git branch. But annex objects found in the tree
	-- (identified by importKey) can still be retrieved from the remote.
	mergeable = thirdPartyPopulated (remotetype r)

	-- exportActions adjusted to use the equivilant import actions,
	-- which take ContentIdentifiers into account.
	exportActionsForImport dbv ciddbv ea = ea
  		{ storeExport = \f k loc p -> do
			db <- getciddb ciddbv
			exportdb <- getexportdb dbv
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
				=<< getkeycids ciddbv k
		, removeExportDirectory = removeExportDirectoryWhenEmpty (importActions r)
		-- renameExport is optional, and the remote's
		-- implementation may lose modifications to the file
		-- (by eg copying and then deleting) so don't use it
		, renameExport = \_ _ _ -> return Nothing
		, checkPresentExport = checkPresentImport ciddbv
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
	getexportdb (dbv, lcklckv, exportinconflict) = 
		liftIO (atomically (tryReadTMVar dbv)) >>= \case
			Just db -> return db
			-- let only one thread take the lock
			Nothing -> ifM (liftIO $ atomically $ tryPutTMVar lcklckv ())
				( do
					db <- Export.openDb (uuid r)
					updateexportdb db exportinconflict
					liftIO $ atomically $ putTMVar dbv db
					return db
				-- loser waits for winner to open the db and
				-- can then also use its handle
				, liftIO $ atomically (readTMVar dbv)
				)

	getexportinconflict (_, _, v) = v

	updateexportdb db exportinconflict =
		Export.updateExportTreeFromLog db >>= \case
			Export.ExportUpdateSuccess -> return ()
			Export.ExportUpdateConflict -> do
				warnExportImportConflict r
				liftIO $ atomically $
					writeTVar exportinconflict True
		
	getexportlocs dbv k = do
		db <- getexportdb dbv
		liftIO $ Export.getExportTree db k
	
	getfirstexportloc dbv k = do
		db <- getexportdb dbv
		liftIO $ Export.getExportTree db k >>= \case
			[] -> ifM (atomically $ readTVar $ getexportinconflict dbv)
				( giveup "unknown export location, likely due to the export conflict"
				, giveup "unknown export location"
				)
			(l:_) -> return l
		
	getkeycids ciddbv k = do
		db <- getciddb ciddbv
		liftIO $ ContentIdentifier.getContentIdentifiers db rs k

	-- Keys can be retrieved using retrieveExport, but since that
	-- retrieves from a path in the remote that another writer could
	-- have replaced with content not of the requested key, the content
	-- has to be strongly verified.
	retrieveKeyFileFromExport dbv k _af dest p = ifM (isVerifiable k)
		( do
			l <- getfirstexportloc dbv k
			retrieveExport (exportActions r) k l dest p
			return MustVerify
		, giveup $ "exported content cannot be verified due to using the " ++ decodeBS (formatKeyVariety (fromKey keyVariety k)) ++ " backend"
		)
	
	retrieveKeyFileFromImport dbv ciddbv k af dest p =
		getkeycids ciddbv k >>= \case
			(cid:_) -> do
				l <- getfirstexportloc dbv k
				void $ retrieveExportWithContentIdentifier (importActions r) l cid dest (pure k) p
				return UnVerified
			-- In case a content identifier is somehow missing,
			-- try this instead.
			[] -> if isexport
				then retrieveKeyFileFromExport dbv k af dest p
				else giveup "no content identifier is recorded, unable to retrieve"
	
	-- appendonly remotes have a key/value store, so can use
	-- the usual retrieveKeyFile, rather than an import/export
	-- variant. However, fall back to that if retrieveKeyFile fails.
	supportappendonlyretrieve k af dest p a
		| appendonly r =
			retrieveKeyFile r k af dest p
				`catchNonAsync` const a
		| otherwise = a

	checkPresentImport ciddbv k loc =
		checkPresentExportWithContentIdentifier
			(importActions r)
			k loc 
			=<< getkeycids ciddbv k
