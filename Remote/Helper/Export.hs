{- exports to remotes
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Export where

import Annex.Common
import qualified Annex
import Types.Remote
import Types.Creds
import Types.Backend
import Types.Key
import Types.TrustLevel
import Backend
import Remote.Helper.Encryptable (isEncrypted)
import Database.Export

import qualified Data.Map as M

-- | Use for remotes that do not support exports.
exportUnsupported :: ExportActions Annex
exportUnsupported = ExportActions
	{ exportSupported = return False
	, storeExport = \_ _ _ _ -> return False
	, retrieveExport = \_ _ _ _ -> return (False, UnVerified)
	, removeExport = \_ _ -> return False
	, checkPresentExport = \_ _ -> return False
	, renameExport = \_ _ _ -> return False
	}

-- | A remote that supports exports when configured with exporttree=yes,
-- and otherwise does not.
exportableRemote :: Remote -> Annex (Maybe Remote)
exportableRemote r = case M.lookup "exporttree" (config r) of
	Just "yes" -> do
		db <- openDb (uuid r)

		return $ Just $ r
			-- Storing a key on an export would need a way to
			-- look up the file(s) that the currently exported
			-- tree uses for a key; there's not currently an
			-- inexpensive way to do that (getExportLocation
			-- only finds files that have been stored on the
			-- export already).
			{ storeKey = \_ _ _ -> do
				warning "remote is configured with exporttree=yes; use `git-annex export` to store content on it"
				return False
			-- Keys can be retrieved, but since an export
			-- is not a true key/value store, the content of
			-- the key has to be able to be strongly verified.
			, retrieveKeyFile = \k _af dest p ->
				if maybe False (isJust . verifyKeyContent) (maybeLookupBackendVariety (keyVariety k))
					then do
						locs <- liftIO $ getExportLocation db k
						case locs of
							[] -> do
								warning "unknown export location"
								return (False, UnVerified)
							(l:_) -> retrieveExport (exportActions r) k l dest p
					else do
						warning $ "exported content cannot be verified due to using the " ++ formatKeyVariety (keyVariety k) ++ " backend"
						return (False, UnVerified)
			, retrieveKeyFileCheap = \_ _ _ -> return False
			-- Remove all files a key was exported to.
			, removeKey = \k -> do
				locs <- liftIO $ getExportLocation db k
				oks <- forM locs $ \loc -> do
					ok <- removeExport (exportActions r) k loc
					when ok $
						liftIO $ removeExportLocation db k loc
					return ok
				liftIO $ flushDbQueue db
				return (and oks)
			-- Can't lock content on exports, since they're
			-- not key/value stores, and someone else could
			-- change what's exported to a file at any time.
			, lockContent = Nothing
			-- Check if any of the files a key was exported
			-- to are present. This doesn't guarantee the
			-- export contains the right content.
			, checkPresent = \k ->
				anyM (checkPresentExport (exportActions r) k)
					=<< liftIO (getExportLocation db k)
			}
	_ -> return $ Just $ r { exportActions = exportUnsupported }

exportableRemoteSetup :: (SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)) -> SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
exportableRemoteSetup setupaction st mu cp c gc = case st of
	Init -> case M.lookup "exporttree" c of
		Just "yes" | isEncrypted c ->
			giveup "cannot enable both encryption and exporttree"
		_ -> cont
	Enable oldc
		| M.lookup "exporttree" c /= M.lookup "exporttree" oldc ->
			giveup "cannot change exporttree of existing special remote"
		| otherwise -> cont
  where
	cont = setupaction st mu cp c gc
