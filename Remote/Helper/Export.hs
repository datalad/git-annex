{- exports to remotes
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Export where

import Annex.Common
import Types.Remote
import Types.Creds
import Remote.Helper.Encryptable (isEncrypted)

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
exportableRemote :: Remote -> Remote
exportableRemote r = case M.lookup "exporttree" (config r) of
	Just "yes" -> r
		{ storeKey = \_ _ _ -> do
			warning "remote is configured with exporttree=yes; use `git-annex export` to store content on it"
			return False
		}
	_ -> r
		{ exportActions = exportUnsupported }

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
