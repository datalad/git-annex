{- git-annex backend varieties
 -
 - Copyright 2012-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Backend.Variety where

import qualified Data.Map as M

import Annex.Common
import Types.Key
import Types.Backend
import qualified Backend.External

-- When adding a new backend, import it here and add it to the builtinList.
import qualified Backend.Hash
import qualified Backend.WORM
import qualified Backend.URL
import qualified Backend.GitRemoteAnnex

{- Regular backends. Does not include externals or VURL. -}
regularBackendList :: [Backend]
regularBackendList = Backend.Hash.backends 
	++ Backend.WORM.backends 
	++ Backend.URL.backends
	++ Backend.GitRemoteAnnex.backends

{- The default hashing backend. -}
defaultHashBackend :: Backend
defaultHashBackend = Prelude.head regularBackendList

makeVarietyMap :: [Backend] -> M.Map KeyVariety Backend
makeVarietyMap l = M.fromList $ zip (map backendVariety l) l

maybeLookupBackendVarietyMap :: KeyVariety -> M.Map KeyVariety Backend -> Annex (Maybe Backend)
maybeLookupBackendVarietyMap (ExternalKey s hasext) _varitymap =
	Just <$> Backend.External.makeBackend s hasext
maybeLookupBackendVarietyMap v varietymap = 
	pure $ M.lookup v varietymap
