{- git-annex key/value backends
 -
 - Copyright 2010-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Backend (
	list,
	defaultBackend,
	genKey,
	getBackend,
	chooseBackend,
	lookupBackendVariety,
	maybeLookupBackendVariety,
	isStableKey,
) where

import Annex.Common
import qualified Annex
import Annex.CheckAttr
import Types.Key
import Types.KeySource
import qualified Types.Backend as B
import Utility.Metered

-- When adding a new backend, import it here and add it to the list.
import qualified Backend.Hash
import qualified Backend.WORM
import qualified Backend.URL

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S8

list :: [Backend]
list = Backend.Hash.backends ++ Backend.WORM.backends ++ Backend.URL.backends

{- Backend to use by default when generating a new key. -}
defaultBackend :: Annex Backend
defaultBackend = maybe cache return =<< Annex.getState Annex.backend
  where
	cache = do
		n <- maybe (annexBackend <$> Annex.getGitConfig) (return . Just)
			=<< Annex.getState Annex.forcebackend
		let b = case n of
			Just name | valid name -> lookupname name
			_ -> Prelude.head list
		Annex.changeState $ \s -> s { Annex.backend = Just b }
		return b
	valid name = not (null name)
	lookupname = lookupBackendVariety . parseKeyVariety . encodeBS

{- Generates a key for a file. -}
genKey :: KeySource -> MeterUpdate -> Maybe Backend -> Annex (Maybe (Key, Backend))
genKey source meterupdate preferredbackend = do
	b <- maybe defaultBackend return preferredbackend
	B.getKey b source meterupdate >>= return . \case
		Nothing -> Nothing
		Just k -> Just (makesane k, b)
  where
	-- keyNames should not contain newline characters.
	makesane k = k { keyName = S8.map fixbadchar (keyName k) }
	fixbadchar c
		| c == '\n' = '_'
		| otherwise = c

getBackend :: FilePath -> Key -> Annex (Maybe Backend)
getBackend file k = case maybeLookupBackendVariety (keyVariety k) of
	Just backend -> return $ Just backend
	Nothing -> do
		warning $ "skipping " ++ file ++ " (unknown backend " ++ decodeBS (formatKeyVariety (keyVariety k)) ++ ")"
		return Nothing

{- Looks up the backend that should be used for a file.
 - That can be configured on a per-file basis in the gitattributes file,
 - or forced with --backend. -}
chooseBackend :: FilePath -> Annex (Maybe Backend)
chooseBackend f = Annex.getState Annex.forcebackend >>= go
  where
	go Nothing = maybeLookupBackendVariety . parseKeyVariety . encodeBS
		<$> checkAttr "annex.backend" f
	go (Just _) = Just <$> defaultBackend

{- Looks up a backend by variety. May fail if unsupported or disabled. -}
lookupBackendVariety :: KeyVariety -> Backend
lookupBackendVariety v = fromMaybe unknown $ maybeLookupBackendVariety v
  where
	unknown = giveup $ "unknown backend " ++ decodeBS (formatKeyVariety v)

maybeLookupBackendVariety :: KeyVariety -> Maybe Backend
maybeLookupBackendVariety v = M.lookup v varietyMap

varietyMap :: M.Map KeyVariety Backend
varietyMap = M.fromList $ zip (map B.backendVariety list) list

isStableKey :: Key -> Bool
isStableKey k = maybe False (`B.isStableKey` k) 
	(maybeLookupBackendVariety (keyVariety k))
