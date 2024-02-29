{- git-annex key/value backends
 -
 - Copyright 2010-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Backend (
	builtinList,
	defaultBackend,
	defaultHashBackend,
	genKey,
	getBackend,
	chooseBackend,
	lookupBackendVariety,
	lookupBuiltinBackendVariety,
	maybeLookupBackendVariety,
	isStableKey,
	isCryptographicallySecure,
	isCryptographicallySecure',
) where

import Annex.Common
import qualified Annex
import Annex.CheckAttr
import Types.Key
import Types.KeySource
import qualified Types.Backend as B
import Utility.Metered

-- When adding a new backend, import it here and add it to the builtinList.
import qualified Backend.Hash
import qualified Backend.WORM
import qualified Backend.URL
import qualified Backend.External

import qualified Data.Map as M

{- Built-in backends. Does not include externals. -}
builtinList :: [Backend]
builtinList = Backend.Hash.backends ++ Backend.WORM.backends ++ Backend.URL.backends

{- The default hashing backend. This must use a cryptographically secure
 - hash. -}
defaultHashBackend :: Backend
defaultHashBackend = Prelude.head builtinList

{- Backend to use by default when generating a new key. Takes git config
 - and --backend option into account. -}
defaultBackend :: Annex Backend
defaultBackend = maybe cache return =<< Annex.getState Annex.backend
  where
	cache = do
		n <- maybe (annexBackend <$> Annex.getGitConfig) (return . Just)
			=<< Annex.getRead Annex.forcebackend
		b <- case n of
			Just name | valid name -> lookupname name
			_ -> pure defaultHashBackend
		Annex.changeState $ \s -> s { Annex.backend = Just b }
		return b
	valid name = not (null name)
	lookupname = lookupBackendVariety . parseKeyVariety . encodeBS

{- Generates a key for a file. -}
genKey :: KeySource -> MeterUpdate -> Backend -> Annex (Key, Backend)
genKey source meterupdate b = case B.genKey b of
	Just a -> do
		k <- a source meterupdate
		return (k, b)
	Nothing -> giveup $ "Cannot generate a key for backend " ++
		decodeBS (formatKeyVariety (B.backendVariety b))

getBackend :: FilePath -> Key -> Annex (Maybe Backend)
getBackend file k = maybeLookupBackendVariety (fromKey keyVariety k) >>= \case
	Just backend -> return $ Just backend
	Nothing -> do
		warning $ "skipping " <> QuotedPath (toRawFilePath file) <> " (" <>
			UnquotedString (unknownBackendVarietyMessage (fromKey keyVariety k)) <> ")"
		return Nothing

unknownBackendVarietyMessage :: KeyVariety -> String
unknownBackendVarietyMessage v =
	"unknown backend " ++ decodeBS (formatKeyVariety v)

{- Looks up the backend that should be used for a file.
 - That can be configured on a per-file basis in the gitattributes file,
 - or forced with --backend. -}
chooseBackend :: RawFilePath -> Annex Backend
chooseBackend f = Annex.getRead Annex.forcebackend >>= go
  where
	go Nothing = do
		mb <- maybeLookupBackendVariety . parseKeyVariety . encodeBS
			=<< checkAttr "annex.backend" f
		case mb of
			Just b -> return b
			Nothing -> defaultBackend
	go (Just _) = defaultBackend

{- Looks up a backend by variety. May fail if unsupported or disabled. -}
lookupBackendVariety :: KeyVariety -> Annex Backend
lookupBackendVariety v = fromMaybe (giveup (unknownBackendVarietyMessage v))
	<$> maybeLookupBackendVariety v

lookupBuiltinBackendVariety :: KeyVariety -> Backend
lookupBuiltinBackendVariety v = fromMaybe (giveup (unknownBackendVarietyMessage v)) $
	maybeLookupBuiltinBackendVariety v

maybeLookupBackendVariety :: KeyVariety -> Annex (Maybe Backend)
maybeLookupBackendVariety (ExternalKey s hasext) =
	Just <$> Backend.External.makeBackend s hasext
maybeLookupBackendVariety v = 
	pure $ M.lookup v varietyMap

maybeLookupBuiltinBackendVariety :: KeyVariety -> Maybe Backend
maybeLookupBuiltinBackendVariety v = M.lookup v varietyMap

varietyMap :: M.Map KeyVariety Backend
varietyMap = M.fromList $ zip (map B.backendVariety builtinList) builtinList

isStableKey :: Key -> Annex Bool
isStableKey k = maybe False (`B.isStableKey` k) 
	<$> maybeLookupBackendVariety (fromKey keyVariety k)

isCryptographicallySecure :: Key -> Annex Bool
isCryptographicallySecure k = maybe False isCryptographicallySecure'
	<$> maybeLookupBackendVariety (fromKey keyVariety k)

isCryptographicallySecure' :: Backend -> Bool
isCryptographicallySecure' = B.isCryptographicallySecure
