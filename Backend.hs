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
	isCryptographicallySecureKey,
	isCryptographicallySecure,
) where

import qualified Data.Map as M

import Annex.Common
import qualified Annex
import Annex.CheckAttr
import Types.Key
import Types.KeySource
import qualified Types.Backend as B
import Utility.Metered
import Backend.Variety
import qualified Backend.VURL

{- Built-in backends. Does not include externals. -}
builtinList :: [Backend]
builtinList = regularBackendList ++ Backend.VURL.backends

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

getBackend :: OsPath -> Key -> Annex (Maybe Backend)
getBackend file k = maybeLookupBackendVariety (fromKey keyVariety k) >>= \case
	Just backend -> return $ Just backend
	Nothing -> do
		warning $ "skipping " <> QuotedPath file <> " (" <>
			UnquotedString (unknownBackendVarietyMessage (fromKey keyVariety k)) <> ")"
		return Nothing

unknownBackendVarietyMessage :: KeyVariety -> String
unknownBackendVarietyMessage v =
	"unknown backend " ++ decodeBS (formatKeyVariety v)

{- Looks up the backend that should be used for a file.
 - That can be configured on a per-file basis in the gitattributes file,
 - or forced with --backend. -}
chooseBackend :: OsPath -> Annex Backend
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

maybeLookupBuiltinBackendVariety :: KeyVariety -> Maybe Backend
maybeLookupBuiltinBackendVariety v = M.lookup v varietyMap

maybeLookupBackendVariety :: KeyVariety -> Annex (Maybe Backend)
maybeLookupBackendVariety v = maybeLookupBackendVarietyMap v varietyMap

varietyMap :: M.Map KeyVariety Backend
varietyMap = makeVarietyMap builtinList

isStableKey :: Key -> Annex Bool
isStableKey k = maybe False (`B.isStableKey` k) 
	<$> maybeLookupBackendVariety (fromKey keyVariety k)

isCryptographicallySecureKey :: Key -> Annex Bool
isCryptographicallySecureKey k = maybe 
	(pure False)
	(\b -> B.isCryptographicallySecureKey b k)
	=<< maybeLookupBackendVariety (fromKey keyVariety k)

isCryptographicallySecure :: Backend -> Bool
isCryptographicallySecure = B.isCryptographicallySecure
