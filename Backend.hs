{- git-annex key/value backends
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Backend (
	builtinList,
	defaultBackend,
	genKey,
	getBackend,
	chooseBackend,
	lookupBackendVariety,
	lookupBuiltinBackendVariety,
	maybeLookupBackendVariety,
	isStableKey,
	isCryptographicallySecure,
	isVerifiable,
	startVerifyKeyContentIncrementally,
) where

import Annex.Common
import qualified Annex
import Annex.CheckAttr
import Annex.Verify
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
import qualified Data.ByteString.Char8 as S8

{- Build-in backends. Does not include externals. -}
builtinList :: [Backend]
builtinList = Backend.Hash.backends ++ Backend.WORM.backends ++ Backend.URL.backends

{- Backend to use by default when generating a new key. -}
defaultBackend :: Annex Backend
defaultBackend = maybe cache return =<< Annex.getState Annex.backend
  where
	cache = do
		n <- maybe (annexBackend <$> Annex.getGitConfig) (return . Just)
			=<< Annex.getState Annex.forcebackend
		b <- case n of
			Just name | valid name -> lookupname name
			_ -> pure (Prelude.head builtinList)
		Annex.changeState $ \s -> s { Annex.backend = Just b }
		return b
	valid name = not (null name)
	lookupname = lookupBackendVariety . parseKeyVariety . encodeBS

{- Generates a key for a file. -}
genKey :: KeySource -> MeterUpdate -> Maybe Backend -> Annex (Key, Backend)
genKey source meterupdate preferredbackend = do
	b <- maybe defaultBackend return preferredbackend
	case B.genKey b of
		Just a -> do
			k <- a source meterupdate
			return (makesane k, b)
		Nothing -> giveup $ "Cannot generate a key for backend " ++
			decodeBS (formatKeyVariety (B.backendVariety b))
  where
	-- keyNames should not contain newline characters.
	makesane k = alterKey k $ \d -> d
		{ keyName = S8.map fixbadchar (fromKey keyName k)
		}
	fixbadchar c
		| c == '\n' = '_'
		| otherwise = c

getBackend :: FilePath -> Key -> Annex (Maybe Backend)
getBackend file k = maybeLookupBackendVariety (fromKey keyVariety k) >>= \case
	Just backend -> return $ Just backend
	Nothing -> do
		warning $ "skipping " ++ file ++ " (" ++ unknownBackendVarietyMessage (fromKey keyVariety k) ++ ")"
		return Nothing

unknownBackendVarietyMessage :: KeyVariety -> String
unknownBackendVarietyMessage v =
	"unknown backend " ++ decodeBS (formatKeyVariety v)

{- Looks up the backend that should be used for a file.
 - That can be configured on a per-file basis in the gitattributes file,
 - or forced with --backend. -}
chooseBackend :: RawFilePath -> Annex (Maybe Backend)
chooseBackend f = Annex.getState Annex.forcebackend >>= go
  where
	go Nothing = maybeLookupBackendVariety . parseKeyVariety . encodeBS
		=<< checkAttr "annex.backend" f
	go (Just _) = Just <$> defaultBackend

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
isCryptographicallySecure k = maybe False (`B.isCryptographicallySecure` k)
	<$> maybeLookupBackendVariety (fromKey keyVariety k)

isVerifiable :: Key -> Annex Bool
isVerifiable k = maybe False (isJust . B.verifyKeyContent) 
	<$> maybeLookupBackendVariety (fromKey keyVariety k)

startVerifyKeyContentIncrementally :: VerifyConfig -> Key -> Annex (Maybe B.IncrementalVerifier)
startVerifyKeyContentIncrementally verifyconfig k = 
	ifM (shouldVerify verifyconfig)
		( maybeLookupBackendVariety (fromKey keyVariety k) >>= \case
			Just b -> case B.verifyKeyContentIncrementally b of
				Just v -> Just <$> v k
				Nothing -> return Nothing
			Nothing -> return Nothing
		, return Nothing
		)
