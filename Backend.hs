{- git-annex key/value backends
 -
 - Copyright 2010-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend (
	list,
	orderedList,
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

-- When adding a new backend, import it here and add it to the list.
import qualified Backend.Hash
import qualified Backend.WORM
import qualified Backend.URL

import qualified Data.Map as M

list :: [Backend]
list = Backend.Hash.backends ++ Backend.WORM.backends ++ Backend.URL.backends

{- List of backends in the order to try them when storing a new key. -}
orderedList :: Annex [Backend]
orderedList = do
	l <- Annex.getState Annex.backends -- list is cached here
	if not $ null l
		then return l
		else do
			f <- Annex.getState Annex.forcebackend
			case f of
				Just name | not (null name) ->
					return [lookupname name]
				_ -> do
					l' <- gen . annexBackends <$> Annex.getGitConfig
					Annex.changeState $ \s -> s { Annex.backends = l' }
					return l'
  where
	gen [] = list
	gen ns = map lookupname ns
	lookupname = lookupBackendVariety . parseKeyVariety

{- Generates a key for a file, trying each backend in turn until one
 - accepts it. -}
genKey :: KeySource -> Maybe Backend -> Annex (Maybe (Key, Backend))
genKey source trybackend = do
	bs <- orderedList
	let bs' = maybe bs (: bs) trybackend
	genKey' bs' source
genKey' :: [Backend] -> KeySource -> Annex (Maybe (Key, Backend))
genKey' [] _ = return Nothing
genKey' (b:bs) source = do
	r <- B.getKey b source
	case r of
		Nothing -> genKey' bs source
		Just k -> return $ Just (makesane k, b)
  where
	-- keyNames should not contain newline characters.
	makesane k = k { keyName = map fixbadchar (keyName k) }
	fixbadchar c
		| c == '\n' = '_'
		| otherwise = c

getBackend :: FilePath -> Key -> Annex (Maybe Backend)
getBackend file k = case maybeLookupBackendVariety (keyVariety k) of
	Just backend -> return $ Just backend
	Nothing -> do
		warning $ "skipping " ++ file ++ " (unknown backend " ++ formatKeyVariety (keyVariety k) ++ ")"
		return Nothing

{- Looks up the backend that should be used for a file.
 - That can be configured on a per-file basis in the gitattributes file. -}
chooseBackend :: FilePath -> Annex (Maybe Backend)
chooseBackend f = Annex.getState Annex.forcebackend >>= go
  where
	go Nothing =  maybeLookupBackendVariety . parseKeyVariety
		<$> checkAttr "annex.backend" f
	go (Just _) = Just . Prelude.head <$> orderedList

{- Looks up a backend by variety. May fail if unsupported or disabled. -}
lookupBackendVariety :: KeyVariety -> Backend
lookupBackendVariety v = fromMaybe unknown $ maybeLookupBackendVariety v
  where
	unknown = error $ "unknown backend " ++ formatKeyVariety v

maybeLookupBackendVariety :: KeyVariety -> Maybe Backend
maybeLookupBackendVariety v = M.lookup v varietyMap

varietyMap :: M.Map KeyVariety Backend
varietyMap = M.fromList $ zip (map B.backendVariety list) list

isStableKey :: Key -> Bool
isStableKey k = maybe False (`B.isStableKey` k) 
	(maybeLookupBackendVariety (keyVariety k))
