{- git-annex key/value backends
 -
 - Copyright 2010-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend (
	list,
	orderedList,
	genKey,
	lookupFile,
	getBackend,
	isAnnexLink,
	chooseBackend,
	lookupBackendName,
	maybeLookupBackendName
) where

import Common.Annex
import qualified Annex
import Annex.CheckAttr
import Annex.CatFile
import Annex.Link
import Types.Key
import Types.KeySource
import qualified Types.Backend as B
import Config

-- When adding a new backend, import it here and add it to the list.
import qualified Backend.Hash
import qualified Backend.WORM
import qualified Backend.URL

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
					return [lookupBackendName name]
				_ -> do
					l' <- gen . annexBackends <$> Annex.getGitConfig
					Annex.changeState $ \s -> s { Annex.backends = l' }
					return l'
  where
	gen [] = list
	gen l = map lookupBackendName l

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

{- Looks up the key corresponding to an annexed file,
 - by examining what the file links to.
 -
 - In direct mode, there is often no link on disk, in which case
 - the symlink is looked up in git instead. However, a real link
 - on disk still takes precedence over what was committed to git in direct
 - mode.
 -}
lookupFile :: FilePath -> Annex (Maybe Key)
lookupFile file = do
	mkey <- isAnnexLink file
	case mkey of
		Just key -> makeret key
		Nothing -> ifM isDirect
			( maybe (return Nothing) makeret =<< catKeyFile file
			, return Nothing
			)
  where
	makeret k = return $ Just k

getBackend :: FilePath -> Key -> Annex (Maybe Backend)
getBackend file k = let bname = keyBackendName k in
	case maybeLookupBackendName bname of
		Just backend -> return $ Just backend
		Nothing -> do
			warning $ "skipping " ++ file ++ " (unknown backend " ++ bname ++ ")"
			return Nothing

{- Looks up the backend that should be used for a file.
 - That can be configured on a per-file basis in the gitattributes file. -}
chooseBackend :: FilePath -> Annex (Maybe Backend)
chooseBackend f = Annex.getState Annex.forcebackend >>= go
  where
	go Nothing =  maybeLookupBackendName <$> checkAttr "annex.backend" f
	go (Just _) = Just . Prelude.head <$> orderedList

{- Looks up a backend by name. May fail if unknown. -}
lookupBackendName :: String -> Backend
lookupBackendName s = fromMaybe unknown $ maybeLookupBackendName s
  where
	unknown = error $ "unknown backend " ++ s
maybeLookupBackendName :: String -> Maybe Backend
maybeLookupBackendName s = headMaybe matches
  where
	matches = filter (\b -> s == B.name b) list
