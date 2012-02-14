{- git-annex key/value backends
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend (
	list,
	orderedList,
	genKey,
	lookupFile,
	chooseBackend,
	lookupBackendName,
	maybeLookupBackendName
) where

import System.Posix.Files

import Common.Annex
import qualified Git.Config
import qualified Annex
import Annex.CheckAttr
import Types.Key
import qualified Types.Backend as B

-- When adding a new backend, import it here and add it to the list.
import qualified Backend.SHA
import qualified Backend.WORM
import qualified Backend.URL

list :: [Backend]
list = Backend.SHA.backends ++ Backend.WORM.backends ++ Backend.URL.backends

{- List of backends in the order to try them when storing a new key. -}
orderedList :: Annex [Backend]
orderedList = do
	l <- Annex.getState Annex.backends -- list is cached here
	if not $ null l
		then return l
		else handle =<< Annex.getState Annex.forcebackend
	where
		handle Nothing = standard
		handle (Just "") = standard
		handle (Just name) = do
			l' <- (lookupBackendName name :) <$> standard
			Annex.changeState $ \s -> s { Annex.backends = l' }
			return l'
		standard = fromRepo $ parseBackendList . Git.Config.get "annex.backends" ""
		parseBackendList [] = list
		parseBackendList s = map lookupBackendName $ words s

{- Generates a key for a file, trying each backend in turn until one
 - accepts it. -}
genKey :: FilePath -> Maybe Backend -> Annex (Maybe (Key, Backend))
genKey file trybackend = do
	bs <- orderedList
	let bs' = maybe bs (: bs) trybackend
	genKey' bs' file
genKey' :: [Backend] -> FilePath -> Annex (Maybe (Key, Backend))
genKey' [] _ = return Nothing
genKey' (b:bs) file = do
	r <- (B.getKey b) file
	case r of
		Nothing -> genKey' bs file
		Just k -> return $ Just (makesane k, b)
	where
		-- keyNames should not contain newline characters.
		makesane k = k { keyName = map fixbadchar (keyName k) }
		fixbadchar c
			| c == '\n' = '_'
			| otherwise = c

{- Looks up the key and backend corresponding to an annexed file,
 - by examining what the file symlinks to. -}
lookupFile :: FilePath -> Annex (Maybe (Key, Backend))
lookupFile file = do
	tl <- liftIO $ tryIO getsymlink
	case tl of
		Left _ -> return Nothing
		Right l -> makekey l
	where
		getsymlink = takeFileName <$> readSymbolicLink file
		makekey l = maybe (return Nothing) (makeret l) (fileKey l)
		makeret l k = let bname = keyBackendName k in
			case maybeLookupBackendName bname of
				Just backend -> return $ Just (k, backend)
				Nothing -> do
					when (isLinkToAnnex l) $ warning $
						"skipping " ++ file ++
						" (unknown backend " ++
						bname ++ ")"
					return Nothing

{- Looks up the backend that should be used for a file.
 - That can be configured on a per-file basis in the gitattributes file.
 -}
chooseBackend :: FilePath -> Annex (Maybe Backend)
chooseBackend f = Annex.getState Annex.forcebackend >>= go
	where
		go Nothing =  maybeLookupBackendName <$>
			checkAttr "annex.backend" f
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
