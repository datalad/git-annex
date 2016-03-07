{- WebDAV remotes.
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ScopedTypeVariables #-}

module Remote.WebDAV (remote, davCreds, configUrl) where

import Network.Protocol.HTTP.DAV
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString.Lazy.UTF8 as L8
import Network.HTTP.Client (HttpException(..))
import Network.HTTP.Types
import System.IO.Error
import Control.Monad.Catch

import Annex.Common
import Types.Remote
import qualified Git
import Config
import Config.Cost
import Remote.Helper.Special
import Remote.Helper.Messages
import Remote.Helper.Http
import qualified Remote.Helper.Chunked.Legacy as Legacy
import Creds
import Utility.Metered
import Utility.Url (URLString)
import Annex.UUID
import Remote.WebDAV.DavLocation

remote :: RemoteType
remote = RemoteType {
	typename = "webdav",
	enumerate = const (findSpecialRemotes "webdav"),
	generate = gen,
	setup = webdavSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = new <$> remoteCost gc expensiveRemoteCost
  where
	new cst = Just $ specialRemote c
		(prepareDAV this $ store chunkconfig)
		(prepareDAV this $ retrieve chunkconfig)
		(prepareDAV this $ remove)
		(prepareDAV this $ checkKey this chunkconfig)
		this
	  where
		this = Remote
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = storeKeyDummy
			, retrieveKeyFile = retreiveKeyFileDummy
			, retrieveKeyFileCheap = retrieveCheap
			, removeKey = removeKeyDummy
			, lockContent = Nothing
			, checkPresent = checkPresentDummy
			, checkPresentCheap = False
			, whereisKey = Nothing
			, remoteFsck = Nothing
			, repairRepo = Nothing
			, config = c
			, repo = r
			, gitconfig = gc
			, localpath = Nothing
			, readonly = False
			, availability = GloballyAvailable
			, remotetype = remote
			, mkUnavailable = gen r u (M.insert "url" "http://!dne!/" c) gc
			, getInfo = includeCredsInfo c (davCreds u) $
				[("url", fromMaybe "unknown" (M.lookup "url" c))]
			, claimUrl = Nothing
			, checkUrl = Nothing
			}
		chunkconfig = getChunkConfig c

webdavSetup :: Maybe UUID -> Maybe CredPair -> RemoteConfig -> Annex (RemoteConfig, UUID)
webdavSetup mu mcreds c = do
	u <- maybe (liftIO genUUID) return mu
	url <- case M.lookup "url" c of
		Nothing -> error "Specify url="
		Just url -> return url
	(c', encsetup) <- encryptionSetup c
	creds <- maybe (getCreds c' u) (return . Just) mcreds
	testDav url creds
	gitConfigSpecialRemote u c' "webdav" "true"
	c'' <- setRemoteCredPair encsetup c' (davCreds u) creds
	return (c'', u)

-- Opens a http connection to the DAV server, which will be reused
-- each time the helper is called.
prepareDAV :: Remote -> (Maybe DavHandle -> helper) -> Preparer helper
prepareDAV = resourcePrepare . const . withDAVHandle

store :: ChunkConfig -> Maybe DavHandle -> Storer
store _ Nothing = byteStorer $ \_k _b _p -> return False
store (LegacyChunks chunksize) (Just dav) = fileStorer $ \k f p -> liftIO $
	withMeteredFile f p $ storeLegacyChunked chunksize k dav
store _  (Just dav) = httpStorer $ \k reqbody -> liftIO $ goDAV dav $ do
	let tmp = keyTmpLocation k
	let dest = keyLocation k
	void $ mkColRecursive tmpDir
	inLocation tmp $
		putContentM' (contentType, reqbody)
	finalizeStore (baseURL dav) tmp dest
	return True

finalizeStore :: URLString -> DavLocation -> DavLocation -> DAVT IO ()
finalizeStore baseurl tmp dest = do
	inLocation dest $ void $ safely $ delContentM
	maybe noop (void . mkColRecursive) (locationParent dest)
	moveDAV baseurl tmp dest

retrieveCheap :: Key -> AssociatedFile -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

retrieve :: ChunkConfig -> Maybe DavHandle -> Retriever
retrieve _ Nothing = error "unable to connect"
retrieve (LegacyChunks _) (Just dav) = retrieveLegacyChunked dav
retrieve _ (Just dav) = fileRetriever $ \d k p -> liftIO $
	goDAV dav $
		inLocation (keyLocation k) $
			withContentM $
				httpBodyRetriever d p

remove :: Maybe DavHandle -> Remover
remove Nothing _ = return False
remove (Just dav) k = liftIO $ do
	-- Delete the key's whole directory, including any
	-- legacy chunked files, etc, in a single action.
	let d = keyDir k
	goDAV dav $ do
		v <- safely $ inLocation d delContentM
		case v of
			Just _ -> return True
			Nothing -> do
				v' <- existsDAV d
				case v' of
					Right False -> return True
					_ -> return False

checkKey :: Remote -> ChunkConfig -> Maybe DavHandle -> CheckPresent
checkKey r _ Nothing _ = error $ name r ++ " not configured"
checkKey r chunkconfig (Just dav) k = do
	showChecking r
	case chunkconfig of
		LegacyChunks _ -> checkKeyLegacyChunked dav k
		_ -> do
			v <- liftIO $ goDAV dav $
				existsDAV (keyLocation k)
			either error return v

configUrl :: Remote -> Maybe URLString
configUrl r = fixup <$> M.lookup "url" (config r)
  where
	-- box.com DAV url changed
	fixup = replace "https://www.box.com/dav/" "https://dav.box.com/dav/"

type DavUser = B8.ByteString
type DavPass = B8.ByteString

baseURL :: DavHandle -> URLString
baseURL (DavHandle _ _ _ u) = u


toDavUser :: String -> DavUser
toDavUser = B8.fromString

toDavPass :: String -> DavPass
toDavPass = B8.fromString

{- Test if a WebDAV store is usable, by writing to a test file, and then
 - deleting the file.
 -
 - Also ensures that the path of the url exists, trying to create it if not.
 -
 - Throws an error if store is not usable.
 -}
testDav :: URLString -> Maybe CredPair -> Annex ()
testDav url (Just (u, p)) = do
	showAction "testing WebDAV server"
	test $ liftIO $ evalDAVT url $ do
		prepDAV user pass
		makeParentDirs
		void $ mkColRecursive tmpDir
		inLocation (tmpLocation "git-annex-test") $ do
			putContentM (Nothing, L8.fromString "test")
			delContentM
  where
	test a = liftIO $
		either (\e -> throwIO $ "WebDAV test failed: " ++ show e)
			(const noop)
			=<< tryNonAsync a

	user = toDavUser u
	pass = toDavPass p
testDav _ Nothing = error "Need to configure webdav username and password."

{- Tries to make all the parent directories in the WebDAV urls's path,
 - right down to the root.
 -
 - Ignores any failures, which can occur for reasons including the WebDAV
 - server only serving up WebDAV in a subdirectory. -}
makeParentDirs :: DAVT IO ()
makeParentDirs = go
  where
	go = do
		l <- getDAVLocation
		case locationParent l of
			Nothing -> noop
			Just p -> void $ safely $ inDAVLocation (const p) go
		void $ safely mkCol

{- Checks if the directory exists. If not, tries to create its
 - parent directories, all the way down to the root, and finally creates
 - it. -}
mkColRecursive :: DavLocation -> DAVT IO Bool
mkColRecursive d = go =<< existsDAV d
  where
	go (Right True) = return True
	go _ = ifM (inLocation d mkCol)
		( return True
		, do
			case locationParent d of
				Nothing -> makeParentDirs
				Just parent -> void (mkColRecursive parent)
			inLocation d mkCol
		)

getCreds :: RemoteConfig -> UUID -> Annex (Maybe CredPair)
getCreds c u = getRemoteCredPairFor "webdav" c (davCreds u)

davCreds :: UUID -> CredPairStorage
davCreds u = CredPairStorage
	{ credPairFile = fromUUID u
	, credPairEnvironment = ("WEBDAV_USERNAME", "WEBDAV_PASSWORD")
	, credPairRemoteKey = Just "davcreds"
	}

{- Content-Type to use for files uploaded to WebDAV. -}
contentType :: Maybe B8.ByteString
contentType = Just $ B8.fromString "application/octet-stream"

throwIO :: String -> IO a
throwIO msg = ioError $ mkIOError userErrorType msg Nothing Nothing

moveDAV :: URLString -> DavLocation -> DavLocation -> DAVT IO ()
moveDAV baseurl src dest = inLocation src $ moveContentM newurl
  where
	newurl = B8.fromString (locationUrl baseurl dest)

existsDAV :: DavLocation -> DAVT IO (Either String Bool)
existsDAV l = inLocation l check `catchNonAsync` (\e -> return (Left $ show e))
  where
	check = do
		-- Some DAV services only support depth of 1, and
		-- more depth is certainly not needed to check if a
		-- location exists.
		setDepth (Just Depth1)
		catchJust
			(matchStatusCodeException (== notFound404))
			(getPropsM >> ispresent True)
			(const $ ispresent False)
	ispresent = return . Right

matchStatusCodeException :: (Status -> Bool) -> HttpException -> Maybe HttpException
matchStatusCodeException want e@(StatusCodeException s _ _)
	| want s = Just e
	| otherwise = Nothing
matchStatusCodeException _ _ = Nothing

-- Ignores any exceptions when performing a DAV action.
safely :: DAVT IO a -> DAVT IO (Maybe a)
safely = eitherToMaybe <$$> tryNonAsync

choke :: IO (Either String a) -> IO a
choke f = do
	x <- f
	case x of
		Left e -> error e
		Right r -> return r

data DavHandle = DavHandle DAVContext DavUser DavPass URLString

withDAVHandle :: Remote -> (Maybe DavHandle -> Annex a) -> Annex a
withDAVHandle r a = do
	mcreds <- getCreds (config r) (uuid r)
	case (mcreds, configUrl r) of
		(Just (user, pass), Just baseurl) ->
			withDAVContext baseurl $ \ctx ->
				a (Just (DavHandle ctx (toDavUser user) (toDavPass pass) baseurl))
		_ -> a Nothing

goDAV :: DavHandle -> DAVT IO a -> IO a
goDAV (DavHandle ctx user pass _) a = choke $ run $ prettifyExceptions $ do
	prepDAV user pass
	a
  where
	run = fst <$$> runDAVContext ctx

{- Catch StatusCodeException and trim it to only the statusMessage part,
 - eliminating a lot of noise, which can include the whole request that
 - failed. The rethrown exception is no longer a StatusCodeException. -}
prettifyExceptions :: DAVT IO a -> DAVT IO a
prettifyExceptions a = catchJust (matchStatusCodeException (const True)) a go
  where
	go (StatusCodeException status _ _) = error $ unwords
		[ "DAV failure:"
		, show (statusCode status)
		, show (statusMessage status)
		]
	go e = throwM e

prepDAV :: DavUser -> DavPass -> DAVT IO ()
prepDAV user pass = do
	setResponseTimeout Nothing -- disable default (5 second!) timeout
	setCreds user pass

--
-- Legacy chunking code, to be removed eventually.
--

storeLegacyChunked :: ChunkSize -> Key -> DavHandle -> L.ByteString -> IO Bool
storeLegacyChunked chunksize k dav b =
	Legacy.storeChunks k tmp dest storer recorder finalizer
  where
	storehttp l b' = void $ goDAV dav $ do
		maybe noop (void . mkColRecursive) (locationParent l)
		inLocation l $ putContentM (contentType, b')
	storer locs = Legacy.storeChunked chunksize locs storehttp b
	recorder l s = storehttp l (L8.fromString s)
	finalizer tmp' dest' = goDAV dav $ 
		finalizeStore (baseURL dav) tmp' (fromJust $ locationParent dest')

	tmp = addTrailingPathSeparator $ keyTmpLocation k
	dest = keyLocation k

retrieveLegacyChunked :: DavHandle -> Retriever
retrieveLegacyChunked dav = fileRetriever $ \d k p -> liftIO $
	withStoredFilesLegacyChunked k dav onerr $ \locs ->
		Legacy.meteredWriteFileChunks p d locs $ \l ->
			goDAV dav $
				inLocation l $
					snd <$> getContentM
  where
	onerr = error "download failed"

checkKeyLegacyChunked :: DavHandle -> CheckPresent
checkKeyLegacyChunked dav k = liftIO $
	either error id <$> withStoredFilesLegacyChunked k dav onerr check
  where
	check [] = return $ Right True
	check (l:ls) = do
		v <- goDAV dav $ existsDAV l
		if v == Right True
			then check ls
			else return v

	{- Failed to read the chunkcount file; see if it's missing,
	 - or if there's a problem accessing it,
	- or perhaps this was an intermittent error. -}
	onerr f = do
		v <- goDAV dav $ existsDAV f
		return $ if v == Right True
			then Left $ "failed to read " ++ f
			else v

withStoredFilesLegacyChunked
	:: Key
	-> DavHandle
	-> (DavLocation -> IO a)
	-> ([DavLocation] -> IO a)
	-> IO a
withStoredFilesLegacyChunked k dav onerr a = do
	let chunkcount = keyloc ++ Legacy.chunkCount
	v <- goDAV dav $ safely $ 
		inLocation chunkcount $
			snd <$> getContentM
	case v of
		Just s -> a $ Legacy.listChunks keyloc $ L8.toString s
		Nothing -> do
			chunks <- Legacy.probeChunks keyloc $ \f ->
				(== Right True) <$> goDAV dav (existsDAV f)
			if null chunks
				then onerr chunkcount
				else a chunks
  where
	keyloc = keyLocation k
