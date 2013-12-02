{- WebDAV remotes.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ScopedTypeVariables, CPP #-}

module Remote.WebDAV (remote, davCreds, setCredsEnv, configUrl) where

import Network.Protocol.HTTP.DAV
import qualified Data.Map as M
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString.Lazy.UTF8 as L8
import qualified Data.ByteString.Lazy as L
import Network.URI (normalizePathSegments)
import qualified Control.Exception as E
import Network.HTTP.Conduit (HttpException(..))
import Network.HTTP.Types
import System.IO.Error

import Common.Annex
import Types.Remote
import qualified Git
import Config
import Config.Cost
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Remote.Helper.Chunked
import Crypto
import Creds
import Utility.Metered
import Annex.Content
import Annex.UUID

type DavUrl = String
type DavUser = B8.ByteString
type DavPass = B8.ByteString

remote :: RemoteType
remote = RemoteType {
	typename = "webdav",
	enumerate = findSpecialRemotes "webdav",
	generate = gen,
	setup = webdavSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = new <$> remoteCost gc expensiveRemoteCost
  where
	new cst = Just $ encryptableRemote c
		(storeEncrypted this)
		(retrieveEncrypted this)
		this
	  where
		this = Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
			storeKey = store this,
			retrieveKeyFile = retrieve this,
			retrieveKeyFileCheap = retrieveCheap this,
			removeKey = remove this,
			hasKey = checkPresent this,
			hasKeyCheap = False,
			whereisKey = Nothing,
			remoteFsck = Nothing,
			repairRepo = Nothing,
			config = c,
			repo = r,
			gitconfig = gc,
			localpath = Nothing,
			readonly = False,
			globallyAvailable = True,
			remotetype = remote
		}

webdavSetup :: Maybe UUID -> RemoteConfig -> Annex (RemoteConfig, UUID)
webdavSetup mu c = do
	u <- maybe (liftIO genUUID) return mu
	let url = fromMaybe (error "Specify url=") $
		M.lookup "url" c
	c' <- encryptionSetup c
	creds <- getCreds c' u
	testDav url creds
	gitConfigSpecialRemote u c' "webdav" "true"
	c'' <- setRemoteCredPair c' (davCreds u)
	return (c'', u)

store :: Remote -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store r k _f p = metered (Just p) k $ \meterupdate ->
	davAction r False $ \(baseurl, user, pass) -> 
		sendAnnex k (void $ remove r k) $ \src ->
			liftIO $ withMeteredFile src meterupdate $
				storeHelper r k baseurl user pass

storeEncrypted :: Remote -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted r (cipher, enck) k p = metered (Just p) k $ \meterupdate ->
	davAction r False $ \(baseurl, user, pass) ->
		sendAnnex k (void $ remove r enck) $ \src ->
			liftIO $ encrypt (getGpgEncParams r) cipher
				(streamMeteredFile src meterupdate) $
				readBytes $ storeHelper r enck baseurl user pass

storeHelper :: Remote -> Key -> DavUrl -> DavUser -> DavPass -> L.ByteString -> IO Bool
storeHelper r k baseurl user pass b = catchBoolIO $ do
	davMkdir tmpurl user pass
	storeChunks k tmpurl keyurl chunksize storer recorder finalizer
  where
	tmpurl = tmpLocation baseurl k
	keyurl = davLocation baseurl k
	chunksize = chunkSize $ config r
	storer urls = storeChunked chunksize urls storehttp b
	recorder url s = storehttp url (L8.fromString s)
	finalizer srcurl desturl = do
		void $ catchMaybeHttp (deleteContent desturl user pass)
		davMkdir (urlParent desturl) user pass
		moveContent srcurl (B8.fromString desturl) user pass
	storehttp url v = putContent url user pass
		(contentType, v)

retrieveCheap :: Remote -> Key -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

retrieve :: Remote -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
retrieve r k _f d p = metered (Just p) k $ \meterupdate ->
	davAction r False $ \(baseurl, user, pass) -> liftIO $ catchBoolIO $
		withStoredFiles r k baseurl user pass onerr $ \urls -> do
			meteredWriteFileChunks meterupdate d urls $ \url -> do
				mb <- davGetUrlContent url user pass
				case mb of
					Nothing -> throwIO "download failed"
					Just b -> return b
			return True
  where
	onerr _ = return False

retrieveEncrypted :: Remote -> (Cipher, Key) -> Key -> FilePath -> MeterUpdate -> Annex Bool
retrieveEncrypted r (cipher, enck) k d p = metered (Just p) k $ \meterupdate ->
	davAction r False $ \(baseurl, user, pass) -> liftIO $ catchBoolIO $
		withStoredFiles r enck baseurl user pass onerr $ \urls -> do
			decrypt cipher (feeder user pass urls) $
				readBytes $ meteredWriteFile meterupdate d
			return True
  where
	onerr _ = return False

	feeder _ _ [] _ = noop
	feeder user pass (url:urls) h = do
		mb <- davGetUrlContent url user pass
		case mb of
			Nothing -> throwIO "download failed"
			Just b -> do
				L.hPut h b
				feeder user pass urls h

remove :: Remote -> Key -> Annex Bool
remove r k = davAction r False $ \(baseurl, user, pass) -> liftIO $ do
	-- Delete the key's whole directory, including any chunked
	-- files, etc, in a single action.
	let url = davLocation baseurl k
	isJust <$> catchMaybeHttp (deleteContent url user pass)

checkPresent :: Remote -> Key -> Annex (Either String Bool)
checkPresent r k = davAction r noconn go
  where
	noconn = Left $ error $ name r ++ " not configured"

	go (baseurl, user, pass) = do
		showAction $ "checking " ++ name r
		liftIO $ withStoredFiles r k baseurl user pass onerr check
	  where
		check [] = return $ Right True
		check (url:urls) = do
			v <- davUrlExists url user pass
			if v == Right True
				then check urls
				else return v

		{- Failed to read the chunkcount file; see if it's missing,
		 - or if there's a problem accessing it,
		 - or perhaps this was an intermittent error. -}
		onerr url = do
			v <- davUrlExists url user pass
			return $ if v == Right True
				then Left $ "failed to read " ++ url
				else v

withStoredFiles
	:: Remote
	-> Key
	-> DavUrl
	-> DavUser
	-> DavPass
	-> (DavUrl -> IO a)
	-> ([DavUrl] -> IO a)
	-> IO a
withStoredFiles r k baseurl user pass onerr a
	| isJust $ chunkSize $ config r = do
		let chunkcount = keyurl ++ chunkCount
		v <- davGetUrlContent chunkcount user pass
		case v of
			Just s -> a $ listChunks keyurl $ L8.toString s
			Nothing -> do
				chunks <- probeChunks keyurl $ \u -> (== Right True) <$> davUrlExists u user pass
				if null chunks
					then onerr chunkcount
					else a chunks
	| otherwise = a [keyurl]
  where
	keyurl = davLocation baseurl k ++ keyFile k

davAction :: Remote -> a -> ((DavUrl, DavUser, DavPass) -> Annex a) -> Annex a
davAction r unconfigured action = do
	mcreds <- getCreds (config r) (uuid r)
	case (mcreds, configUrl r) of
		(Just (user, pass), Just url) ->
			action (url, toDavUser user, toDavPass pass)
		_ -> return unconfigured

configUrl :: Remote -> Maybe DavUrl
configUrl r = fixup <$> M.lookup "url" (config r)
  where
	-- box.com DAV url changed
	fixup = replace "https://www.box.com/dav/" "https://dav.box.com/dav/"

toDavUser :: String -> DavUser
toDavUser = B8.fromString

toDavPass :: String -> DavPass
toDavPass = B8.fromString

{- The directory where files(s) for a key are stored. -}
davLocation :: DavUrl -> Key -> DavUrl
davLocation baseurl k = addTrailingPathSeparator $
	davUrl baseurl $ hashDirLower k </> keyFile k

{- Where we store temporary data for a key as it's being uploaded. -}
tmpLocation :: DavUrl -> Key -> DavUrl
tmpLocation baseurl k = addTrailingPathSeparator $
	davUrl baseurl $ "tmp" </> keyFile k

davUrl :: DavUrl -> FilePath -> DavUrl
davUrl baseurl file = baseurl </> file

davUrlExists :: DavUrl -> DavUser -> DavPass -> IO (Either String Bool)
davUrlExists url user pass = decode <$> catchHttp get
  where
	decode (Right _) = Right True
#if ! MIN_VERSION_http_conduit(1,9,0)
	decode (Left (Left (StatusCodeException status _)))
#else
	decode (Left (Left (StatusCodeException status _ _)))
#endif
		| statusCode status == statusCode notFound404 = Right False
	decode (Left e) = Left $ showEitherException e
#if ! MIN_VERSION_DAV(0,4,0)
	get = getProps url user pass
#else
	get = getProps url user pass Nothing
#endif

davGetUrlContent :: DavUrl -> DavUser -> DavPass -> IO  (Maybe L.ByteString)
davGetUrlContent url user pass = fmap (snd . snd) <$>
	catchMaybeHttp (getPropsAndContent url user pass)

{- Creates a directory in WebDAV, if not already present; also creating
 - any missing parent directories. -}
davMkdir :: DavUrl -> DavUser -> DavPass -> IO ()
davMkdir url user pass = go url
  where
	make u = makeCollection u user pass

	go u = do
		r <- E.try (make u) :: IO (Either E.SomeException Bool)
		case r of
			{- Parent directory is missing. Recurse to create
			 - it, and try once more to create the directory. -}
			Right False -> do
				go (urlParent u)
				void $ make u
			{- Directory created successfully -}
			Right True -> return ()
			{- Directory already exists, or some other error
			 - occurred. In the latter case, whatever wanted
			 - to use this directory will fail. -}
			Left _ -> return ()

{- Catches HTTP and IO exceptions. -}
catchMaybeHttp :: IO a -> IO (Maybe a)
catchMaybeHttp a = (Just <$> a) `E.catches`
	[ E.Handler $ \(_e :: HttpException) -> return Nothing
	, E.Handler $ \(_e :: E.IOException) -> return Nothing
	]

{- Catches HTTP and IO exceptions -}
catchHttp :: IO a -> IO (Either EitherException a)
catchHttp a = (Right <$> a) `E.catches`
	[ E.Handler $ \(e :: HttpException) -> return $ Left $ Left e
	, E.Handler $ \(e :: E.IOException) -> return $ Left $ Right e
	]

type EitherException = Either HttpException E.IOException

showEitherException :: EitherException -> String
#if ! MIN_VERSION_http_conduit(1,9,0)
showEitherException (Left (StatusCodeException status _)) =
#else
showEitherException (Left (StatusCodeException status _ _)) =
#endif
	show $ statusMessage status
showEitherException (Left httpexception) = show httpexception
showEitherException (Right ioexception) = show ioexception

throwIO :: String -> IO a
throwIO msg = ioError $ mkIOError userErrorType msg Nothing Nothing

urlParent :: DavUrl -> DavUrl
urlParent url = dropTrailingPathSeparator $
	normalizePathSegments (dropTrailingPathSeparator url ++ "/..")
  where

{- Test if a WebDAV store is usable, by writing to a test file, and then
 - deleting the file. Exits with an IO error if not. -}
testDav :: String -> Maybe CredPair -> Annex ()
testDav baseurl (Just (u, p)) = do
	showSideAction "testing WebDAV server"
	test "make directory" $ davMkdir baseurl user pass
	test "write file" $ putContent testurl user pass
		(contentType, L.empty)
	test "delete file" $ deleteContent testurl user pass
  where
	test desc a = liftIO $
		either (\e -> throwIO $ "WebDAV failed to " ++ desc ++ ": " ++ showEitherException e)
			(const noop)
			=<< catchHttp a

	user = toDavUser u
	pass = toDavPass p
	testurl = davUrl baseurl "git-annex-test"
testDav _ Nothing = error "Need to configure webdav username and password."

{- Content-Type to use for files uploaded to WebDAV. -}
contentType :: Maybe B8.ByteString
contentType = Just $ B8.fromString "application/octet-stream"

getCreds :: RemoteConfig -> UUID -> Annex (Maybe CredPair)
getCreds c u = getRemoteCredPairFor "webdav" c (davCreds u)

davCreds :: UUID -> CredPairStorage
davCreds u = CredPairStorage
	{ credPairFile = fromUUID u
	, credPairEnvironment = ("WEBDAV_USERNAME", "WEBDAV_PASSWORD")
	, credPairRemoteKey = Just "davcreds"
	}

setCredsEnv :: (String, String) -> IO ()
setCredsEnv creds = setEnvCredPair creds $ davCreds undefined
