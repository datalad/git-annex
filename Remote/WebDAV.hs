{- WebDAV remotes.
 -
 - Copyright 2012-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Remote.WebDAV (remote, davCreds, configUrl) where

import Network.Protocol.HTTP.DAV
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString.Lazy.UTF8 as L8
import Network.HTTP.Client (HttpException(..), RequestBody)
#if MIN_VERSION_http_client(0,5,0)
import qualified Network.HTTP.Client as HTTP
#endif
import Network.HTTP.Types
import System.IO.Error
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO)
import System.Log.Logger (debugM)

import Annex.Common
import Types.Remote
import Types.Export
import qualified Git
import Config
import Config.Cost
import Remote.Helper.Special
import Remote.Helper.Messages
import Remote.Helper.Http
import Remote.Helper.Export
import qualified Remote.Helper.Chunked.Legacy as Legacy
import Creds
import Utility.Metered
import Utility.Url (URLString, matchStatusCodeException, matchHttpExceptionContent)
import Annex.UUID
import Remote.WebDAV.DavLocation

#if MIN_VERSION_http_client(0,5,0)
import Network.HTTP.Client (HttpExceptionContent(..), responseStatus)
#endif

remote :: RemoteType
remote = RemoteType
	{ typename = "webdav"
	, enumerate = const (findSpecialRemotes "webdav")
	, generate = gen
	, setup = webdavSetup
	, exportSupported = exportIsSupported
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
			, exportActions = withDAVHandle this $ \mh -> return $ ExportActions
				{ storeExport = storeExportDav mh
				, retrieveExport = retrieveExportDav mh
				, checkPresentExport = checkPresentExportDav this mh
				, removeExport = removeExportDav mh
				, removeExportDirectory = Just $
					removeExportDirectoryDav mh
				, renameExport = renameExportDav mh
				}
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

webdavSetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
webdavSetup _ mu mcreds c gc = do
	u <- maybe (liftIO genUUID) return mu
	url <- case M.lookup "url" c of
		Nothing -> giveup "Specify url="
		Just url -> return url
	(c', encsetup) <- encryptionSetup c gc
	creds <- maybe (getCreds c' gc u) (return . Just) mcreds
	testDav url creds
	gitConfigSpecialRemote u c' "webdav" "true"
	c'' <- setRemoteCredPair encsetup c' gc (davCreds u) creds
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
	storeHelper dav tmp dest reqbody
	return True

storeHelper :: DavHandle -> DavLocation -> DavLocation -> RequestBody -> DAVT IO ()
storeHelper dav tmp dest reqbody = do
	maybe noop (void . mkColRecursive) (locationParent tmp)
	debugDav $ "putContent " ++ tmp
	inLocation tmp $
		putContentM' (contentType, reqbody)
	finalizeStore dav tmp dest

finalizeStore :: DavHandle -> DavLocation -> DavLocation -> DAVT IO ()
finalizeStore dav tmp dest = do
	debugDav $ "delContent " ++ dest
	inLocation dest $ void $ safely $ delContentM
	maybe noop (void . mkColRecursive) (locationParent dest)
	moveDAV (baseURL dav) tmp dest

retrieveCheap :: Key -> AssociatedFile -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

retrieve :: ChunkConfig -> Maybe DavHandle -> Retriever
retrieve _ Nothing = giveup "unable to connect"
retrieve (LegacyChunks _) (Just dav) = retrieveLegacyChunked dav
retrieve _ (Just dav) = fileRetriever $ \d k p -> liftIO $
	goDAV dav $ retrieveHelper (keyLocation k) d p

retrieveHelper :: DavLocation -> FilePath -> MeterUpdate -> DAVT IO ()
retrieveHelper loc d p = do
	debugDav $ "retrieve " ++ loc
	inLocation loc $
		withContentM $ httpBodyRetriever d p

remove :: Maybe DavHandle -> Remover
remove Nothing _ = return False
remove (Just dav) k = liftIO $ goDAV dav $
	-- Delete the key's whole directory, including any
	-- legacy chunked files, etc, in a single action.
	removeHelper (keyDir k)

removeHelper :: DavLocation -> DAVT IO Bool
removeHelper d = do
	debugDav $ "delContent " ++ d
	v <- safely $ inLocation d delContentM
	case v of
		Just _ -> return True
		Nothing -> do
			v' <- existsDAV d
			case v' of
				Right False -> return True
				_ -> return False

checkKey :: Remote -> ChunkConfig -> Maybe DavHandle -> CheckPresent
checkKey r _ Nothing _ = giveup $ name r ++ " not configured"
checkKey r chunkconfig (Just dav) k = do
	showChecking r
	case chunkconfig of
		LegacyChunks _ -> checkKeyLegacyChunked dav k
		_ -> do
			v <- liftIO $ goDAV dav $
				existsDAV (keyLocation k)
			either giveup return v

storeExportDav :: Maybe DavHandle -> FilePath -> Key -> ExportLocation -> MeterUpdate -> Annex Bool
storeExportDav mh f k loc p = runExport mh $ \dav -> do
	reqbody <- liftIO $ httpBodyStorer f p
	storeHelper dav (keyTmpLocation k) (exportLocation loc) reqbody
	return True

retrieveExportDav :: Maybe DavHandle -> Key -> ExportLocation -> FilePath -> MeterUpdate -> Annex Bool
retrieveExportDav mh  _k loc d p = runExport mh $ \_dav -> do
	retrieveHelper (exportLocation loc) d p
	return True

checkPresentExportDav :: Remote -> Maybe DavHandle -> Key -> ExportLocation -> Annex Bool
checkPresentExportDav r mh _k loc = case mh of
	Nothing -> giveup $ name r ++ " not configured"
	Just h -> liftIO $ do
		v <- goDAV h $ existsDAV (exportLocation loc)
		either giveup return v

removeExportDav :: Maybe DavHandle -> Key -> ExportLocation -> Annex Bool
removeExportDav mh _k loc = runExport mh $ \_dav ->
	removeHelper (exportLocation loc)

removeExportDirectoryDav :: Maybe DavHandle -> ExportDirectory -> Annex Bool
removeExportDirectoryDav mh dir = runExport mh $ \_dav -> do
	let d = fromExportDirectory dir
	debugDav $ "delContent " ++ d
	safely (inLocation d delContentM)
		>>= maybe (return False) (const $ return True)

renameExportDav :: Maybe DavHandle -> Key -> ExportLocation -> ExportLocation -> Annex Bool
renameExportDav Nothing _ _ _ = return False
renameExportDav (Just h) _k src dest
	-- box.com's DAV endpoint has buggy handling of renames,
	-- so avoid renaming when using it.
	| boxComUrl `isPrefixOf` baseURL h = return False
	| otherwise = runExport (Just h) $ \dav -> do
		maybe noop (void . mkColRecursive) (locationParent (exportLocation dest))
		moveDAV (baseURL dav) (exportLocation src) (exportLocation dest)
		return True

runExport :: Maybe DavHandle -> (DavHandle -> DAVT IO Bool) -> Annex Bool
runExport Nothing _ = return False
runExport (Just h) a = fromMaybe False <$> liftIO (goDAV h $ safely (a h))

configUrl :: Remote -> Maybe URLString
configUrl r = fixup <$> M.lookup "url" (config r)
  where
	-- box.com DAV url changed
	fixup = replace "https://www.box.com/dav/" boxComUrl

boxComUrl :: URLString
boxComUrl = "https://dav.box.com/dav/"

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
		void $ mkColRecursive "/"
		inLocation (tmpLocation "test") $ do
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
	go _ = do
		debugDav $ "mkCol " ++ d
		ifM (inLocation d mkCol)
			( return True
			, do
				case locationParent d of
					Nothing -> makeParentDirs
					Just parent -> void (mkColRecursive parent)
				inLocation d mkCol
			)

getCreds :: RemoteConfig -> RemoteGitConfig -> UUID -> Annex (Maybe CredPair)
getCreds c gc u = getRemoteCredPairFor "webdav" c gc (davCreds u)

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
moveDAV baseurl src dest = do
	debugDav $ "moveContent " ++ src ++ " " ++ newurl
	inLocation src $ moveContentM (B8.fromString newurl)
  where
	newurl = locationUrl baseurl dest

existsDAV :: DavLocation -> DAVT IO (Either String Bool)
existsDAV l = do
	debugDav $ "getProps " ++ l
	inLocation l check `catchNonAsync` (\e -> return (Left $ show e))
  where
	check = do
		-- Some DAV services only support depth of 1, and
		-- more depth is certainly not needed to check if a
		-- location exists.
		setDepth (Just Depth1)
		catchJust missinghttpstatus
			(getPropsM >> ispresent True)
			(const $ ispresent False)
	ispresent = return . Right
	missinghttpstatus e = 
		matchStatusCodeException (== notFound404) e
		<|> matchHttpExceptionContent toomanyredirects e
	toomanyredirects (TooManyRedirects _) = True
	toomanyredirects _ = False

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
	mcreds <- getCreds (config r) (gitconfig r) (uuid r)
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
#if MIN_VERSION_http_client(0,5,0)
prettifyExceptions :: DAVT IO a -> DAVT IO a
prettifyExceptions a = catchJust (matchStatusCodeException (const True)) a go
  where
	go (HttpExceptionRequest req (StatusCodeException response message)) = giveup $ unwords
		[ "DAV failure:"
		, show (responseStatus response)
		, show (message)
		, "HTTP request:"
		, show (HTTP.method req)
		, show (HTTP.path req)
		]
	go e = throwM e
#else
prettifyExceptions :: DAVT IO a -> DAVT IO a
prettifyExceptions a = catchJust (matchStatusCodeException (const True)) a go
  where
	go (StatusCodeException status _ _) = giveup $ unwords
		[ "DAV failure:"
		, show (statusCode status)
		, show (statusMessage status)
		]
	go e = throwM e
#endif

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
		debugDav $ "putContent " ++ l
		inLocation l $ putContentM (contentType, b')
	storer locs = Legacy.storeChunked chunksize locs storehttp b
	recorder l s = storehttp l (L8.fromString s)
	finalizer tmp' dest' = goDAV dav $ 
		finalizeStore dav tmp' (fromJust $ locationParent dest')

	tmp = addTrailingPathSeparator $ keyTmpLocation k
	dest = keyLocation k

retrieveLegacyChunked :: DavHandle -> Retriever
retrieveLegacyChunked dav = fileRetriever $ \d k p -> liftIO $
	withStoredFilesLegacyChunked k dav onerr $ \locs ->
		Legacy.meteredWriteFileChunks p d locs $ \l ->
			goDAV dav $ do
				debugDav $ "getContent " ++ l
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
	v <- goDAV dav $ safely $ do
		debugDav $ "getContent " ++ chunkcount
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

debugDav :: MonadIO m => String -> DAVT m ()
debugDav msg = liftIO $ debugM "WebDAV" msg
