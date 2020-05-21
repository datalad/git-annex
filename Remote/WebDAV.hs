{- WebDAV remotes.
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE ScopedTypeVariables #-}

module Remote.WebDAV (remote, davCreds, configUrl) where

import Network.Protocol.HTTP.DAV
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString.Lazy.UTF8 as L8
import Network.HTTP.Client (HttpException(..), RequestBody)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client (HttpExceptionContent(..), responseStatus)
import Network.HTTP.Types
import System.IO.Error
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO)
import System.Log.Logger (debugM)
import Control.Concurrent.STM hiding (check)

import Annex.Common
import Types.Remote
import Types.Export
import qualified Git
import Config
import Config.Cost
import Annex.SpecialRemote.Config
import Remote.Helper.Special
import Remote.Helper.Messages
import Remote.Helper.Http
import Remote.Helper.ExportImport
import qualified Remote.Helper.Chunked.Legacy as Legacy
import Creds
import Utility.Metered
import Utility.Url (URLString, matchStatusCodeException, matchHttpExceptionContent)
import Annex.UUID
import Remote.WebDAV.DavLocation
import Types.ProposedAccepted

remote :: RemoteType
remote = specialRemoteType $ RemoteType
	{ typename = "webdav"
	, enumerate = const (findSpecialRemotes "webdav")
	, generate = gen
	, configParser = mkRemoteConfigParser
		[ optionalStringParser urlField
			(FieldDesc "(required) url to the WebDAV directory")
		, optionalStringParser davcredsField HiddenField
		]
	, setup = webdavSetup
	, exportSupported = exportIsSupported
	, importSupported = importUnsupported
	}

urlField :: RemoteConfigField
urlField = Accepted "url"

davcredsField :: RemoteConfigField
davcredsField = Accepted "davcreds"

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = do
	c <- parsedRemoteConfig remote rc
	new
		<$> pure c
		<*> remoteCost gc expensiveRemoteCost
		<*> mkDavHandleVar c gc u
  where
	new c cst hdl = Just $ specialRemote c
		(store hdl chunkconfig)
		(retrieve hdl chunkconfig)
		(remove hdl)
		(checkKey hdl this chunkconfig)
		this
	  where
		this = Remote
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = storeKeyDummy
			, retrieveKeyFile = retrieveKeyFileDummy
			, retrieveKeyFileCheap = Nothing
			-- HttpManagerRestricted is used here, so this is
			-- secure.
			, retrievalSecurityPolicy = RetrievalAllKeysSecure
			, removeKey = removeKeyDummy
			, lockContent = Nothing
			, checkPresent = checkPresentDummy
			, checkPresentCheap = False
			, exportActions = ExportActions
				{ storeExport = storeExportDav hdl
				, retrieveExport = retrieveExportDav hdl
				, checkPresentExport = checkPresentExportDav hdl this
				, removeExport = removeExportDav hdl
				, removeExportDirectory = Just $
					removeExportDirectoryDav hdl
				, renameExport = renameExportDav hdl
				}
			, importActions = importUnsupported
			, whereisKey = Nothing
			, remoteFsck = Nothing
			, repairRepo = Nothing
			, config = c
			, getRepo = return r
			, gitconfig = gc
			, localpath = Nothing
			, readonly = False
			, appendonly = False
			, availability = GloballyAvailable
			, remotetype = remote
			, mkUnavailable = gen r u (M.insert urlField (Proposed "http://!dne!/") rc) gc rs
			, getInfo = includeCredsInfo c (davCreds u) $
				[("url", fromMaybe "unknown" $ getRemoteConfigValue urlField c)]
			, claimUrl = Nothing
			, checkUrl = Nothing
			, remoteStateHandle = rs
			}
		chunkconfig = getChunkConfig c

webdavSetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
webdavSetup _ mu mcreds c gc = do
	u <- maybe (liftIO genUUID) return mu
	url <- maybe (giveup "Specify url=")
		(return . fromProposedAccepted)
		(M.lookup urlField c)
	(c', encsetup) <- encryptionSetup c gc
	pc <- either giveup return . parseRemoteConfig c' =<< configParser remote c'
	creds <- maybe (getCreds pc gc u) (return . Just) mcreds
	testDav url creds
	gitConfigSpecialRemote u c' [("webdav", "true")]
	c'' <- setRemoteCredPair encsetup pc gc (davCreds u) creds
	return (c'', u)

store :: DavHandleVar -> ChunkConfig -> Storer
store hv (LegacyChunks chunksize) = fileStorer $ \k f p -> 
	withDavHandle hv $ \dav -> liftIO $
		withMeteredFile f p $ storeLegacyChunked chunksize k dav
store hv _ = httpStorer $ \k reqbody -> 
	withDavHandle hv $ \dav -> liftIO $ goDAV dav $ do
		let tmp = keyTmpLocation k
		let dest = keyLocation k
		storeHelper dav tmp dest reqbody

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

retrieve :: DavHandleVar -> ChunkConfig -> Retriever
retrieve hv cc = fileRetriever $ \d k p ->
	withDavHandle hv $ \dav -> case cc of
		LegacyChunks _ -> retrieveLegacyChunked d k p dav
		_ -> liftIO $
			goDAV dav $ retrieveHelper (keyLocation k) d p

retrieveHelper :: DavLocation -> FilePath -> MeterUpdate -> DAVT IO ()
retrieveHelper loc d p = do
	debugDav $ "retrieve " ++ loc
	inLocation loc $
		withContentM $ httpBodyRetriever d p

remove :: DavHandleVar -> Remover
remove hv k = withDavHandle hv $ \dav -> liftIO $ goDAV dav $
	-- Delete the key's whole directory, including any
	-- legacy chunked files, etc, in a single action.
	removeHelper (keyDir k)

removeHelper :: DavLocation -> DAVT IO ()
removeHelper d = do
	debugDav $ "delContent " ++ d
	v <- safely $ inLocation d delContentM
	case v of
		Just _ -> return ()
		Nothing -> do
			v' <- existsDAV d
			case v' of
				Right False -> return ()
				_ -> giveup "failed to remove content from remote"

checkKey :: DavHandleVar -> Remote -> ChunkConfig -> CheckPresent
checkKey hv r chunkconfig k = withDavHandle hv $ \dav -> do
	showChecking r
	case chunkconfig of
		LegacyChunks _ -> checkKeyLegacyChunked dav k
		_ -> do
			v <- liftIO $ goDAV dav $
				existsDAV (keyLocation k)
			either giveup return v

storeExportDav :: DavHandleVar -> FilePath -> Key -> ExportLocation -> MeterUpdate -> Annex ()
storeExportDav hdl f k loc p = case exportLocation loc of
	Right dest -> withDavHandle hdl $ \h -> runExport h $ \dav -> do
		reqbody <- liftIO $ httpBodyStorer f p
		storeHelper dav (keyTmpLocation k) dest reqbody
	Left err -> giveup err

retrieveExportDav :: DavHandleVar -> Key -> ExportLocation -> FilePath -> MeterUpdate -> Annex ()
retrieveExportDav hdl  _k loc d p = case exportLocation loc of
	Right src -> withDavHandle hdl $ \h -> runExport h $ \_dav ->
		retrieveHelper src d p
	Left err -> giveup err

checkPresentExportDav :: DavHandleVar -> Remote -> Key -> ExportLocation -> Annex Bool
checkPresentExportDav hdl _ _k loc = case exportLocation loc of
	Right p -> withDavHandle hdl $ \h -> liftIO $ do
		v <- goDAV h $ existsDAV p
		either giveup return v
	Left err -> giveup err

removeExportDav :: DavHandleVar-> Key -> ExportLocation -> Annex ()
removeExportDav hdl _k loc = case exportLocation loc of
	Right p -> withDavHandle hdl $ \h -> runExport h $ \_dav ->
		removeHelper p
	-- When the exportLocation is not legal for webdav,
	-- the content is certianly not stored there, so it's ok for
	-- removal to succeed. This allows recovery after failure to store
	-- content there, as the user can rename the problem file and
	-- this will be called to make sure it's gone.
	Left _err -> return ()

removeExportDirectoryDav :: DavHandleVar -> ExportDirectory -> Annex ()
removeExportDirectoryDav hdl dir = withDavHandle hdl $ \h -> runExport h $ \_dav -> do
	let d = fromRawFilePath $ fromExportDirectory dir
	debugDav $ "delContent " ++ d
	inLocation d delContentM

renameExportDav :: DavHandleVar -> Key -> ExportLocation -> ExportLocation -> Annex (Maybe ())
renameExportDav hdl _k src dest = case (exportLocation src, exportLocation dest) of
	(Right srcl, Right destl) -> withDavHandle hdl $ \h -> 
		-- box.com's DAV endpoint has buggy handling of renames,
		-- so avoid renaming when using it.
		if boxComUrl `isPrefixOf` baseURL h 
			then return Nothing
			else runExport h $ \dav -> do
				maybe noop (void . mkColRecursive) (locationParent destl)
				moveDAV (baseURL dav) srcl destl
				return (Just ())
	(Left err, _) -> giveup err
	(_, Left err) -> giveup err

runExport :: DavHandle -> (DavHandle -> DAVT IO a) -> Annex a
runExport h a = liftIO (goDAV h (a h))

configUrl :: ParsedRemoteConfig -> Maybe URLString
configUrl c = fixup <$> getRemoteConfigValue urlField c
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

getCreds :: ParsedRemoteConfig -> RemoteGitConfig -> UUID -> Annex (Maybe CredPair)
getCreds c gc u = getRemoteCredPairFor "webdav" c gc (davCreds u)

davCreds :: UUID -> CredPairStorage
davCreds u = CredPairStorage
	{ credPairFile = fromUUID u
	, credPairEnvironment = ("WEBDAV_USERNAME", "WEBDAV_PASSWORD")
	, credPairRemoteField = davcredsField
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

type DavHandleVar = TVar (Either (Annex (Either String DavHandle)) (Either String DavHandle))

{- Prepares a DavHandle for later use. Does not connect to the server or do
 - anything else expensive. -}
mkDavHandleVar :: ParsedRemoteConfig -> RemoteGitConfig -> UUID -> Annex DavHandleVar
mkDavHandleVar c gc u = liftIO $ newTVarIO $ Left $ do
	mcreds <- getCreds c gc u
	case (mcreds, configUrl c) of
		(Just (user, pass), Just baseurl) -> do
			ctx <- mkDAVContext baseurl
			let h = DavHandle ctx (toDavUser user) (toDavPass pass) baseurl
			return (Right h)
		_ -> return $ Left "webdav credentials not available"

withDavHandle :: DavHandleVar -> (DavHandle -> Annex a) -> Annex a
withDavHandle hv a = liftIO (readTVarIO hv) >>= \case
	Right hdl -> either giveup a hdl
	Left mkhdl -> do
		hdl <- mkhdl
		liftIO $ atomically $ writeTVar hv (Right hdl)
		either giveup a hdl

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
	go (HttpExceptionRequest req (StatusCodeException response message)) = giveup $ unwords
		[ "DAV failure:"
		, show (responseStatus response)
		, show (message)
		, "HTTP request:"
		, show (HTTP.method req)
		, show (HTTP.path req)
		]
	go e = throwM e

prepDAV :: DavUser -> DavPass -> DAVT IO ()
prepDAV user pass = do
	setResponseTimeout Nothing -- disable default (5 second!) timeout
	setCreds user pass

--
-- Legacy chunking code, to be removed eventually.
--

storeLegacyChunked :: ChunkSize -> Key -> DavHandle -> L.ByteString -> IO ()
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

retrieveLegacyChunked :: FilePath -> Key -> MeterUpdate -> DavHandle -> Annex ()
retrieveLegacyChunked d k p dav = liftIO $
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
