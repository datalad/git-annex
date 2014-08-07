{- WebDAV remotes.
 -
 - Copyright 2012-2014 Joey Hess <joey@kitenet.net>
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
import qualified Control.Exception.Lifted as EL
import Network.HTTP.Client (HttpException(..))
import Network.HTTP.Types
import System.IO.Error

import Common.Annex
import Types.Remote
import qualified Git
import Config
import Config.Cost
import Remote.Helper.Special
import qualified Remote.Helper.Chunked.Legacy as Legacy
import Creds
import Utility.Metered
import Utility.Url (URLString)
import Annex.UUID
import Remote.WebDAV.DavLocation

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
	new cst = Just $ specialRemote c
		(prepareStore this chunkconfig)
		(prepareRetrieve this chunkconfig)
		(prepareRemove this)
		(prepareCheckPresent this chunkconfig)
		this
	  where
		this = Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
			storeKey = storeKeyDummy,
			retrieveKeyFile = retreiveKeyFileDummy,
			retrieveKeyFileCheap = retrieveCheap,
			removeKey = removeKeyDummy,
			checkPresent = checkPresentDummy,
			checkPresentCheap = False,
			whereisKey = Nothing,
			remoteFsck = Nothing,
			repairRepo = Nothing,
			config = c,
			repo = r,
			gitconfig = gc,
			localpath = Nothing,
			readonly = False,
			availability = GloballyAvailable,
			remotetype = remote
		}
		chunkconfig = getChunkConfig c

webdavSetup :: Maybe UUID -> Maybe CredPair -> RemoteConfig -> Annex (RemoteConfig, UUID)
webdavSetup mu mcreds c = do
	u <- maybe (liftIO genUUID) return mu
	let url = fromMaybe (error "Specify url=") $
		M.lookup "url" c
	c' <- encryptionSetup c
	creds <- maybe (getCreds c' u) (return . Just) mcreds
	testDav url creds
	gitConfigSpecialRemote u c' "webdav" "true"
	c'' <- setRemoteCredPair c' (davCreds u) creds
	return (c'', u)

prepareStore :: Remote -> ChunkConfig -> Preparer Storer
prepareStore r chunkconfig = simplyPrepare $ fileStorer $ \k f p ->
	davAction r False $ \(baseurl, user, pass) -> liftIO $
		withMeteredFile f p $
			storeHelper chunkconfig k baseurl user pass

storeHelper :: ChunkConfig -> Key -> URLString -> DavUser -> DavPass -> L.ByteString -> IO Bool
storeHelper chunkconfig k baseurl user pass b = do
	case chunkconfig of
		LegacyChunks chunksize -> do
			let storehttp l b' = do
				void $ goDAV baseurl user pass $ do
					maybe noop (void . mkColRecursive) (locationParent l)
					inLocation l $ putContentM (contentType, b')
			let storer locs = Legacy.storeChunked chunksize locs storehttp b
			let recorder l s = storehttp l (L8.fromString s)
			let finalizer tmp' dest' = goDAV baseurl user pass $ 
				finalizeStore baseurl tmp' (fromJust $ locationParent dest')
			Legacy.storeChunks k tmp dest storer recorder finalizer
		_ -> goDAV baseurl user pass $ do
			void $ mkColRecursive tmpDir
			inLocation tmp $
				putContentM (contentType, b)
			finalizeStore baseurl tmp dest
			return True
  where
	tmp = keyTmpLocation k
	dest = keyLocation k ++ keyFile k

finalizeStore :: URLString -> DavLocation -> DavLocation -> DAVT IO ()
finalizeStore baseurl tmp dest = do
	inLocation dest $ void $ safely $ delContentM
	maybe noop (void . mkColRecursive) (locationParent dest)
	moveDAV baseurl tmp dest

retrieveCheap :: Key -> FilePath -> Annex Bool
retrieveCheap _ _ = return False

prepareRetrieve :: Remote -> ChunkConfig -> Preparer Retriever
prepareRetrieve r chunkconfig = simplyPrepare $ fileRetriever $ \d k p ->
	davAction r onerr $ \(baseurl, user, pass) -> liftIO $
		withStoredFiles chunkconfig k baseurl user pass onerr $ \locs -> do
			Legacy.meteredWriteFileChunks p d locs $ \l -> do
				mb <- goDAV baseurl user pass $ safely $
					inLocation l $
						snd <$> getContentM
				case mb of
					Nothing -> onerr
					Just b -> return b
  where
	onerr = error "download failed"

prepareRemove :: Remote -> Preparer Remover
prepareRemove r = simplyPrepare $ \k ->
	davAction r False $ \(baseurl, user, pass) -> liftIO $ do
		-- Delete the key's whole directory, including any
		-- legacy chunked files, etc, in a single action.
		ret <- goDAV baseurl user pass $ safely $
			inLocation (keyLocation k) delContentM
		return (isJust ret)

prepareCheckPresent :: Remote -> ChunkConfig -> Preparer CheckPresent
prepareCheckPresent r chunkconfig = simplyPrepare $ checkKey r chunkconfig

checkKey :: Remote -> ChunkConfig -> Key -> Annex Bool
checkKey r chunkconfig k = davAction r noconn (either error id <$$> go)
  where
	noconn = error $ name r ++ " not configured"

	go (baseurl, user, pass) = do
		showAction $ "checking " ++ name r
		liftIO $ withStoredFiles chunkconfig k baseurl user pass onerr check
	  where
		check [] = return $ Right True
		check (l:ls) = do
			v <- goDAV baseurl user pass $ existsDAV l
			if v == Right True
				then check ls
				else return v

		{- Failed to read the chunkcount file; see if it's missing,
		 - or if there's a problem accessing it,
		- or perhaps this was an intermittent error. -}
		onerr f = do
			v <- goDAV baseurl user pass $ existsDAV f
			return $ if v == Right True
				then Left $ "failed to read " ++ f
				else v

withStoredFiles
	:: ChunkConfig
	-> Key
	-> URLString
	-> DavUser
	-> DavPass
	-> (DavLocation -> IO a)
	-> ([DavLocation] -> IO a)
	-> IO a
withStoredFiles chunkconfig k baseurl user pass onerr a = case chunkconfig of
	LegacyChunks _ -> do
		let chunkcount = keyloc ++ Legacy.chunkCount
		v <- goDAV baseurl user pass $ safely $ 
			inLocation chunkcount $
				snd <$> getContentM
		case v of
			Just s -> a $ Legacy.listChunks keyloc $ L8.toString s
			Nothing -> do
				chunks <- Legacy.probeChunks keyloc $ \f ->
					(== Right True) <$> goDAV baseurl user pass (existsDAV f)
				if null chunks
					then onerr chunkcount
					else a chunks
	_ -> a [keyloc]
  where
	keyloc = keyLocation k ++ keyFile k

davAction :: Remote -> a -> ((DavLocation, DavUser, DavPass) -> Annex a) -> Annex a
davAction r unconfigured action = do
	mcreds <- getCreds (config r) (uuid r)
	case (mcreds, configUrl r) of
		(Just (user, pass), Just url) ->
			action (url, toDavUser user, toDavPass pass)
		_ -> return unconfigured

configUrl :: Remote -> Maybe URLString
configUrl r = fixup <$> M.lookup "url" (config r)
  where
	-- box.com DAV url changed
	fixup = replace "https://www.box.com/dav/" "https://dav.box.com/dav/"

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
	showSideAction "testing WebDAV server"
	test $ liftIO $ goDAV url user pass $ do
		makeParentDirs
		inLocation tmpDir $ void mkCol
		inLocation (tmpLocation "git-annex-test") $ do
			putContentM (Nothing, L.empty)
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
existsDAV l = inLocation l check `EL.catch` (\(e :: EL.SomeException) -> return (Left $ show e))
  where
	check = do
		setDepth Nothing
		EL.catchJust
			(matchStatusCodeException notFound404)
			(getPropsM >> ispresent True)
			(const $ ispresent False)
	ispresent = return . Right

matchStatusCodeException :: Status -> HttpException -> Maybe ()
matchStatusCodeException want (StatusCodeException s _ _)
	| s == want = Just ()
	| otherwise = Nothing
matchStatusCodeException _ _ = Nothing

-- Ignores any exceptions when performing a DAV action.
safely :: DAVT IO a -> DAVT IO (Maybe a)
safely a = (Just <$> a)
	`EL.catch` (\(_ :: EL.SomeException) -> return Nothing)

goDAV :: URLString -> DavUser -> DavPass -> DAVT IO a -> IO a
goDAV url user pass a = choke $ evalDAVT url $ do
	setResponseTimeout Nothing -- disable default (5 second!) timeout
	setCreds user pass
	a
  where
	choke :: IO (Either String a) -> IO a
	choke f = do
		x <- f
		case x of
			Left e -> error e
			Right r -> return r
