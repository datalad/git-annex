{- WebDAV remotes.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ScopedTypeVariables #-}

module Remote.WebDAV (remote) where

import Network.Protocol.HTTP.DAV
import qualified Data.Map as M
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Text.XML as XML
import Network.URI (normalizePathSegments)
import qualified Control.Exception as E
import Network.HTTP.Conduit (HttpException(..))
import Network.HTTP.Types

import Common.Annex
import Types.Remote
import qualified Git
import Config
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Crypto
import Creds

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

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex Remote
gen r u c = do
	cst <- remoteCost r expensiveRemoteCost
	return $ gen' r u c cst
gen' :: Git.Repo -> UUID -> Maybe RemoteConfig -> Int -> Remote
gen' r u c cst =
	encryptableRemote c
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
		config = c,
		repo = r,
		localpath = Nothing,
		readonly = False,
		remotetype = remote
	}

webdavSetup :: UUID -> RemoteConfig -> Annex RemoteConfig
webdavSetup u c = do
	let url = fromMaybe (error "Specify url=") $
		M.lookup "url" c
	c' <- encryptionSetup c
	creds <- getCreds c' u
	testDav url creds
	gitConfigSpecialRemote u c' "webdav" "true"
	setRemoteCredPair c (davCreds u)

store :: Remote -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store r k _f _p = do
	f <- inRepo $ gitAnnexLocation k
	davAction r False $ \(baseurl, user, pass) -> liftIO $ do
		let url = Prelude.head $ davLocations baseurl k
		davMkdir (urlParent url) user pass
		b <- L.readFile f
		v <- catchMaybeHttp $ putContentAndProps url user pass
			(noProps, (contentType, b))
		return $ isJust v

storeEncrypted :: Remote -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted r (cipher, enck) k _p = davAction r False $ \creds -> liftIO $ do
	error "TODO"

retrieve :: Remote -> Key -> AssociatedFile -> FilePath -> Annex Bool
retrieve r k _f d = davAction r False $ liftIO . go
  where
	go (baseurl, user, pass) = get $ davLocations baseurl k
	  where
		get [] = return False
		get (u:urls) = maybe (get urls) save
			=<< catchMaybeHttp (getPropsAndContent u user pass)
		save (_, (_, b)) = do
			L.writeFile d b
			return True

retrieveCheap :: Remote -> Key -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

retrieveEncrypted :: Remote -> (Cipher, Key) -> Key -> FilePath -> Annex Bool
retrieveEncrypted r (cipher, enck) _ f = davAction r False $ \creds -> do
	error "TODO"

remove :: Remote -> Key -> Annex Bool
remove r k = davAction r False $ liftIO . go
  where
	go (baseurl, user, pass) = delone $ davLocations baseurl k
	  where
		delone [] = return False
		delone (u:urls) = maybe (delone urls) (const $ return True)
			=<< catchMaybeHttp (deletedir u)

		{- Rather than deleting first the file, and then its
		 - immediate parent directory (to clean up), delete the
		 - parent directory, along with all its contents in a
		 - single recursive DAV call.
		 - 
		 - The file is the only thing we keep in there, and this
		 - is faster. -}
		deletedir u = deleteContent (urlParent u) user pass

checkPresent :: Remote -> Key -> Annex (Either String Bool)
checkPresent r k = davAction r noconn go
  where
	noconn = Left $ error $ name r ++ " not configured"
	go (baseurl, user, pass) = do
		showAction $ "checking " ++ name r
		liftIO $ check $ davLocations baseurl k	
	  where
		check [] = return $ Right False
		check (u:urls) = do
			v <- catchHttp $ getProps u user pass
			case v of
				Right _ -> return $ Right True
				Left (Left (StatusCodeException status _))
					| statusCode status == statusCode notFound404 -> check urls
					| otherwise -> return $ Left $ show $ statusMessage status
				Left (Left httpexception) -> return $ Left $ show httpexception
				Left (Right ioexception) -> return $ Left $ show ioexception

davAction :: Remote -> a -> ((DavUrl, DavUser, DavPass) -> Annex a) -> Annex a
davAction r unconfigured action = case config r of
	Nothing -> return unconfigured
	Just c -> do
		mcreds <- getCreds c (uuid r)
		case (mcreds, M.lookup "url" c) of
			(Just (user, pass), Just url) ->
				action (url, toDavUser user, toDavPass pass)
			_ -> return unconfigured

toDavUser :: String -> DavUser
toDavUser = B8.fromString

toDavPass :: String -> DavPass
toDavPass = B8.fromString

{- All possibile locations to try to access a given Key.
 -
 - This is intentially the same as the directory special remote uses, to
 - allow interoperability. -}
davLocations :: DavUrl -> Key -> [DavUrl]
davLocations baseurl k = map (davUrl baseurl) (keyPaths k)

davUrl :: DavUrl -> FilePath -> DavUrl
davUrl baseurl file = baseurl </> file

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
catchHttp :: IO a -> IO (Either (Either HttpException E.IOException) a)
catchHttp a = (Right <$> a) `E.catches`
	[ E.Handler $ \(e :: HttpException) -> return $ Left $ Left e
	, E.Handler $ \(e :: E.IOException) -> return $ Left $ Right e
	]

urlParent :: DavUrl -> DavUrl
urlParent url = reverse $ dropWhile (== '/') $ reverse $
	normalizePathSegments (url ++ "/..")

{- Test if a WebDAV store is usable, by writing to a test file, and then
 - deleting the file. Exits with an error if not. -}
testDav :: String -> Maybe CredPair -> Annex ()
testDav baseurl Nothing = error "Need to configure webdav username and password."
testDav baseurl (Just (u, p)) = do
	showSideAction "testing WebDAV server"
	liftIO $ do
		davMkdir baseurl user pass
		putContentAndProps testurl user pass
			(noProps, (contentType, L.empty))
		deleteContent testurl user pass
  where
	user = toDavUser u
	pass = toDavPass p
	testurl = davUrl baseurl "git-annex-test"

{- Content-Type to use for files uploaded to WebDAV. -}
contentType :: Maybe B8.ByteString
contentType = Just $ B8.fromString "application/octet-stream"

{- The DAV library requires that properties be specified when storing a file.
 - This just omits any real properties. -}
noProps :: XML.Document
noProps = XML.Document (XML.Prologue [] Nothing []) root []
  where
    root = XML.Element (XML.Name (T.pack "propertyupdate") Nothing Nothing) [] []

getCreds :: RemoteConfig -> UUID -> Annex (Maybe CredPair)
getCreds c u = maybe missing (return . Just) =<< getRemoteCredPair c creds
  where
	creds = davCreds u
	(loginvar, passwordvar) = credPairEnvironment creds
	missing = do
		warning $ "Set both " ++ loginvar ++ " and " ++ passwordvar ++ " to use webdav"
		return Nothing

davCreds :: UUID -> CredPairStorage
davCreds u = CredPairStorage
        { credPairFile = fromUUID u
        , credPairEnvironment = ("WEBDAV_USERNAME", "WEBDAV_PASSWORD")
        , credPairRemoteKey = Just "davcreds"
        }
