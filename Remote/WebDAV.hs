{- WebDAV remotes.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.WebDAV (remote) where

import Network.Protocol.HTTP.DAV
import qualified Data.Map as M
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Text.XML as XML

import Common.Annex
import Types.Remote
import qualified Git
import Config
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Crypto
import Creds
import Annex.Content

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
	davAction r False $ \(baseurl, user, pass) -> liftIO $ catchBoolIO $ do
		content <- L.readFile f
		let url = Prelude.head $ davLocations baseurl k
		putContentAndProps url user pass
			(noProps, (contentType, content))
		return True

storeEncrypted :: Remote -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted r (cipher, enck) k _p = davAction r False $ \creds -> do
	error "TODO"

retrieve :: Remote -> Key -> AssociatedFile -> FilePath -> Annex Bool
retrieve r k _f d = davAction r False $ \creds -> do
	error "TODO"

retrieveCheap :: Remote -> Key -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

retrieveEncrypted :: Remote -> (Cipher, Key) -> Key -> FilePath -> Annex Bool
retrieveEncrypted r (cipher, enck) _ f = davAction r False $ \creds -> do
	error "TODO"

remove :: Remote -> Key -> Annex Bool
remove r k = davAction r False $ \creds -> do
	error "TODO"

checkPresent :: Remote -> Key -> Annex (Either String Bool)
checkPresent r k = davAction r noconn $ \creds -> do
	showAction $ "checking " ++ name r
	return $ Right False
	--error "TODO"
  where
	noconn = Left $ error $ name r ++ " not configured"

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

{- FIXME: Replacing / with _ to avoid needing collections. -}
davUrl :: DavUrl -> FilePath -> DavUrl
davUrl baseurl file = baseurl </> replace "/" "_" file

{- Test if a WebDAV store is usable, by writing to a test file, and then
 - deleting the file. Exits with an error if not. -}
testDav :: String -> Maybe CredPair -> Annex ()
testDav baseurl Nothing = error "Need to configure webdav username and password."
testDav baseurl (Just (u, p)) = do
	showSideAction "testing WebDAV server"
	liftIO $ do
		putContentAndProps testurl username password
			(noProps, (contentType, L.empty))
		deleteContent testurl username password
  where
	username = toDavUser u
	password = toDavPass p
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
