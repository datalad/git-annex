{- WebDAV remotes.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.WebDAV (remote) where

import Network.Protocol.HTTP.DAV
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Text.XML as XML
import Data.Default

import Common.Annex
import Types.Remote
import qualified Git
import Config
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Crypto
import Creds
import Annex.Content

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
store r k _f _p = davAction r False $ \creds -> do
	error "TODO"

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
	error "TODO"
  where
	noconn = Left $ error $ name r ++ " not configured"

davAction :: Remote -> a -> (CredPair -> Annex a) -> Annex a
davAction r unconfigured action = case config r of
	Nothing -> return unconfigured
	Just c -> maybe (return unconfigured) action =<< getCreds c (uuid r)

davUrl :: String -> FilePath -> String
davUrl baseurl file = baseurl </> file

{- Test if a WebDAV store is usable, by writing to a test file, and then
 - deleting the file. Exits with an error if not. -}
testDav :: String -> Maybe CredPair -> Annex ()
testDav baseurl Nothing = error "Need to configure webdav username and password."
testDav baseurl (Just (u, p)) = do
	showSideAction "testing WebDAV server"
	liftIO $ do
		putContentAndProps testurl username password
			(dummyProps, (contentType, L.empty))
		-- TODO delete testurl
  where
	username = B8.pack u
	password = B8.pack p
	testurl = davUrl baseurl "git-annex-test"

{- Content-Type to use for files uploaded to WebDAV. -}
contentType :: Maybe B8.ByteString
contentType = Just $ B8.pack "application/octet-stream"

{- The DAV library requires that properties be specified when storing a file.
 - 
 - Also, it has a bug where if no properties are present, it generates an
 - invalid XML document, that will make putContentAndProps fail.
 - 
 - We don't really need to store any properties, so this is an
 - XML document that stores a single dummy property. -}
dummyProps :: XML.Document
dummyProps = XML.parseLBS_ def $ L8.pack
	"<D:multistatus xmlns:D=\"DAV:\"><D:response><D:propstat><D:prop><D:gitannex></D:gitannex></D:prop></D:propstat></D:response></D:multistatus>"

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
