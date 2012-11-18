{- Amazon S3 remotes.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.S3 (remote, setCredsEnv) where

import Network.AWS.AWSConnection
import Network.AWS.S3Object
import Network.AWS.S3Bucket hiding (size)
import Network.AWS.AWSResult
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import Data.Char

import Common.Annex
import Types.Remote
import Types.Key
import qualified Git
import Config
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Crypto
import Creds
import Annex.Content

remote :: RemoteType
remote = RemoteType {
	typename = "S3",
	enumerate = findSpecialRemotes "s3",
	generate = gen,
	setup = s3Setup
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

s3Setup :: UUID -> RemoteConfig -> Annex RemoteConfig
s3Setup u c = handlehost $ M.lookup "host" c
  where
	remotename = fromJust (M.lookup "name" c)
	defbucket = remotename ++ "-" ++ fromUUID u
	defaults = M.fromList
		[ ("datacenter", "US")
		, ("storageclass", "STANDARD")
		, ("host", defaultAmazonS3Host)
		, ("port", show defaultAmazonS3Port)
		, ("bucket", defbucket)
		]
		
	handlehost Nothing = defaulthost
	handlehost (Just h)
		| ".archive.org" `isSuffixOf` map toLower h = archiveorg
		| otherwise = defaulthost

	use fullconfig = do
		gitConfigSpecialRemote u fullconfig "s3" "true"
		setRemoteCredPair fullconfig (s3Creds u)

	defaulthost = do
		c' <- encryptionSetup c
		let fullconfig = c' `M.union` defaults
		genBucket fullconfig u
		use fullconfig

	archiveorg = do
		showNote "Internet Archive mode"
		maybe (error "specify bucket=") (const noop) $
			M.lookup "bucket" archiveconfig
		use archiveconfig
	  where
		archiveconfig =
			-- hS3 does not pass through x-archive-* headers
			M.mapKeys (replace "x-archive-" "x-amz-") $
			-- encryption does not make sense here
			M.insert "encryption" "none" $
			M.union c $
			-- special constraints on key names
			M.insert "mungekeys" "ia" $
			-- bucket created only when files are uploaded
			M.insert "x-amz-auto-make-bucket" "1" $
			-- no default bucket name; should be human-readable
			M.delete "bucket" defaults

store :: Remote -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store r k _f _p = s3Action r False $ \(conn, bucket) -> do
	src <- inRepo $ gitAnnexLocation k
	res <- liftIO $ storeHelper (conn, bucket) r k src
	s3Bool res

storeEncrypted :: Remote -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted r (cipher, enck) k _p = s3Action r False $ \(conn, bucket) -> 
	-- To get file size of the encrypted content, have to use a temp file.
	-- (An alternative would be chunking to to a constant size.)
	withTmp enck $ \tmp -> do
		f <- inRepo $ gitAnnexLocation k
		liftIO $ encrypt cipher (feedFile f) $
			readBytes $ L.writeFile tmp
		res <- liftIO $ storeHelper (conn, bucket) r enck tmp
		s3Bool res

storeHelper :: (AWSConnection, String) -> Remote -> Key -> FilePath -> IO (AWSResult ())
storeHelper (conn, bucket) r k file = do
	content <- liftIO $ L.readFile file
	-- size is provided to S3 so the whole content does not need to be
	-- buffered to calculate it
	size <- maybe getsize (return . fromIntegral) $ keySize k
	let object = setStorageClass storageclass $ 
		S3Object bucket (bucketFile r k) ""
			(("Content-Length", show size) : xheaders) content
	sendObject conn object
  where
	storageclass =
		case fromJust $ M.lookup "storageclass" $ fromJust $ config r of
			"REDUCED_REDUNDANCY" -> REDUCED_REDUNDANCY
			_ -> STANDARD
	getsize = fileSize <$> (liftIO $ getFileStatus file)
	
	xheaders = filter isxheader $ M.assocs $ fromJust $ config r
	isxheader (h, _) = "x-amz-" `isPrefixOf` h

retrieve :: Remote -> Key -> AssociatedFile -> FilePath -> Annex Bool
retrieve r k _f d = s3Action r False $ \(conn, bucket) -> do
	res <- liftIO $ getObject conn $ bucketKey r bucket k
	case res of
		Right o -> do
			liftIO $ L.writeFile d $ obj_data o
			return True
		Left e -> s3Warning e

retrieveCheap :: Remote -> Key -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

retrieveEncrypted :: Remote -> (Cipher, Key) -> Key -> FilePath -> Annex Bool
retrieveEncrypted r (cipher, enck) _ f = s3Action r False $ \(conn, bucket) -> do
	res <- liftIO $ getObject conn $ bucketKey r bucket enck
	case res of
		Right o -> liftIO $ decrypt cipher (feedBytes $ obj_data o) $ 
			readBytes $ \content -> do
				L.writeFile f content
				return True
		Left e -> s3Warning e

remove :: Remote -> Key -> Annex Bool
remove r k = s3Action r False $ \(conn, bucket) -> do
	res <- liftIO $ deleteObject conn $ bucketKey r bucket k
	s3Bool res

checkPresent :: Remote -> Key -> Annex (Either String Bool)
checkPresent r k = s3Action r noconn $ \(conn, bucket) -> do
	showAction $ "checking " ++ name r
	res <- liftIO $ getObjectInfo conn $ bucketKey r bucket k
	case res of
		Right _ -> return $ Right True
		Left (AWSError _ _) -> return $ Right False
		Left e -> return $ Left (s3Error e)
  where
	noconn = Left $ error "S3 not configured"
			
s3Warning :: ReqError -> Annex Bool
s3Warning e = do
	warning $ prettyReqError e
	return False

s3Error :: ReqError -> a
s3Error e = error $ prettyReqError e

s3Bool :: AWSResult () -> Annex Bool
s3Bool (Right _) = return True
s3Bool (Left e) = s3Warning e

s3Action :: Remote -> a -> ((AWSConnection, String) -> Annex a) -> Annex a
s3Action r noconn action = do
	when (isNothing $ config r) $
		error $ "Missing configuration for special remote " ++ name r
	let bucket = M.lookup "bucket" $ fromJust $ config r
	conn <- s3Connection (fromJust $ config r) (uuid r)
	case (bucket, conn) of
		(Just b, Just c) -> action (c, b)
		_ -> return noconn

bucketFile :: Remote -> Key -> FilePath
bucketFile r = munge . key2file
  where
	munge s = case M.lookup "mungekeys" c of
		Just "ia" -> iaMunge $ fileprefix ++ s
		_ -> fileprefix ++ s
	fileprefix = M.findWithDefault "" "fileprefix" c
	c = fromJust $ config r

bucketKey :: Remote -> String -> Key -> S3Object
bucketKey r bucket k = S3Object bucket (bucketFile r k) "" [] L.empty

{- Internet Archive limits filenames to a subset of ascii,
 - with no whitespace. Other characters are xml entity
 - encoded. -}
iaMunge :: String -> String
iaMunge = (>>= munge)
  where
	munge c
		| isAsciiUpper c || isAsciiLower c || isNumber c = [c]
		| c `elem` "_-.\"" = [c]
		| isSpace c = []
		| otherwise = "&" ++ show (ord c) ++ ";"

genBucket :: RemoteConfig -> UUID -> Annex ()
genBucket c u = do
	conn <- s3ConnectionRequired c u
	showAction "checking bucket"
	loc <- liftIO $ getBucketLocation conn bucket 
	case loc of
		Right _ -> noop
		Left err@(NetworkError _) -> s3Error err
		Left (AWSError _ _) -> do
			showAction $ "creating bucket in " ++ datacenter
			res <- liftIO $ createBucketIn conn bucket datacenter
			case res of
				Right _ -> noop
				Left err -> s3Error err
  where
	bucket = fromJust $ M.lookup "bucket" c
	datacenter = fromJust $ M.lookup "datacenter" c

s3ConnectionRequired :: RemoteConfig -> UUID -> Annex AWSConnection
s3ConnectionRequired c u =
	maybe (error "Cannot connect to S3") return =<< s3Connection c u

s3Connection :: RemoteConfig -> UUID -> Annex (Maybe AWSConnection)
s3Connection c u = go =<< getRemoteCredPair c creds
  where
	go Nothing = do
		warning $ "Set both " ++ s3AccessKey ++ " and " ++ s3SecretKey  ++ " to use S3"
		return Nothing
	go (Just (ak, sk)) = return $ Just $ AWSConnection host port ak sk

	creds = s3Creds u
	(s3AccessKey, s3SecretKey) = credPairEnvironment creds

	host = fromJust $ M.lookup "host" c
	port = let s = fromJust $ M.lookup "port" c in
		case reads s of
		[(p, _)] -> p
		_ -> error $ "bad S3 port value: " ++ s

s3Creds :: UUID -> CredPairStorage
s3Creds u = CredPairStorage
	{ credPairFile = fromUUID u
	, credPairEnvironment = ("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY")
	, credPairRemoteKey = Just "s3creds"
	}

setCredsEnv :: (String, String) -> IO ()
setCredsEnv creds = setEnvCredPair creds $ s3Creds undefined
