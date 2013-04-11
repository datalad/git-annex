{- Amazon S3 remotes.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.S3 (remote) where

import Network.AWS.AWSConnection
import Network.AWS.S3Object
import Network.AWS.S3Bucket hiding (size)
import Network.AWS.AWSResult
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import Data.Char

import Common.Annex
import Types.Remote
import Types.Key
import qualified Git
import Config
import Config.Cost
import Remote.Helper.Special
import Remote.Helper.Encryptable
import qualified Remote.Helper.AWS as AWS
import Crypto
import Creds
import Utility.Metered
import Annex.Content

remote :: RemoteType
remote = RemoteType {
	typename = "S3",
	enumerate = findSpecialRemotes "s3",
	generate = gen,
	setup = s3Setup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex Remote
gen r u c gc = new <$> remoteCost gc expensiveRemoteCost
  where
	new cst = encryptableRemote c
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
			gitconfig = gc,
			localpath = Nothing,
			readonly = False,
			globallyAvailable = True,
			remotetype = remote
		}

s3Setup :: UUID -> RemoteConfig -> Annex RemoteConfig
s3Setup u c = handlehost $ M.lookup "host" c
  where
	remotename = fromJust (M.lookup "name" c)
	defbucket = remotename ++ "-" ++ fromUUID u
	defaults = M.fromList
		[ ("datacenter", T.unpack $ AWS.defaultRegion AWS.S3)
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
		setRemoteCredPair fullconfig (AWS.creds u)

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
store r k _f p = s3Action r False $ \(conn, bucket) -> 
	sendAnnex k (void $ remove r k) $ \src -> do
		res <- storeHelper (conn, bucket) r k p src
		s3Bool res

storeEncrypted :: Remote -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted r (cipher, enck) k p = s3Action r False $ \(conn, bucket) -> 
	-- To get file size of the encrypted content, have to use a temp file.
	-- (An alternative would be chunking to to a constant size.)
	withTmp enck $ \tmp -> sendAnnex k (void $ remove r enck) $ \src -> do
		liftIO $ encrypt (getGpgOpts r) cipher (feedFile src) $
			readBytes $ L.writeFile tmp
		res <- storeHelper (conn, bucket) r enck p tmp
		s3Bool res

storeHelper :: (AWSConnection, String) -> Remote -> Key -> MeterUpdate -> FilePath -> Annex (AWSResult ())
storeHelper (conn, bucket) r k p file = do
	size <- maybe getsize (return . fromIntegral) $ keySize k
	meteredBytes (Just p) size $ \meterupdate ->
		liftIO $ withMeteredFile file meterupdate $ \content -> do
			-- size is provided to S3 so the whole content
			-- does not need to be buffered to calculate it
			let object = setStorageClass storageclass $ S3Object
				bucket (bucketFile r k) ""
				(("Content-Length", show size) : xheaders)
				content
			sendObject conn object
  where
	storageclass =
		case fromJust $ M.lookup "storageclass" $ config r of
			"REDUCED_REDUNDANCY" -> REDUCED_REDUNDANCY
			_ -> STANDARD

	getsize = liftIO $ fromIntegral . fileSize <$> getFileStatus file
	
	xheaders = filter isxheader $ M.assocs $ config r
	isxheader (h, _) = "x-amz-" `isPrefixOf` h

retrieve :: Remote -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
retrieve r k _f d p = s3Action r False $ \(conn, bucket) ->
	metered (Just p) k $ \meterupdate -> do
		res <- liftIO $ getObject conn $ bucketKey r bucket k
		case res of
			Right o -> do
				liftIO $ meteredWriteFile meterupdate d $
					obj_data o
				return True
			Left e -> s3Warning e

retrieveCheap :: Remote -> Key -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

retrieveEncrypted :: Remote -> (Cipher, Key) -> Key -> FilePath -> MeterUpdate -> Annex Bool
retrieveEncrypted r (cipher, enck) k d p = s3Action r False $ \(conn, bucket) ->
	metered (Just p) k $ \meterupdate -> do
		res <- liftIO $ getObject conn $ bucketKey r bucket enck
		case res of
			Right o -> liftIO $ decrypt cipher (\h -> meteredWrite meterupdate h $ obj_data o) $ 
				readBytes $ \content -> do
					L.writeFile d content
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
	let bucket = M.lookup "bucket" $ config r
	conn <- s3Connection (config r) (uuid r)
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
	c = config r

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
s3Connection c u = go =<< getRemoteCredPairFor "S3" c (AWS.creds u)
  where
	go Nothing = return Nothing
	go (Just (ak, sk)) = return $ Just $ AWSConnection host port ak sk

	host = fromJust $ M.lookup "host" c
	port = let s = fromJust $ M.lookup "port" c in
		case reads s of
		[(p, _)] -> p
		_ -> error $ "bad S3 port value: " ++ s
