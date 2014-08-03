{- S3 remotes
 -
 - Copyright 2011-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.S3 (remote, iaHost, isIA, isIAHost, iaItemUrl) where

import Network.AWS.AWSConnection
import Network.AWS.S3Object hiding (getStorageClass)
import Network.AWS.S3Bucket hiding (size)
import Network.AWS.AWSResult
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Char
import Network.Socket (HostName)

import Common.Annex
import Types.Remote
import Types.Key
import qualified Git
import Config
import Config.Cost
import Remote.Helper.Special
import qualified Remote.Helper.AWS as AWS
import Creds
import Utility.Metered
import Annex.UUID
import Logs.Web

type Bucket = String

remote :: RemoteType
remote = RemoteType {
	typename = "S3",
	enumerate = findSpecialRemotes "s3",
	generate = gen,
	setup = s3Setup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = new <$> remoteCost gc expensiveRemoteCost
  where
	new cst = Just $ specialRemote c
		(prepareStore this)
		(prepareRetrieve this)
		this
	  where
		this = Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
			storeKey = storeKeyDummy,
			retrieveKeyFile = retreiveKeyFileDummy,
			retrieveKeyFileCheap = retrieveCheap this,
			removeKey = remove this c,
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
			availability = GloballyAvailable,
			remotetype = remote
		}

s3Setup :: Maybe UUID -> Maybe CredPair -> RemoteConfig -> Annex (RemoteConfig, UUID)
s3Setup mu mcreds c = do
	u <- maybe (liftIO genUUID) return mu
	c' <- setRemoteCredPair c (AWS.creds u) mcreds
	s3Setup' u c'
s3Setup' :: UUID -> RemoteConfig -> Annex (RemoteConfig, UUID)
s3Setup' u c = if isIA c then archiveorg else defaulthost
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
		
	use fullconfig = do
		gitConfigSpecialRemote u fullconfig "s3" "true"
		return (fullconfig, u)

	defaulthost = do
		c' <- encryptionSetup c
		let fullconfig = c' `M.union` defaults
		genBucket fullconfig u
		use fullconfig

	archiveorg = do
		showNote "Internet Archive mode"
		-- Ensure user enters a valid bucket name, since
		-- this determines the name of the archive.org item.
		let bucket = replace " " "-" $ map toLower $
			fromMaybe (error "specify bucket=") $
				getBucket c
		let archiveconfig = 
			-- hS3 does not pass through x-archive-* headers
			M.mapKeys (replace "x-archive-" "x-amz-") $
			-- encryption does not make sense here
			M.insert "encryption" "none" $
			M.insert "bucket" bucket $
			M.union c $
			-- special constraints on key names
			M.insert "mungekeys" "ia" $
			-- bucket created only when files are uploaded
			M.insert "x-amz-auto-make-bucket" "1" defaults
		writeUUIDFile archiveconfig u
		use archiveconfig

prepareStore :: Remote -> Preparer Storer
prepareStore r = resourcePrepare (const $ s3Action r False) $ \(conn, bucket) -> 
	fileStorer $ \k src p -> do
		ok <- s3Bool =<< liftIO (store (conn, bucket) r k p src)

		-- Store public URL to item in Internet Archive.
		when (ok && isIA (config r) && not (isChunkKey k)) $
			setUrlPresent k (iaKeyUrl r k)

		return ok

store :: (AWSConnection, Bucket) -> Remote -> Key -> MeterUpdate -> FilePath -> IO (AWSResult ())
store (conn, bucket) r k p file = do
	size <- (fromIntegral . fileSize <$> getFileStatus file) :: IO Integer
	withMeteredFile file p $ \content -> do
		-- size is provided to S3 so the whole content
		-- does not need to be buffered to calculate it
		let object = S3Object
			bucket (bucketFile r k) ""
			(("Content-Length", show size) : getXheaders (config r))
			content
		sendObject conn $
			setStorageClass (getStorageClass $ config r) object

prepareRetrieve :: Remote -> Preparer Retriever
prepareRetrieve r = resourcePrepare (const $ s3Action r False) $ \(conn, bucket) ->
	byteRetriever $ \k sink ->
		liftIO (getObject conn $ bucketKey r bucket k)
			>>= either s3Error (sink . obj_data)

retrieveCheap :: Remote -> Key -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

{- Internet Archive doesn't easily allow removing content.
 - While it may remove the file, there are generally other files
 - derived from it that it does not remove. -}
remove :: Remote -> RemoteConfig -> Key -> Annex Bool
remove r c k
	| isIA c = do
		warning "Cannot remove content from the Internet Archive"
		return False
	| otherwise = remove' r k

remove' :: Remote -> Key -> Annex Bool
remove' r k = s3Action r False $ \(conn, bucket) ->
	s3Bool =<< liftIO (deleteObject conn $ bucketKey r bucket k)

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

s3Action :: Remote -> a -> ((AWSConnection, Bucket) -> Annex a) -> Annex a
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
		Just "ia" -> iaMunge $ filePrefix c ++ s
		_ -> filePrefix c ++ s
	c = config r

filePrefix :: RemoteConfig -> String
filePrefix = M.findWithDefault "" "fileprefix"

bucketKey :: Remote -> Bucket -> Key -> S3Object
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

{- Generate the bucket if it does not already exist, including creating the
 - UUID file within the bucket.
 -
 - To check if the bucket exists, ask for its location. However, some ACLs
 - can allow read/write to buckets, but not querying location, so first
 - check if the UUID file already exists and we can skip doing anything.
 -}
genBucket :: RemoteConfig -> UUID -> Annex ()
genBucket c u = do
	conn <- s3ConnectionRequired c u
	showAction "checking bucket"
	unlessM ((== Right True) <$> checkUUIDFile c u conn) $ do
		loc <- liftIO $ getBucketLocation conn bucket
		case loc of
			Right _ -> writeUUIDFile c u
			Left err@(NetworkError _) -> s3Error err
			Left (AWSError _ _) -> do
				showAction $ "creating bucket in " ++ datacenter
				res <- liftIO $ createBucketIn conn bucket datacenter
				case res of
					Right _ -> writeUUIDFile c u
					Left err -> s3Error err
  where
	bucket = fromJust $ getBucket c
	datacenter = fromJust $ M.lookup "datacenter" c

{- Writes the UUID to an annex-uuid file within the bucket.
 -
 - If the file already exists in the bucket, it must match.
 -
 - Note that IA items do not get created by createBucketIn. 
 - Rather, they are created the first time a file is stored in them.
 - So this also takes care of that.
 -}
writeUUIDFile :: RemoteConfig -> UUID -> Annex ()
writeUUIDFile c u = do
	conn <- s3ConnectionRequired c u
	v <- checkUUIDFile  c u conn
	case v of
		Left e -> error e
		Right True -> return ()
		Right False -> do
			let object = setStorageClass (getStorageClass c) (mkobject uuidb)
			either s3Error return =<< liftIO (sendObject conn object)
  where
	file = uuidFile c
	uuidb = L.fromChunks [T.encodeUtf8 $ T.pack $ fromUUID u]
	bucket = fromJust $ getBucket c

	mkobject = S3Object bucket file "" (getXheaders c)

{- Checks if the UUID file exists in the bucket and has the specified UUID already. -}
checkUUIDFile :: RemoteConfig -> UUID -> AWSConnection -> Annex (Either String Bool)
checkUUIDFile c u conn = check <$> liftIO (tryNonAsync $ getObject conn $ mkobject L.empty)
  where
	check (Right (Right o))
		| obj_data o == uuidb = Right True
		| otherwise = Left $ "This bucket is already in use by a different S3 special remote, with UUID: " ++ show (obj_data o)
	check _ = Right False

	uuidb = L.fromChunks [T.encodeUtf8 $ T.pack $ fromUUID u]
	bucket = fromJust $ getBucket c
	file = uuidFile c

	mkobject = S3Object bucket file "" (getXheaders c)

uuidFile :: RemoteConfig -> FilePath
uuidFile c = filePrefix c ++ "annex-uuid"

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

getBucket :: RemoteConfig -> Maybe Bucket
getBucket = M.lookup "bucket"

getStorageClass :: RemoteConfig -> StorageClass
getStorageClass c = case fromJust $ M.lookup "storageclass" c of
	"REDUCED_REDUNDANCY" -> REDUCED_REDUNDANCY
	_ -> STANDARD
	
getXheaders :: RemoteConfig -> [(String, String)]
getXheaders = filter isxheader . M.assocs
  where
	isxheader (h, _) = "x-amz-" `isPrefixOf` h

{- Hostname to use for archive.org S3. -}
iaHost :: HostName
iaHost = "s3.us.archive.org"

isIA :: RemoteConfig -> Bool
isIA c = maybe False isIAHost (M.lookup "host" c)

isIAHost :: HostName -> Bool
isIAHost h = ".archive.org" `isSuffixOf` map toLower h

iaItemUrl :: Bucket -> URLString
iaItemUrl bucket = "http://archive.org/details/" ++ bucket

iaKeyUrl :: Remote -> Key -> URLString
iaKeyUrl r k = "http://archive.org/download/" ++ bucket ++ "/" ++ bucketFile r k
  where
	bucket = fromMaybe "" $ getBucket $ config r
