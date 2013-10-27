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
import qualified Data.ByteString.Lazy.Char8 as L
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
import Remote.Helper.Encryptable
import qualified Remote.Helper.AWS as AWS
import Crypto
import Creds
import Utility.Metered
import Annex.Content
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
			globallyAvailable = True,
			remotetype = remote
		}

s3Setup :: Maybe UUID -> RemoteConfig -> Annex (RemoteConfig, UUID)
s3Setup mu c = do
	u <- maybe (liftIO genUUID) return mu
	s3Setup' u c
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
		c' <- setRemoteCredPair fullconfig (AWS.creds u)
		return (c', u)

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

store :: Remote -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store r k _f p = s3Action r False $ \(conn, bucket) -> 
	sendAnnex k (void $ remove' r k) $ \src -> do
		ok <- s3Bool =<< storeHelper (conn, bucket) r k p src

		-- Store public URL to item in Internet Archive.
		when (ok && isIA (config r)) $
			setUrlPresent k (iaKeyUrl r k)

		return ok

storeEncrypted :: Remote -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted r (cipher, enck) k p = s3Action r False $ \(conn, bucket) -> 
	-- To get file size of the encrypted content, have to use a temp file.
	-- (An alternative would be chunking to to a constant size.)
	withTmp enck $ \tmp -> sendAnnex k (void $ remove' r enck) $ \src -> do
		liftIO $ encrypt (getGpgEncParams r) cipher (feedFile src) $
			readBytes $ L.writeFile tmp
		s3Bool =<< storeHelper (conn, bucket) r enck p tmp

storeHelper :: (AWSConnection, Bucket) -> Remote -> Key -> MeterUpdate -> FilePath -> Annex (AWSResult ())
storeHelper (conn, bucket) r k p file = do
	size <- maybe getsize (return . fromIntegral) $ keySize k
	meteredBytes (Just p) size $ \meterupdate ->
		liftIO $ withMeteredFile file meterupdate $ \content -> do
			-- size is provided to S3 so the whole content
			-- does not need to be buffered to calculate it
			let object = S3Object
				bucket (bucketFile r k) ""
				(("Content-Length", show size) : getXheaders (config r))
				content
			sendObject conn $
				setStorageClass (getStorageClass $ config r) object
  where
	getsize = liftIO $ fromIntegral . fileSize <$> getFileStatus file

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

genBucket :: RemoteConfig -> UUID -> Annex ()
genBucket c u = do
	conn <- s3ConnectionRequired c u
	showAction "checking bucket"
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
	go conn =<< liftIO (tryNonAsync $ getObject conn $ mkobject L.empty)
  where
	go _conn (Right (Right o)) = unless (obj_data o == uuidb) $
		error $ "This bucket is already in use by a different S3 special remote, with UUID: " ++ L.unpack (obj_data o)
	go conn _ = do
		let object = setStorageClass (getStorageClass c) (mkobject uuidb)
		either s3Error return =<< liftIO (sendObject conn object)

	file = filePrefix c ++ "annex-uuid"
	uuidb = L.pack $ fromUUID u
	bucket = fromJust $ getBucket c

	mkobject = S3Object bucket file "" (getXheaders c)

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
