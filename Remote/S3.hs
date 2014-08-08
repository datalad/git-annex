{- S3 remotes
 -
 - Copyright 2011-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies #-}

module Remote.S3 (remote, iaHost, isIA, isIAHost, iaItemUrl) where

import qualified Aws as AWS
import qualified Aws.Core as AWS
import qualified Aws.S3 as S3
import Network.AWS.AWSConnection
import Network.AWS.S3Object hiding (getStorageClass)
import Network.AWS.AWSResult
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Char
import Network.Socket (HostName)
import Network.HTTP.Conduit (Manager, newManager, closeManager)
import Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout, responseStatus, responseBody, RequestBody(..))
import Network.HTTP.Types
import Control.Monad.Trans.Resource
import Control.Monad.Catch

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
		(simplyPrepare $ remove this c)
		(simplyPrepare $ checkKey this)
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
		, ("host", AWS.s3DefaultHost)
		, ("port", "80")
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
		withS3Handle archiveconfig u $
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
	error "TODO"
	{-
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
	-}

prepareRetrieve :: Remote -> Preparer Retriever
prepareRetrieve r = resourcePrepare (const $ s3Action r False) $ \(conn, bucket) ->
	error "TODO"
	{- 
	byteRetriever $ \k sink ->
		liftIO (getObject conn $ bucketKey r bucket k)
			>>= either s3Error (sink . obj_data)
	-}

retrieveCheap :: Key -> FilePath -> Annex Bool
retrieveCheap _ _ = return False

{- Internet Archive doesn't easily allow removing content.
 - While it may remove the file, there are generally other files
 - derived from it that it does not remove. -}
remove :: Remote -> RemoteConfig -> Remover
remove r c k
	| isIA c = do
		warning "Cannot remove content from the Internet Archive"
		return False
	| otherwise = remove' r k

remove' :: Remote -> Key -> Annex Bool
remove' r k = s3Action r False $ \(conn, bucket) ->
	s3Bool =<< liftIO (deleteObject conn $ bucketKey r bucket k)

checkKey :: Remote -> CheckPresent
checkKey r k = s3Action r noconn $ \(conn, bucket) -> do
	showAction $ "checking " ++ name r
	{-
	res <- liftIO $ getObjectInfo conn $ bucketKey r bucket k
	case res of
		Right _ -> return True
		Left (AWSError _ _) -> return False
		Left e -> s3Error e
	-}
	error "TODO"
  where
	noconn = error "S3 not configured"
			
s3Warning :: ReqError -> Annex Bool
s3Warning e = do
	warning $ prettyReqError e
	return False

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
 - Some ACLs can allow read/write to buckets, but not querying them,
 - so first check if the UUID file already exists and we can skip doing
 - anything.
 -}
genBucket :: RemoteConfig -> UUID -> Annex ()
genBucket c u = do
	showAction "checking bucket"
	withS3Handle c u $ \h ->
		go h =<< checkUUIDFile c u h
  where
	go _ (Right True) = noop
	go h _ = do
		v <- sendS3Handle h (S3.getBucket bucket)
		case v of
			Right _ -> noop
			Left _ -> do
				showAction $ "creating bucket in " ++ datacenter
				void $ mustSucceed $ sendS3Handle h $
					S3.PutBucket bucket Nothing $
						AWS.mkLocationConstraint $
							T.pack datacenter
		writeUUIDFile c u h
	
	bucket = T.pack $ fromJust $ getBucket c
	datacenter = fromJust $ M.lookup "datacenter" c

{- Writes the UUID to an annex-uuid file within the bucket.
 -
 - If the file already exists in the bucket, it must match.
 -
 - Note that IA buckets can only created by having a file
 - stored in them. So this also takes care of that.
 -}
writeUUIDFile :: RemoteConfig -> UUID -> S3Handle -> Annex ()
writeUUIDFile c u h = do
	v <- checkUUIDFile c u h
	case v of
		Left e -> throwM e
		Right True -> noop
		Right False -> void $ mustSucceed $ sendS3Handle h mkobject
  where
	file = T.pack $ uuidFile c
	uuidb = L.fromChunks [T.encodeUtf8 $ T.pack $ fromUUID u]
	bucket = T.pack $ fromJust $ getBucket c

	-- TODO: add headers from getXheaders
	-- (See https://github.com/aristidb/aws/issues/119)
	mkobject = (S3.putObject bucket file $ RequestBodyLBS uuidb)
		{ S3.poStorageClass = Just (getStorageClass c) }

{- Checks if the UUID file exists in the bucket
 - and has the specified UUID already. -}
checkUUIDFile :: RemoteConfig -> UUID -> S3Handle -> Annex (Either SomeException Bool)
checkUUIDFile c u h = tryNonAsync $ check <$> get
  where
	get = liftIO 
		. runResourceT 
		. either (pure . Left) (Right <$$> AWS.loadToMemory)
		=<< sendS3Handle h (S3.getObject bucket file)
	check (Right (S3.GetObjectMemoryResponse _meta rsp)) =
		responseStatus rsp == ok200 && responseBody rsp == uuidb
	check (Left _S3Error) = False

	bucket = T.pack $ fromJust $ getBucket c
	file = T.pack $ uuidFile c
	uuidb = L.fromChunks [T.encodeUtf8 $ T.pack $ fromUUID u]

uuidFile :: RemoteConfig -> FilePath
uuidFile c = filePrefix c ++ "annex-uuid"

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

data S3Handle = S3Handle Manager AWS.Configuration (S3.S3Configuration AWS.NormalQuery)

{- Sends a request to S3 and gets back the response.
 - 
 - Note that pureAws's use of ResourceT is bypassed here;
 - the response should be processed while the S3Handle is still open,
 - eg within a call to withS3Handle.
 -}
sendS3Handle
	:: (AWS.Transaction req res, AWS.ServiceConfiguration req ~ S3.S3Configuration)
	=> S3Handle 
	-> req
	-> Annex (Either S3.S3Error res)
sendS3Handle (S3Handle manager awscfg s3cfg) req = safely $ liftIO $
	runResourceT $ AWS.pureAws awscfg s3cfg manager req
  where
	safely a = (Right <$> a) `catch` (pure . Left)

mustSucceed :: Annex (Either S3.S3Error res) -> Annex res
mustSucceed a = a >>= either s3Error return

s3Error :: S3.S3Error -> a
s3Error (S3.S3Error { S3.s3ErrorMessage = m }) = error $ "S3 error: " ++ T.unpack m

withS3Handle :: RemoteConfig -> UUID -> (S3Handle -> Annex a) -> Annex a
withS3Handle c u a = do
	creds <- getRemoteCredPairFor "S3" c (AWS.creds u)
	awscreds <- liftIO $ AWS.genCredentials $ fromMaybe nocreds creds
	let awscfg = AWS.Configuration AWS.Timestamp awscreds (AWS.defaultLog AWS.Error)
	bracketIO (newManager httpcfg) closeManager $ \mgr -> 
		a $ S3Handle mgr awscfg s3cfg
  where
	s3cfg = s3Configuration c
	httpcfg = defaultManagerSettings
		{ managerResponseTimeout = Nothing }
	nocreds = error "Cannot use S3 without credentials configured"

s3Configuration :: RemoteConfig -> S3.S3Configuration AWS.NormalQuery
s3Configuration c = (S3.s3 proto endpoint False) { S3.s3Port = port }
  where
	proto
		| port == 443 = AWS.HTTPS
		| otherwise = AWS.HTTP
	host = fromJust $ M.lookup "host" c
	datacenter = fromJust $ M.lookup "datacenter" c
	-- When the default S3 host is configured, connect directly to
	-- the S3 endpoint for the configured datacenter.
	-- When another host is configured, it's used as-is.
	endpoint
		| host == AWS.s3DefaultHost = AWS.s3HostName $ T.pack datacenter
		| otherwise = T.encodeUtf8 $ T.pack host
	port = let s = fromJust $ M.lookup "port" c in
		case reads s of
		[(p, _)] -> p
		_ -> error $ "bad S3 port value: " ++ s

getBucket :: RemoteConfig -> Maybe Bucket
getBucket = M.lookup "bucket"

getStorageClass :: RemoteConfig -> S3.StorageClass
getStorageClass c = case fromJust $ M.lookup "storageclass" c of
	"REDUCED_REDUNDANCY" -> S3.ReducedRedundancy
	_ -> S3.Standard
	
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
