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
import Remote.Helper.Http
import qualified Remote.Helper.AWS as AWS
import Creds
import Annex.UUID
import Logs.Web

type BucketName = String

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
		(prepareS3 this $ store this)
		(prepareRetrieve this)
		(prepareS3 this $ remove this)
		(prepareS3 this $ checkKey this)
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
				getBucketName c
		let archiveconfig = 
			-- IA acdepts x-amz-* as an alias for x-archive-*
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

-- Sets up a http connection manager for S3 encdpoint, which allows
-- http connections to be reused across calls to the helper.
prepareS3 :: Remote -> (S3Handle -> helper) -> Preparer helper
prepareS3 r = resourcePrepare $ const $ withS3Handle (config r) (uuid r)

store :: Remote -> S3Handle -> Storer
store r h = fileStorer $ \k f p -> do
	rbody <- liftIO $ httpBodyStorer f p
	void $ sendS3Handle h $
		S3.putObject (hBucket h) (hBucketObject h k) rbody
	
	-- Store public URL to item in Internet Archive.
	when (hIsIA h && not (isChunkKey k)) $
		setUrlPresent k (iaKeyUrl r k)
	
	return True

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
remove :: Remote -> S3Handle -> Remover
remove r h k
	| hIsIA h = do
		warning "Cannot remove content from the Internet Archive"
		return False
	| otherwise = do
		res <- tryNonAsync $ sendS3Handle h $
			S3.DeleteObject (hBucketObject h k) (hBucket h)
		return $ either (const False) (const True) res

checkKey :: Remote -> S3Handle -> CheckPresent
checkKey r h k = do
	showAction $ "checking " ++ name r
	catchMissingException $ do
		void $ sendS3Handle h $
			S3.headObject (hBucket h) (hBucketObject h k)
		return True
	
{- Catch exception headObject returns when an object is not present
 - in the bucket, and returns False. All other exceptions indicate a
 - check error and are let through. -}
catchMissingException :: Annex Bool -> Annex Bool
catchMissingException a = catchJust missing a (const $ return False)
  where
	-- This is not very good; see
	-- https://github.com/aristidb/aws/issues/121
	missing :: AWS.HeaderException -> Maybe ()
	missing e
		| AWS.headerErrorMessage e == "ETag missing" = Just ()
		| otherwise = Nothing

s3Warning :: ReqError -> Annex Bool
s3Warning e = do
	warning $ prettyReqError e
	return False

s3Bool :: AWSResult () -> Annex Bool
s3Bool (Right _) = return True
s3Bool (Left e) = s3Warning e

s3Action :: Remote -> a -> ((AWSConnection, BucketName) -> Annex a) -> Annex a
s3Action r noconn action = do
	let bucket = M.lookup "bucket" $ config r
	conn <- s3Connection (config r) (uuid r)
	case (bucket, conn) of
		(Just b, Just c) -> action (c, b)
		_ -> return noconn

bucketKey :: Remote -> BucketName -> Key -> S3Object
bucketKey r bucket k = S3Object bucket (bucketObject (config r) k) "" [] L.empty

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
		v <- tryS3 $ sendS3Handle h (S3.getBucket $ hBucket h)
		case v of
			Right _ -> noop
			Left _ -> do
				showAction $ "creating bucket in " ++ datacenter
				void $ sendS3Handle h $
					S3.PutBucket (hBucket h) Nothing $
						AWS.mkLocationConstraint $
							T.pack datacenter
		writeUUIDFile c u h
	
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
		Right False -> void $ sendS3Handle h mkobject
  where
	file = T.pack $ uuidFile c
	uuidb = L.fromChunks [T.encodeUtf8 $ T.pack $ fromUUID u]

	-- TODO: add headers from getXheaders
	-- (See https://github.com/aristidb/aws/issues/119)
	mkobject = (S3.putObject (hBucket h) file $ RequestBodyLBS uuidb)
		{ S3.poStorageClass = Just (hStorageClass h) }

{- Checks if the UUID file exists in the bucket
 - and has the specified UUID already. -}
checkUUIDFile :: RemoteConfig -> UUID -> S3Handle -> Annex (Either SomeException Bool)
checkUUIDFile c u h = tryNonAsync $ check <$> get
  where
	get = liftIO 
		. runResourceT 
		. either (pure . Left) (Right <$$> AWS.loadToMemory)
		=<< tryS3 (sendS3Handle h (S3.getObject (hBucket h) file))
	check (Right (S3.GetObjectMemoryResponse _meta rsp)) =
		responseStatus rsp == ok200 && responseBody rsp == uuidb
	check (Left _S3Error) = False

	file = T.pack $ uuidFile c
	uuidb = L.fromChunks [T.encodeUtf8 $ T.pack $ fromUUID u]

uuidFile :: RemoteConfig -> FilePath
uuidFile c = getFilePrefix c ++ "annex-uuid"

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

data S3Handle = S3Handle 
	{ hmanager :: Manager
	, hawscfg :: AWS.Configuration
	, hs3cfg :: S3.S3Configuration AWS.NormalQuery

	-- Cached values.
	, hBucket :: S3.Bucket
	, hStorageClass :: S3.StorageClass
	, hBucketObject :: Key -> S3.Bucket
	, hIsIA :: Bool
	}

{- Sends a request to S3 and gets back the response.
 - 
 - Note that pureAws's use of ResourceT is bypassed here;
 - the response should be fully processed while the S3Handle
 - is still open, eg within a call to withS3Handle.
 -}
sendS3Handle
	:: (AWS.Transaction req res, AWS.ServiceConfiguration req ~ S3.S3Configuration)
	=> S3Handle 
	-> req
	-> Annex res
sendS3Handle h = liftIO . runResourceT . call
  where
	call = AWS.pureAws (hawscfg h) (hs3cfg h) (hmanager h)

withS3Handle :: RemoteConfig -> UUID -> (S3Handle -> Annex a) -> Annex a
withS3Handle c u a = do
	creds <- getRemoteCredPairFor "S3" c (AWS.creds u)
	awscreds <- liftIO $ AWS.genCredentials $ fromMaybe nocreds creds
	bucket <- maybe nobucket (return . T.pack) (getBucketName c)
	let awscfg = AWS.Configuration AWS.Timestamp awscreds (AWS.defaultLog AWS.Error)
	bracketIO (newManager httpcfg) closeManager $ \mgr -> 
		a $ S3Handle mgr awscfg s3cfg bucket sc bo (isIA c)
  where
	s3cfg = s3Configuration c
	httpcfg = defaultManagerSettings
		{ managerResponseTimeout = Nothing }
	sc = getStorageClass c
	bo = T.pack . bucketObject c -- memoized
	nocreds = error "Cannot use S3 without credentials configured"
	nobucket = error "S3 bucket not configured"

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

tryS3 :: Annex a -> Annex (Either S3.S3Error a)
tryS3 a = (Right <$> a) `catch` (pure . Left)

s3Error :: S3.S3Error -> a
s3Error (S3.S3Error { S3.s3ErrorMessage = m }) = 
	error $ "S3 error: " ++ T.unpack m

getBucketName :: RemoteConfig -> Maybe BucketName
getBucketName = M.lookup "bucket"

getStorageClass :: RemoteConfig -> S3.StorageClass
getStorageClass c = case M.lookup "storageclass" c of
	Just "REDUCED_REDUNDANCY" -> S3.ReducedRedundancy
	_ -> S3.Standard
	
getXheaders :: RemoteConfig -> [(String, String)]
getXheaders = filter isxheader . M.assocs
  where
	isxheader (h, _) = "x-amz-" `isPrefixOf` h

getFilePrefix :: RemoteConfig -> String
getFilePrefix = M.findWithDefault "" "fileprefix"

bucketObject :: RemoteConfig -> Key -> FilePath
bucketObject c = munge . key2file
  where
	munge s = case M.lookup "mungekeys" c of
		Just "ia" -> iaMunge $ getFilePrefix c ++ s
		_ -> getFilePrefix c ++ s

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

{- Hostname to use for archive.org S3. -}
iaHost :: HostName
iaHost = "s3.us.archive.org"

isIA :: RemoteConfig -> Bool
isIA c = maybe False isIAHost (M.lookup "host" c)

isIAHost :: HostName -> Bool
isIAHost h = ".archive.org" `isSuffixOf` map toLower h

iaItemUrl :: BucketName -> URLString
iaItemUrl bucket = "http://archive.org/details/" ++ bucket

iaKeyUrl :: Remote -> Key -> URLString
iaKeyUrl r k = "http://archive.org/download/" ++ bucket ++ "/" ++ bucketObject (config r) k
  where
	bucket = fromMaybe "" $ getBucketName $ config r
