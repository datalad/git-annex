{- S3 remotes
 -
 - Copyright 2011-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Remote.S3 (remote, iaHost, configIA, iaItemUrl) where

import qualified Aws as AWS
import qualified Aws.Core as AWS
import qualified Aws.S3 as S3
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.Map as M
import Data.Char
import Network.Socket (HostName)
import Network.HTTP.Conduit (Manager, newManager, closeManager)
import Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout, responseStatus, responseBody, RequestBody(..))
import Network.HTTP.Types
import Control.Monad.Trans.Resource
import Control.Monad.Catch
import Data.Conduit
import Data.IORef

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
import Utility.Metered
import Utility.DataUnits

type BucketName = String

remote :: RemoteType
remote = RemoteType {
	typename = "S3",
	enumerate = findSpecialRemotes "s3",
	generate = gen,
	setup = s3Setup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = do
	cst <- remoteCost gc expensiveRemoteCost
	info <- extractS3Info c
	return $ new cst info
  where
	new cst info = Just $ specialRemote c
		(prepareS3 this info $ store this)
		(prepareS3 this info retrieve)
		(prepareS3 this info remove)
		(prepareS3 this info $ checkKey this)
		this
	  where
		this = Remote
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = storeKeyDummy
			, retrieveKeyFile = retreiveKeyFileDummy
			, retrieveKeyFileCheap = retrieveCheap
			, removeKey = removeKeyDummy
			, checkPresent = checkPresentDummy
			, checkPresentCheap = False
			, whereisKey = Nothing
			, remoteFsck = Nothing
			, repairRepo = Nothing
			, config = c
			, repo = r
			, gitconfig = gc
			, localpath = Nothing
			, readonly = False
			, availability = GloballyAvailable
			, remotetype = remote
			, mkUnavailable = gen r u (M.insert "host" "!dne!" c) gc
			, getInfo = includeCredsInfo c (AWS.creds u) $ catMaybes
				[ Just ("bucket", fromMaybe "unknown" (getBucketName c))
				, if configIA c
					then Just ("internet archive item", iaItemUrl $ fromMaybe "unknown" $ getBucketName c)
					else Nothing
				, Just ("partsize", maybe "unlimited" (roughSize storageUnits False) (getPartSize c))
				]
			, claimUrl = Nothing
			, checkUrl = Nothing
			}

s3Setup :: Maybe UUID -> Maybe CredPair -> RemoteConfig -> Annex (RemoteConfig, UUID)
s3Setup mu mcreds c = do
	u <- maybe (liftIO genUUID) return mu
	s3Setup' u mcreds c
s3Setup' :: UUID -> Maybe CredPair -> RemoteConfig -> Annex (RemoteConfig, UUID)
s3Setup' u mcreds c = if configIA c then archiveorg else defaulthost
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
		(c', encsetup) <- encryptionSetup c
		c'' <- setRemoteCredPair encsetup c' (AWS.creds u) mcreds
		let fullconfig = c'' `M.union` defaults
		genBucket fullconfig u
		use fullconfig

	archiveorg = do
		showNote "Internet Archive mode"
		c' <- setRemoteCredPair noEncryptionUsed c (AWS.creds u) mcreds
		-- Ensure user enters a valid bucket name, since
		-- this determines the name of the archive.org item.
		let validbucket = replace " " "-" $ map toLower $
			fromMaybe (error "specify bucket=") $
				getBucketName c'
		let archiveconfig = 
			-- IA acdepts x-amz-* as an alias for x-archive-*
			M.mapKeys (replace "x-archive-" "x-amz-") $
			-- encryption does not make sense here
			M.insert "encryption" "none" $
			M.insert "bucket" validbucket $
			M.union c' $
			-- special constraints on key names
			M.insert "mungekeys" "ia" defaults
		info <- extractS3Info archiveconfig
		withS3Handle archiveconfig u info $
			writeUUIDFile archiveconfig u
		use archiveconfig

-- Sets up a http connection manager for S3 encdpoint, which allows
-- http connections to be reused across calls to the helper.
prepareS3 :: Remote -> S3Info -> (S3Handle -> helper) -> Preparer helper
prepareS3 r info = resourcePrepare $ const $
	withS3Handle (config r) (uuid r) info

store :: Remote -> S3Handle -> Storer
store r h = fileStorer $ \k f p -> do
	case partSize (hinfo h) of
		Just partsz | partsz > 0 -> do
			fsz <- liftIO $ getFileSize f
			if fsz > partsz
				then multipartupload fsz partsz k f p
				else singlepartupload k f p
		_ -> singlepartupload k f p	
	-- Store public URL to item in Internet Archive.
	when (isIA (hinfo h) && not (isChunkKey k)) $
		setUrlPresent webUUID k (iaKeyUrl r k)
	return True
  where
	singlepartupload k f p = do
		rbody <- liftIO $ httpBodyStorer f p
		void $ sendS3Handle h $ putObject h (bucketObject (hinfo h) k) rbody
	multipartupload fsz partsz k f p = do
#if MIN_VERSION_aws(0,10,6)
		let info = hinfo h
		let object = bucketObject info k

		let startreq = (S3.postInitiateMultipartUpload (bucket info) object)
				{ S3.imuStorageClass = Just (storageClass info)
				, S3.imuMetadata = metaHeaders info
				, S3.imuAutoMakeBucket = isIA info
				, S3.imuExpires = Nothing -- TODO set some reasonable expiry
				}
		uploadid <- S3.imurUploadId <$> sendS3Handle h startreq

		-- The actual part size will be a even multiple of the
		-- 32k chunk size that hGetUntilMetered uses.
		let partsz' = (partsz `div` toInteger defaultChunkSize) * toInteger defaultChunkSize

		-- Send parts of the file, taking care to stream each part
		-- w/o buffering in memory, since the parts can be large.
		etags <- bracketIO (openBinaryFile f ReadMode) hClose $ \fh -> do
			let sendparts meter etags partnum = do
				pos <- liftIO $ hTell fh
				if pos >= fsz
					then return (reverse etags)
					else do
						-- Calculate size of part that will
						-- be read.
						let sz = if fsz - pos < partsz'
							then fsz - pos
							else partsz'
						let p' = offsetMeterUpdate p (toBytesProcessed pos)
						let numchunks = ceiling (fromIntegral sz / fromIntegral defaultChunkSize :: Double)
						let popper = handlePopper numchunks defaultChunkSize p' fh
						let req = S3.uploadPart (bucket info) object partnum uploadid $
							 RequestBodyStream (fromIntegral sz) popper
						S3.UploadPartResponse _ etag <- sendS3Handle h req
						sendparts (offsetMeterUpdate meter (toBytesProcessed sz)) (etag:etags) (partnum + 1)
			sendparts p [] 1

		void $ sendS3Handle h $ S3.postCompleteMultipartUpload
			(bucket info) object uploadid (zip [1..] etags)
#else
		warning $ "Cannot do multipart upload (partsize " ++ show partsz ++ ") of large file (" ++ show fsz ++ "); built with too old a version of the aws library."
		singlepartupload k f p
#endif

{- Implemented as a fileRetriever, that uses conduit to stream the chunks
 - out to the file. Would be better to implement a byteRetriever, but
 - that is difficult. -}
retrieve :: S3Handle -> Retriever
retrieve h = fileRetriever $ \f k p -> liftIO $ runResourceT $ do
	(fr, fh) <- allocate (openFile f WriteMode) hClose
	let req = S3.getObject (bucket info) (bucketObject info k)
	S3.GetObjectResponse { S3.gorResponse = rsp } <- sendS3Handle' h req
	responseBody rsp $$+- sinkprogressfile fh p zeroBytesProcessed
	release fr
  where
	info = hinfo h
	sinkprogressfile fh meterupdate sofar = do
		mbs <- await
		case mbs of
			Nothing -> return ()
			Just bs -> do
				let sofar' = addBytesProcessed sofar (S.length bs)
				liftIO $ do
					void $ meterupdate sofar'
					S.hPut fh bs
				sinkprogressfile fh meterupdate sofar'

retrieveCheap :: Key -> AssociatedFile -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

{- Internet Archive doesn't easily allow removing content.
 - While it may remove the file, there are generally other files
 - derived from it that it does not remove. -}
remove :: S3Handle -> Remover
remove h k
	| isIA info = do
		warning "Cannot remove content from the Internet Archive"
		return False
	| otherwise = do
		res <- tryNonAsync $ sendS3Handle h $
			S3.DeleteObject (bucketObject info k) (bucket info)
		return $ either (const False) (const True) res
  where
	info = hinfo h

checkKey :: Remote -> S3Handle -> CheckPresent
checkKey r h k = do
	showAction $ "checking " ++ name r
#if MIN_VERSION_aws(0,10,0)
	rsp <- go
	return (isJust $ S3.horMetadata rsp)
#else
	catchMissingException $ do
		void go
		return True
#endif
  where
	go = sendS3Handle h $
		S3.headObject (bucket (hinfo h)) (bucketObject (hinfo h) k)

#if ! MIN_VERSION_aws(0,10,0)
	{- Catch exception headObject returns when an object is not present
	 - in the bucket, and returns False. All other exceptions indicate a
	 - check error and are let through. -}
	catchMissingException :: Annex Bool -> Annex Bool
	catchMissingException a = catchJust missing a (const $ return False)
	  where
		missing :: AWS.HeaderException -> Maybe ()
		missing e
			| AWS.headerErrorMessage e == "ETag missing" = Just ()
			| otherwise = Nothing
#endif

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
	info <- extractS3Info c
	withS3Handle c u info $ \h ->
		go h =<< checkUUIDFile c u h
  where
	go _ (Right True) = noop
	go h _ = do
		v <- tryNonAsync $ sendS3Handle h (S3.getBucket $ bucket $ hinfo h)
		case v of
			Right _ -> noop
			Left _ -> do
				showAction $ "creating bucket in " ++ datacenter
				void $ sendS3Handle h $
					S3.PutBucket (bucket $ hinfo h) Nothing $
						mkLocationConstraint $
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
		Right True -> noop
		_ -> void $ sendS3Handle h mkobject
  where
	file = T.pack $ uuidFile c
	uuidb = L.fromChunks [T.encodeUtf8 $ T.pack $ fromUUID u]

	mkobject = putObject h file (RequestBodyLBS uuidb)

{- Checks if the UUID file exists in the bucket
 - and has the specified UUID already. -}
checkUUIDFile :: RemoteConfig -> UUID -> S3Handle -> Annex (Either SomeException Bool)
checkUUIDFile c u h = tryNonAsync $ check <$> get
  where
	get = liftIO 
		. runResourceT 
		. either (pure . Left) (Right <$$> AWS.loadToMemory)
		=<< tryS3 (sendS3Handle h (S3.getObject (bucket (hinfo h)) file))
	check (Right (S3.GetObjectMemoryResponse _meta rsp)) =
		responseStatus rsp == ok200 && responseBody rsp == uuidb
	check (Left _S3Error) = False

	file = T.pack $ uuidFile c
	uuidb = L.fromChunks [T.encodeUtf8 $ T.pack $ fromUUID u]

uuidFile :: RemoteConfig -> FilePath
uuidFile c = getFilePrefix c ++ "annex-uuid"

putObject :: S3Handle -> T.Text -> RequestBody -> S3.PutObject
putObject h file rbody = (S3.putObject (bucket info) file rbody)
	{ S3.poStorageClass = Just (storageClass info)
	, S3.poMetadata = metaHeaders info
	, S3.poAutoMakeBucket = isIA info
	}
  where
	info = hinfo h

data S3Handle = S3Handle 
	{ hmanager :: Manager
	, hawscfg :: AWS.Configuration
	, hs3cfg :: S3.S3Configuration AWS.NormalQuery
	, hinfo :: S3Info
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
sendS3Handle h r = liftIO $ runResourceT $ sendS3Handle' h r

sendS3Handle'
	:: (AWS.Transaction r a, AWS.ServiceConfiguration r ~ S3.S3Configuration)
	=> S3Handle
	-> r
	-> ResourceT IO a
sendS3Handle' h = AWS.pureAws (hawscfg h) (hs3cfg h) (hmanager h)

withS3Handle :: RemoteConfig -> UUID -> S3Info -> (S3Handle -> Annex a) -> Annex a
withS3Handle c u info a = do
	creds <- getRemoteCredPairFor "S3" c (AWS.creds u)
	awscreds <- liftIO $ genCredentials $ fromMaybe nocreds creds
	let awscfg = AWS.Configuration AWS.Timestamp awscreds (AWS.defaultLog AWS.Error)
	bracketIO (newManager httpcfg) closeManager $ \mgr -> 
		a $ S3Handle mgr awscfg s3cfg info
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

tryS3 :: Annex a -> Annex (Either S3.S3Error a)
tryS3 a = (Right <$> a) `catch` (pure . Left)

data S3Info = S3Info
	{ bucket :: S3.Bucket
	, storageClass :: S3.StorageClass
	, bucketObject :: Key -> T.Text
	, metaHeaders :: [(T.Text, T.Text)]
	, partSize :: Maybe Integer
	, isIA :: Bool
	}

extractS3Info :: RemoteConfig -> Annex S3Info
extractS3Info c = do
	b <- maybe
		(error "S3 bucket not configured")
		(return . T.pack)
		(getBucketName c)
	return $ S3Info
		{ bucket = b
		, storageClass = getStorageClass c
		, bucketObject = T.pack . getBucketObject c
		, metaHeaders = getMetaHeaders c
		, partSize = getPartSize c
		, isIA = configIA c
		}

getBucketName :: RemoteConfig -> Maybe BucketName
getBucketName = M.lookup "bucket"

getStorageClass :: RemoteConfig -> S3.StorageClass
getStorageClass c = case M.lookup "storageclass" c of
	Just "REDUCED_REDUNDANCY" -> S3.ReducedRedundancy
	_ -> S3.Standard

getPartSize :: RemoteConfig -> Maybe Integer
getPartSize c = readSize dataUnits =<< M.lookup "partsize" c

getMetaHeaders :: RemoteConfig -> [(T.Text, T.Text)]
getMetaHeaders = map munge . filter ismetaheader . M.assocs
  where
	ismetaheader (h, _) = metaprefix `isPrefixOf` h
	metaprefix = "x-amz-meta-"
	metaprefixlen = length metaprefix
	munge (k, v) = (T.pack $ drop metaprefixlen k, T.pack v)

getFilePrefix :: RemoteConfig -> String
getFilePrefix = M.findWithDefault "" "fileprefix"

getBucketObject :: RemoteConfig -> Key -> FilePath
getBucketObject c = munge . key2file
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

configIA :: RemoteConfig -> Bool
configIA = maybe False isIAHost . M.lookup "host"

{- Hostname to use for archive.org S3. -}
iaHost :: HostName
iaHost = "s3.us.archive.org"

isIAHost :: HostName -> Bool
isIAHost h = ".archive.org" `isSuffixOf` map toLower h

iaItemUrl :: BucketName -> URLString
iaItemUrl b = "http://archive.org/details/" ++ b

iaKeyUrl :: Remote -> Key -> URLString
iaKeyUrl r k = "http://archive.org/download/" ++ b ++ "/" ++ getBucketObject (config r) k
  where
	b = fromMaybe "" $ getBucketName $ config r

genCredentials :: CredPair -> IO AWS.Credentials
genCredentials (keyid, secret) = AWS.Credentials
	<$> pure (T.encodeUtf8 (T.pack keyid))
	<*> pure (T.encodeUtf8 (T.pack secret))
	<*> newIORef []
	<*> pure Nothing

mkLocationConstraint :: AWS.Region -> S3.LocationConstraint
mkLocationConstraint "US" = S3.locationUsClassic
mkLocationConstraint r = r
