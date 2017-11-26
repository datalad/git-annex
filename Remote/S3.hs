{- S3 remotes
 -
 - Copyright 2011-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
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
import Network.HTTP.Conduit (Manager, newManager)
import Network.HTTP.Client (responseStatus, responseBody, RequestBody(..))
import Network.HTTP.Types
import Control.Monad.Trans.Resource
import Control.Monad.Catch
import Data.Conduit
import Data.IORef
import System.Log.Logger

import Annex.Common
import Types.Remote
import Types.Export
import Annex.Export
import qualified Git
import Config
import Config.Cost
import Remote.Helper.Special
import Remote.Helper.Http
import Remote.Helper.Messages
import Remote.Helper.Export
import qualified Remote.Helper.AWS as AWS
import Creds
import Annex.UUID
import Logs.Web
import Utility.Metered
import Utility.DataUnits
import Utility.FileSystemEncoding
import Annex.Content
import Annex.Url (withUrlOptions)
import Utility.Url (checkBoth, managerSettings, closeManager)

type BucketName = String

remote :: RemoteType
remote = RemoteType
	{ typename = "S3"
	, enumerate = const (findSpecialRemotes "s3")
	, generate = gen
	, setup = s3Setup
	, exportSupported = exportIsSupported
	}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = do
	cst <- remoteCost gc expensiveRemoteCost
	info <- extractS3Info c
	return $ new cst info
  where
	new cst info = Just $ specialRemote c
		(prepareS3Handle this $ store this info)
		(prepareS3HandleMaybe this $ retrieve this info)
		(prepareS3Handle this $ remove info)
		(prepareS3HandleMaybe this $ checkKey this info)
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
			, lockContent = Nothing
			, checkPresent = checkPresentDummy
			, checkPresentCheap = False
			, exportActions = withS3Handle c gc u $ \h -> 
				return $ ExportActions
					{ storeExport = storeExportS3 info h
					, retrieveExport = retrieveExportS3 info h
					, removeExport = removeExportS3 info h
					, checkPresentExport = checkPresentExportS3 info h
					-- S3 does not have directories.
					, removeExportDirectory = Nothing
					, renameExport = renameExportS3 info h
					}
			, whereisKey = Just (getWebUrls info c)
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
			, getInfo = includeCredsInfo c (AWS.creds u) (s3Info c info)
			, claimUrl = Nothing
			, checkUrl = Nothing
			}

s3Setup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
s3Setup ss mu mcreds c gc = do
	u <- maybe (liftIO genUUID) return mu
	s3Setup' ss u mcreds c gc

s3Setup' :: SetupStage -> UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
s3Setup' ss u mcreds c gc
	| configIA c = archiveorg
	| otherwise = defaulthost
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
		(c', encsetup) <- encryptionSetup c gc
		c'' <- setRemoteCredPair encsetup c' gc (AWS.creds u) mcreds
		let fullconfig = c'' `M.union` defaults
		case ss of
			Init -> genBucket fullconfig gc u
			_ -> return ()
		use fullconfig

	archiveorg = do
		showNote "Internet Archive mode"
		c' <- setRemoteCredPair noEncryptionUsed c gc (AWS.creds u) mcreds
		-- Ensure user enters a valid bucket name, since
		-- this determines the name of the archive.org item.
		let validbucket = replace " " "-" $
			fromMaybe (giveup "specify bucket=") $
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
		withS3Handle archiveconfig gc u $
			writeUUIDFile archiveconfig u info
		use archiveconfig

-- Sets up a http connection manager for S3 endpoint, which allows
-- http connections to be reused across calls to the helper.
prepareS3Handle :: Remote -> (S3Handle -> helper) -> Preparer helper
prepareS3Handle r = resourcePrepare $ const $
	withS3Handle (config r) (gitconfig r) (uuid r)

-- Allows for read-only actions, which can be run without a S3Handle.
prepareS3HandleMaybe :: Remote -> (Maybe S3Handle -> helper) -> Preparer helper
prepareS3HandleMaybe r = resourcePrepare $ const $
	withS3HandleMaybe (config r) (gitconfig r) (uuid r)

store :: Remote -> S3Info -> S3Handle -> Storer
store _r info h = fileStorer $ \k f p -> do
	storeHelper info h f (T.pack $ bucketObject info k) p
	-- Store public URL to item in Internet Archive.
	when (isIA info && not (isChunkKey k)) $
		setUrlPresent webUUID k (iaPublicKeyUrl info k)
	return True

storeHelper :: S3Info -> S3Handle -> FilePath -> S3.Object -> MeterUpdate -> Annex ()
storeHelper info h f object p = case partSize info of
	Just partsz | partsz > 0 -> do
		fsz <- liftIO $ getFileSize f
		if fsz > partsz
			then multipartupload fsz partsz
			else singlepartupload
	_ -> singlepartupload
  where
	singlepartupload = do
		rbody <- liftIO $ httpBodyStorer f p
		void $ sendS3Handle h $ putObject info object rbody
	multipartupload fsz partsz = do
#if MIN_VERSION_aws(0,10,6)
		let startreq = (S3.postInitiateMultipartUpload (bucket info) object)
				{ S3.imuStorageClass = Just (storageClass info)
				, S3.imuMetadata = metaHeaders info
				, S3.imuAutoMakeBucket = isIA info
				, S3.imuExpires = Nothing -- TODO set some reasonable expiry
				}
		uploadid <- S3.imurUploadId <$> sendS3Handle h startreq

		-- The actual part size will be a even multiple of the
		-- 32k chunk size that lazy ByteStrings use.
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
						S3.UploadPartResponse { S3.uprETag = etag } <- sendS3Handle h req
						sendparts (offsetMeterUpdate meter (toBytesProcessed sz)) (etag:etags) (partnum + 1)
			sendparts p [] 1

		void $ sendS3Handle h $ S3.postCompleteMultipartUpload
			(bucket info) object uploadid (zip [1..] etags)
#else
		warning $ "Cannot do multipart upload (partsize " ++ show partsz ++ ") of large file (" ++ show fsz ++ "); built with too old a version of the aws library."
		singlepartupload
#endif

{- Implemented as a fileRetriever, that uses conduit to stream the chunks
 - out to the file. Would be better to implement a byteRetriever, but
 - that is difficult. -}
retrieve :: Remote -> S3Info -> Maybe S3Handle -> Retriever
retrieve _ info (Just h) = fileRetriever $ \f k p ->
	retrieveHelper info h (T.pack $ bucketObject info k) f p
retrieve r info Nothing = case getpublicurl info of
	Nothing -> \_ _ _ -> do
		warnMissingCredPairFor "S3" (AWS.creds $ uuid r)
		return False
	Just geturl -> fileRetriever $ \f k p ->
		unlessM (downloadUrl k p [geturl k] f) $
			giveup "failed to download content"

retrieveHelper :: S3Info -> S3Handle -> S3.Object -> FilePath -> MeterUpdate -> Annex ()
retrieveHelper info h object f p = liftIO $ runResourceT $ do
	(fr, fh) <- allocate (openFile f WriteMode) hClose
	let req = S3.getObject (bucket info) object
	S3.GetObjectResponse { S3.gorResponse = rsp } <- sendS3Handle' h req
	responseBody rsp $$+- sinkprogressfile fh p zeroBytesProcessed
	release fr
  where
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
remove :: S3Info -> S3Handle -> Remover
remove info h k = do
	res <- tryNonAsync $ sendS3Handle h $
		S3.DeleteObject (T.pack $ bucketObject info k) (bucket info)
	return $ either (const False) (const True) res

checkKey :: Remote -> S3Info -> Maybe S3Handle -> CheckPresent
checkKey r info Nothing k = case getpublicurl info of
	Nothing -> do
		warnMissingCredPairFor "S3" (AWS.creds $ uuid r)
		giveup "No S3 credentials configured"
	Just geturl -> do
		showChecking r
		withUrlOptions $ checkBoth (geturl k) (keySize k)
checkKey r info (Just h) k = do
	showChecking r
	checkKeyHelper info h (T.pack $ bucketObject info k)

checkKeyHelper :: S3Info -> S3Handle -> S3.Object -> Annex Bool
checkKeyHelper info h object = do
#if MIN_VERSION_aws(0,10,0)
	rsp <- go
	return (isJust $ S3.horMetadata rsp)
#else
	catchMissingException $ do
		void go
		return True
#endif
  where
	go = sendS3Handle h $ S3.headObject (bucket info) object

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

storeExportS3 :: S3Info -> S3Handle -> FilePath -> Key -> ExportLocation -> MeterUpdate -> Annex Bool
storeExportS3 info h f _k loc p = 
	catchNonAsync go (\e -> warning (show e) >> return False)
  where
	go = do
		storeHelper info h f (T.pack $ bucketExportLocation info loc) p
		return True

retrieveExportS3 :: S3Info -> S3Handle -> Key -> ExportLocation -> FilePath -> MeterUpdate -> Annex Bool
retrieveExportS3 info h _k loc f p =
	catchNonAsync go (\e -> warning (show e) >> return False)
  where
	go = do
		retrieveHelper info h (T.pack $ bucketExportLocation info loc) f p
		return True

removeExportS3 :: S3Info -> S3Handle -> Key -> ExportLocation -> Annex Bool
removeExportS3 info h _k loc = 
	catchNonAsync go (\e -> warning (show e) >> return False)
  where
	go = do
		res <- tryNonAsync $ sendS3Handle h $
			S3.DeleteObject (T.pack $ bucketExportLocation info loc) (bucket info)
		return $ either (const False) (const True) res

checkPresentExportS3 :: S3Info -> S3Handle -> Key -> ExportLocation -> Annex Bool
checkPresentExportS3 info h _k loc =
	checkKeyHelper info h (T.pack $ bucketExportLocation info loc)

-- S3 has no move primitive; copy and delete.
renameExportS3 :: S3Info -> S3Handle -> Key -> ExportLocation -> ExportLocation -> Annex Bool
renameExportS3 info h _k src dest = catchNonAsync go (\_ -> return False)
  where
	go = do
		let co = S3.copyObject (bucket info) dstobject
			(S3.ObjectId (bucket info) srcobject Nothing)
			S3.CopyMetadata
		-- ACL is not preserved by copy.
		void $ sendS3Handle h $ co { S3.coAcl = acl info }
		void $ sendS3Handle h $ S3.DeleteObject srcobject (bucket info)
		return True
	srcobject = T.pack $ bucketExportLocation info src
	dstobject = T.pack $ bucketExportLocation info dest

{- Generate the bucket if it does not already exist, including creating the
 - UUID file within the bucket.
 -
 - Some ACLs can allow read/write to buckets, but not querying them,
 - so first check if the UUID file already exists and we can skip doing
 - anything.
 -}
genBucket :: RemoteConfig -> RemoteGitConfig -> UUID -> Annex ()
genBucket c gc u = do
	showAction "checking bucket"
	info <- extractS3Info c
	withS3Handle c gc u $ \h ->
		go info h =<< checkUUIDFile c u info h
  where
	go _ _ (Right True) = noop
	go info h _ = do
		v <- tryNonAsync $ sendS3Handle h (S3.getBucket $ bucket info)
		case v of
			Right _ -> noop
			Left _ -> do
				showAction $ "creating bucket in " ++ datacenter
				void $ sendS3Handle h $ S3.PutBucket
					(bucket info)
					(acl info)
					locconstraint
#if MIN_VERSION_aws(0,13,0)
					storageclass
#endif
		writeUUIDFile c u info h
	
	locconstraint = mkLocationConstraint $ T.pack datacenter
	datacenter = fromJust $ M.lookup "datacenter" c
#if MIN_VERSION_aws(0,13,0)
	-- "NEARLINE" as a storage class when creating a bucket is a
	-- nonstandard extension of Google Cloud Storage.
	storageclass = case getStorageClass c of
		sc@(S3.OtherStorageClass "NEARLINE") -> Just sc
		_ -> Nothing
#endif

{- Writes the UUID to an annex-uuid file within the bucket.
 -
 - If the file already exists in the bucket, it must match,
 - or this fails.
 -
 - Note that IA buckets can only created by having a file
 - stored in them. So this also takes care of that.
 -}
writeUUIDFile :: RemoteConfig -> UUID -> S3Info -> S3Handle -> Annex ()
writeUUIDFile c u info h = do
	v <- checkUUIDFile c u info h
	case v of
		Right True -> noop
		Right False -> do
			warning "The bucket already exists, and its annex-uuid file indicates it is used by a different special remote."
			giveup "Cannot reuse this bucket."
		_ -> void $ sendS3Handle h mkobject
  where
	file = T.pack $ uuidFile c
	uuidb = L.fromChunks [T.encodeUtf8 $ T.pack $ fromUUID u]

	mkobject = putObject info file (RequestBodyLBS uuidb)

{- Checks if the UUID file exists in the bucket
 - and has the specified UUID already. -}
checkUUIDFile :: RemoteConfig -> UUID -> S3Info -> S3Handle -> Annex (Either SomeException Bool)
checkUUIDFile c u info h = tryNonAsync $ liftIO $ runResourceT $ do
	resp <- tryS3 $ sendS3Handle' h (S3.getObject (bucket info) file)
	case resp of
		Left _ -> return False
		Right r -> do
			v <- AWS.loadToMemory r
			let !ok = check v
			return ok
  where
	check (S3.GetObjectMemoryResponse _meta rsp) =
		responseStatus rsp == ok200 && responseBody rsp == uuidb

	file = T.pack $ uuidFile c
	uuidb = L.fromChunks [T.encodeUtf8 $ T.pack $ fromUUID u]

uuidFile :: RemoteConfig -> FilePath
uuidFile c = getFilePrefix c ++ "annex-uuid"

tryS3 :: ResourceT IO a -> ResourceT IO (Either S3.S3Error a)
tryS3 a = (Right <$> a) `catch` (pure . Left)

data S3Handle = S3Handle 
	{ hmanager :: Manager
	, hawscfg :: AWS.Configuration
	, hs3cfg :: S3.S3Configuration AWS.NormalQuery
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
sendS3Handle' h r = AWS.pureAws (hawscfg h) (hs3cfg h) (hmanager h) r

withS3Handle :: RemoteConfig -> RemoteGitConfig -> UUID -> (S3Handle -> Annex a) -> Annex a
withS3Handle c gc u a = withS3HandleMaybe c gc u $ \mh -> case mh of
	Just h -> a h
	Nothing -> do
		warnMissingCredPairFor "S3" (AWS.creds u)
		giveup "No S3 credentials configured"

withS3HandleMaybe :: RemoteConfig -> RemoteGitConfig -> UUID -> (Maybe S3Handle -> Annex a) -> Annex a
withS3HandleMaybe c gc u a = do
	mcreds <- getRemoteCredPair c gc (AWS.creds u)
	case mcreds of
		Just creds -> do
			awscreds <- liftIO $ genCredentials creds
			let awscfg = AWS.Configuration AWS.Timestamp awscreds debugMapper
#if MIN_VERSION_aws(0,17,0)
				Nothing
#endif
			bracketIO (newManager managerSettings) closeManager $ \mgr -> 
				a $ Just $ S3Handle mgr awscfg s3cfg
		Nothing -> a Nothing
  where
	s3cfg = s3Configuration c

s3Configuration :: RemoteConfig -> S3.S3Configuration AWS.NormalQuery
s3Configuration c = cfg
	{ S3.s3Port = port
	, S3.s3RequestStyle = case M.lookup "requeststyle" c of
		Just "path" -> S3.PathStyle
		Just s -> giveup $ "bad S3 requeststyle value: " ++ s
		Nothing -> S3.s3RequestStyle cfg
	}
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
		_ -> giveup $ "bad S3 port value: " ++ s
	cfg = S3.s3 proto endpoint False

data S3Info = S3Info
	{ bucket :: S3.Bucket
	, storageClass :: S3.StorageClass
	, bucketObject :: Key -> String
	, bucketExportLocation :: ExportLocation -> String
	, metaHeaders :: [(T.Text, T.Text)]
	, partSize :: Maybe Integer
	, isIA :: Bool
	, public :: Bool
	, getpublicurl :: Maybe (Key -> URLString)
	}

extractS3Info :: RemoteConfig -> Annex S3Info
extractS3Info c = do
	b <- maybe
		(giveup "S3 bucket not configured")
		(return . T.pack)
		(getBucketName c)
	let info = S3Info
		{ bucket = b
		, storageClass = getStorageClass c
		, bucketObject = getBucketObject c
		, bucketExportLocation = getBucketExportLocation c
		, metaHeaders = getMetaHeaders c
		, partSize = getPartSize c
		, isIA = configIA c
		, public = case M.lookup "public" c of
			Just "yes" -> True
			_ -> False
		, getpublicurl = case M.lookup "publicurl" c of
			Just u -> Just $ genericPublicKeyUrl info u
			Nothing -> case M.lookup "host" c of
				Just h
					| h == AWS.s3DefaultHost ->
						Just $ awsPublicKeyUrl info
					| isIAHost h ->
						Just $ iaPublicKeyUrl info
				_ -> Nothing
		}
	return info

putObject :: S3Info -> T.Text -> RequestBody -> S3.PutObject
putObject info file rbody = (S3.putObject (bucket info) file rbody)
	{ S3.poStorageClass = Just (storageClass info)
	, S3.poMetadata = metaHeaders info
	, S3.poAutoMakeBucket = isIA info
	, S3.poAcl = acl info
	}

acl :: S3Info -> Maybe S3.CannedAcl
acl info
	| public info = Just S3.AclPublicRead
	| otherwise = Nothing

getBucketName :: RemoteConfig -> Maybe BucketName
getBucketName = map toLower <$$> M.lookup "bucket"

getStorageClass :: RemoteConfig -> S3.StorageClass
getStorageClass c = case M.lookup "storageclass" c of
	Just "REDUCED_REDUNDANCY" -> S3.ReducedRedundancy
#if MIN_VERSION_aws(0,13,0)
	Just s -> S3.OtherStorageClass (T.pack s)
#endif
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

getBucketExportLocation :: RemoteConfig -> ExportLocation -> FilePath
getBucketExportLocation c loc = getFilePrefix c ++ fromExportLocation loc

{- Internet Archive documentation limits filenames to a subset of ascii.
 - While other characters seem to work now, this entity encodes everything
 - else to avoid problems. -}
iaMunge :: String -> String
iaMunge = (>>= munge)
  where
	munge c
		| isAsciiUpper c || isAsciiLower c || isNumber c = [c]
		| c `elem` ("_-.\"" :: String) = [c]
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

iaPublicKeyUrl :: S3Info -> Key -> URLString
iaPublicKeyUrl info = genericPublicKeyUrl info $
	"http://archive.org/download/" ++ T.unpack (bucket info) ++ "/" 

awsPublicKeyUrl :: S3Info -> Key -> URLString
awsPublicKeyUrl info = genericPublicKeyUrl info $ 
	"https://" ++ T.unpack (bucket info) ++ ".s3.amazonaws.com/" 

genericPublicKeyUrl :: S3Info -> URLString -> Key -> URLString
genericPublicKeyUrl info baseurl k = baseurl ++ bucketObject info k

genCredentials :: CredPair -> IO AWS.Credentials
genCredentials (keyid, secret) = AWS.Credentials
	<$> pure (T.encodeUtf8 (T.pack keyid))
	<*> pure (T.encodeUtf8 (T.pack secret))
	<*> newIORef []
	<*> pure Nothing

mkLocationConstraint :: AWS.Region -> S3.LocationConstraint
mkLocationConstraint "US" = S3.locationUsClassic
mkLocationConstraint r = r

debugMapper :: AWS.Logger
debugMapper level t = forward "S3" (T.unpack t)
  where
	forward = case level of
		AWS.Debug -> debugM
		AWS.Info -> infoM
		AWS.Warning -> warningM
		AWS.Error -> errorM

s3Info :: RemoteConfig -> S3Info -> [(String, String)]
s3Info c info = catMaybes
	[ Just ("bucket", fromMaybe "unknown" (getBucketName c))
	, Just ("endpoint", w82s (S.unpack (S3.s3Endpoint s3c)))
	, Just ("port", show (S3.s3Port s3c))
	, Just ("storage class", showstorageclass (getStorageClass c))
	, if configIA c
		then Just ("internet archive item", iaItemUrl $ fromMaybe "unknown" $ getBucketName c)
		else Nothing
	, Just ("partsize", maybe "unlimited" (roughSize storageUnits False) (getPartSize c))
	, Just ("public", if public info then "yes" else "no")
	]
  where
	s3c = s3Configuration c
#if MIN_VERSION_aws(0,13,0)
	showstorageclass (S3.OtherStorageClass t) = T.unpack t
#endif
	showstorageclass sc = show sc

getWebUrls :: S3Info -> RemoteConfig -> Key -> Annex [URLString]
getWebUrls info c k
	| exportTree c = return []
	| otherwise = case (public info, getpublicurl info) of
		(True, Just geturl) -> return [geturl k]
		_ -> return []

