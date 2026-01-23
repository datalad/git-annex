{- S3 remotes
 -
 - Copyright 2011-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

#if ! MIN_VERSION_aws(0,25,2)
#warning Building with an old version of the aws library. Recommend updating to 0.25.2, which fixes bugs and is needed for some features.
#endif

module Remote.S3 (remote, iaHost, configIA, iaItemUrl) where

import qualified Aws as AWS
import qualified Aws.Core as AWS
import qualified Aws.S3 as S3
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import qualified Data.Set as S
import qualified System.FilePath.Posix as Posix
import Data.Char
import Data.String
import Data.Maybe
import Data.Time.Clock
import Network.Socket (HostName)
import Network.HTTP.Conduit (Manager)
import Network.HTTP.Client (responseStatus, responseBody, RequestBody(..))
import Network.HTTP.Types
import Network.URI
import Control.Monad.Trans.Resource
import Control.Monad.Catch
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar

import Annex.Common
import Types.Remote
import Types.Export
import qualified Git
import qualified Annex
import Config
import Config.Cost
import Annex.SpecialRemote.Config
import Remote.Helper.Special
import Remote.Helper.Http
import Remote.Helper.ExportImport
import Types.Import
import qualified Remote.Helper.AWS as AWS
import Creds
import Annex.UUID
import Annex.Magic
import Logs.Web
import Logs.MetaData
import Types.MetaData
import Types.ProposedAccepted
import Types.NumCopies
import Utility.Metered
import Utility.DataUnits
import Annex.Content
import qualified Annex.Url as Url
import Utility.Url (extractFromResourceT, UserAgent)
import Annex.Url (getUserAgent, getUrlOptions, withUrlOptions, UrlOptions(..))
import Utility.Env
import Annex.Verify
import qualified Utility.FileIO as F

type BucketName = String
type BucketObject = String

remote :: RemoteType
remote = specialRemoteType $ RemoteType
	{ typename = "S3"
	, enumerate = const (findSpecialRemotes "s3")
	, generate = gen
	, configParser = const $ pure $ RemoteConfigParser
		{ remoteConfigFieldParsers = 
			[ optionalStringParser bucketField
				(FieldDesc "name of bucket to store content in")
			, optionalStringParser hostField
				(FieldDesc "S3 server hostname (default is Amazon S3)")
			, optionalStringParser datacenterField
				(FieldDesc "S3 datacenter to use (US, EU, us-west-1, ..)")
			, optionalStringParser regionField
				(FieldDesc "S3 region to use")
			, optionalStringParser partsizeField
				(FieldDesc "part size for multipart upload (eg 1GiB)")
			, optionalStringParser storageclassField
				(FieldDesc "storage class, eg STANDARD or STANDARD_IA or ONEZONE_IA")
			, yesNoParser restoreField (Just False)
				(FieldDesc "enable restore of files not currently accessible in the bucket")
			, optionalStringParser fileprefixField
				(FieldDesc "prefix to add to filenames in the bucket")
			, yesNoParser versioningField (Just False)
				(FieldDesc "enable versioning of bucket content")
			, yesNoParser publicField (Just False)
				(FieldDesc "allow public read access to the bucket via ACLs (only supported for old Amazon S3 buckets)")
			, optionalStringParser publicurlField
				(FieldDesc "url that can be used by public to download files")
			, optionalStringParser protocolField
				(FieldDesc "http or https")
			, optionalStringParser portField
				(FieldDesc "port to connect to")
			, optionalStringParser requeststyleField
				(FieldDesc "for path-style requests, set to \"path\"")
			, signatureVersionParser signatureField
				(FieldDesc "S3 signature version")
			, optionalStringParser taggingField
				(FieldDesc "tagging header to add when storing on S3")
			, optionalStringParser mungekeysField HiddenField
			, optionalStringParser AWS.s3credsField HiddenField
			]
		, remoteConfigRestPassthrough = Just
			( \f -> isMetaHeader f || isArchiveMetaHeader f
			,
				[ ("x-amz-meta-*", FieldDesc "http headers to add when storing on S3")
				, ("x-archive-meta-*", FieldDesc "http headers to add when storing on Internet Archive")
			  	]
			)
		}
	, setup = s3Setup
	, exportSupported = exportIsSupported
	, importSupported = importIsSupported
	, thirdPartyPopulated = False
	}

bucketField :: RemoteConfigField
bucketField = Accepted "bucket"

hostField :: RemoteConfigField
hostField = Accepted "host"

datacenterField :: RemoteConfigField
datacenterField = Accepted "datacenter"

regionField :: RemoteConfigField
regionField = Accepted "region"

partsizeField :: RemoteConfigField
partsizeField = Accepted "partsize"

storageclassField :: RemoteConfigField
storageclassField = Accepted "storageclass"

fileprefixField :: RemoteConfigField
fileprefixField = Accepted "fileprefix"

restoreField :: RemoteConfigField
restoreField = Accepted "restore"

publicField :: RemoteConfigField
publicField = Accepted "public"

publicurlField :: RemoteConfigField
publicurlField = Accepted "publicurl"

protocolField :: RemoteConfigField
protocolField = Accepted "protocol"

requeststyleField :: RemoteConfigField
requeststyleField = Accepted "requeststyle"

signatureField :: RemoteConfigField
signatureField = Accepted "signature"

taggingField :: RemoteConfigField
taggingField = Accepted "x-amz-tagging"

data SignatureVersion 
	= SignatureVersion Int
	| DefaultSignatureVersion
	| Anonymous

signatureVersionParser :: RemoteConfigField -> FieldDesc -> RemoteConfigFieldParser
signatureVersionParser f fd =
	genParser go f (Just DefaultSignatureVersion) fd
		(Just (ValueDesc "v2 or v4 or anonymous"))
  where
	go "v2" = Just (SignatureVersion 2)
	go "v4" = Just (SignatureVersion 4)
	go "anonymous" = Just Anonymous
	go _ = Nothing

isAnonymous :: ParsedRemoteConfig -> Bool
isAnonymous c = 
	case getRemoteConfigValue signatureField c of
		Just Anonymous -> True
		_ -> False

portField :: RemoteConfigField
portField = Accepted "port"

mungekeysField :: RemoteConfigField
mungekeysField = Accepted "mungekeys"

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = do
	c <- parsedRemoteConfig remote rc
	cst <- remoteCost gc c expensiveRemoteCost
	info <- extractS3Info c
	hdl <- mkS3HandleVar False c gc u
	magic <- liftIO initMagicMime
	return $ new c cst info hdl magic
  where
	new c cst info hdl magic = Just $ specialRemote c
		(store hdl this info magic)
		(retrieve gc hdl rs c info)
		(remove hdl this info)
		(checkKey hdl rs c info)
		this
	  where
		this = Remote
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = storeKeyDummy
			, retrieveKeyFile = retrieveKeyFileDummy
			, retrieveKeyFileInOrder = pure True
			, retrieveKeyFileCheap = Nothing
			-- HttpManagerRestricted is used here, so this is
			-- secure.
			, retrievalSecurityPolicy = RetrievalAllKeysSecure
			, removeKey = removeKeyDummy
			, lockContent = lockContentS3 hdl this rs c info
			, checkPresent = checkPresentDummy
			, checkPresentCheap = False
			, exportActions = ExportActions
				{ storeExport = storeExportS3 hdl this rs info magic
				, retrieveExport = retrieveExportS3 hdl this info
				, removeExport = removeExportS3 hdl this rs info
				, checkPresentExport = checkPresentExportS3 hdl this info
				-- S3 does not have directories.
				, removeExportDirectory = Nothing
				, renameExport = Just $ renameExportS3 hdl this rs info
				}
			, importActions = ImportActions
                                { listImportableContents = listImportableContentsS3 hdl this info c
				, importKey = Nothing
                                , retrieveExportWithContentIdentifier = retrieveExportWithContentIdentifierS3 hdl this rs info
                                , storeExportWithContentIdentifier = storeExportWithContentIdentifierS3 hdl this rs info magic
                                , removeExportWithContentIdentifier = removeExportWithContentIdentifierS3 hdl this rs info
                                , removeExportDirectoryWhenEmpty = Nothing
                                , checkPresentExportWithContentIdentifier = checkPresentExportWithContentIdentifierS3 hdl this info
                                }
			, whereisKey = Just (getPublicWebUrls rs info c)
			, remoteFsck = Nothing
			, repairRepo = Nothing
			, config = c
			, getRepo = return r
			, gitconfig = gc
			, localpath = Nothing
			, readonly = False
			, appendonly = False
			, untrustworthy = False
			, availability = pure GloballyAvailable
			, remotetype = remote
			, mkUnavailable = gen r u (M.insert hostField (Proposed "!dne!") rc) gc rs
			, getInfo = includeCredsInfo c (AWS.creds u) (s3Info c info)
			, claimUrl = Nothing
			, checkUrl = Nothing
			, remoteStateHandle = rs
			}

s3Setup :: SetupStage -> Maybe UUID -> RemoteName -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
s3Setup ss mu _ mcreds c gc = do
	u <- maybe (liftIO genUUID) return mu
	s3Setup' ss u mcreds c gc

s3Setup' :: SetupStage -> UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
s3Setup'  ss u mcreds c gc
	| maybe False (isIAHost . fromProposedAccepted) (M.lookup hostField c) = archiveorg
	| otherwise = defaulthost
  where
	remotename = fromJust (lookupName c)
	defbucket = remotename ++ "-" ++ fromUUID u
	defaults = M.fromList
		[ (datacenterField, Proposed $ T.unpack $ AWS.defaultRegion AWS.S3)
		, (storageclassField, Proposed "STANDARD")
		, (hostField, Proposed AWS.s3DefaultHost)
		, (portField, Proposed "80")
		, (bucketField, Proposed defbucket)
		]

	use fullconfig pc info = do
		enableBucketVersioning ss info pc gc u
		gitConfigSpecialRemote u fullconfig [("s3", "true")]
		return (fullconfig, u)

	defaulthost = do
		(c', encsetup) <- encryptionSetup ss (c `M.union` defaults) gc
		pc <- either giveup return . parseRemoteConfig c'
			=<< configParser remote c'
		c'' <- if isAnonymous pc
			then pure c'
			else do
				v <- setRemoteCredPair ss encsetup pc gc (AWS.creds u) mcreds
				if M.member datacenterField c || M.member regionField c
					then return v
					-- Check if a bucket with this name
					-- already exists, and if so, use
					-- that location, rather than the
					-- default datacenterField.
					else getBucketLocation pc gc u >>= return . \case
						Nothing -> v
						Just loc -> M.insert datacenterField (Proposed $ T.unpack loc) v
		pc' <- either giveup return . parseRemoteConfig c''
			=<< configParser remote c''
		info <- extractS3Info pc'
		checkexportimportsafe pc' info
		case ss of
			Init -> genBucket pc' gc u
			_ -> return ()
		use c'' pc' info

	archiveorg = do
		showNote "Internet Archive mode"
		pc <- either giveup return . parseRemoteConfig c
			=<< configParser remote c
		c' <- if isAnonymous pc
			then pure c
			else setRemoteCredPair ss noEncryptionUsed pc gc (AWS.creds u) mcreds
		-- Ensure user enters a valid bucket name, since
		-- this determines the name of the archive.org item.
		let validbucket = replace " " "-" $ map toLower $
			maybe (giveup "specify bucket=") fromProposedAccepted
				(M.lookup bucketField c')
		let archiveconfig = 
			-- IA accepts x-amz-* as an alias for x-archive-*
			M.mapKeys (Proposed . replace "x-archive-" "x-amz-" . fromProposedAccepted) $
			-- encryption does not make sense here
			M.insert encryptionField (Proposed "none") $
			M.insert bucketField (Proposed validbucket) $
			M.union c' $
			-- special constraints on key names
			M.insert mungekeysField (Proposed "ia") defaults
		pc' <- either giveup return . parseRemoteConfig archiveconfig
			=<< configParser remote archiveconfig
		info <- extractS3Info pc'
		checkexportimportsafe pc' info
		hdl <- mkS3HandleVar False pc' gc u
		withS3HandleOrFail u hdl $
			writeUUIDFile pc' u info
		use archiveconfig pc' info
	
	checkexportimportsafe c' info =
		unlessM (Annex.getRead Annex.force) $
			checkexportimportsafe' c' info
	checkexportimportsafe' c' info
		| versioning info = return ()
		| otherwise = when (exportTree c' && importTree c') $
			giveup $ unwords
				[ "Combining exporttree=yes and importtree=yes"
				, "with an unversioned S3 bucket is not safe;"
				, "exporting can overwrite other modifications"
				, "to files in the bucket."
				, "Recommend you add versioning=yes."
				, "(Or use --force if you don't mind possibly losing data.)"
				]

store :: S3HandleVar -> Remote -> S3Info -> Maybe Magic -> Storer
store mh r info magic = fileStorer $ \k f p -> withS3HandleOrFail (uuid r) mh $ \h -> do
	void $ storeHelper info h magic f (T.pack $ bucketObject info k) p
	-- Store public URL to item in Internet Archive.
	when (isIA info && not (isChunkKey k)) $
		setUrlPresent k (iaPublicUrl info (bucketObject info k))

storeHelper :: S3Info -> S3Handle -> Maybe Magic -> OsPath -> S3.Object -> MeterUpdate -> Annex (Maybe S3Etag, Maybe S3VersionID)
storeHelper info h magic f object p = liftIO $ case partSize info of
	Just partsz | partsz > 0 -> do
		fsz <- getFileSize f
		if fsz > partsz
			then multipartupload fsz partsz
			else singlepartupload
	_ -> singlepartupload
  where
	singlepartupload = runResourceT $ do
		contenttype <- liftIO getcontenttype
		rbody <- liftIO $ httpBodyStorer f p
		let req = (putObject info object rbody)
			{ S3.poContentType = encodeBS <$> contenttype }
		resp <- sendS3Handle h req
		vid <- mkS3VersionID object
			<$> extractFromResourceT (S3.porVersionId resp)
		etag <- extractFromResourceT (Just (S3.porETag resp))
		return (etag, vid)
	multipartupload fsz partsz = runResourceT $ do
		contenttype <- liftIO getcontenttype
		let startreq = (S3.postInitiateMultipartUpload (bucket info) object)
				{ S3.imuStorageClass = Just (storageClass info)
				, S3.imuMetadata = metaHeaders info
				, S3.imuAutoMakeBucket = isIA info
				, S3.imuExpires = Nothing -- TODO set some reasonable expiry
				, S3.imuContentType = fromString <$> contenttype
				}
		uploadid <- S3.imurUploadId <$> sendS3Handle h startreq

		-- The actual part size will be a even multiple of the
		-- 32k chunk size that lazy ByteStrings use.
		let partsz' = (partsz `div` toInteger defaultChunkSize) * toInteger defaultChunkSize

		-- Send parts of the file, taking care to stream each part
		-- w/o buffering in memory, since the parts can be large.
		etags <- bracketIO (F.openBinaryFile f ReadMode) hClose $ \fh -> do
			let sendparts meter etags partnum = do
				pos <- liftIO $ hTell fh
				if pos >= fsz
					then return (reverse etags)
					else do
						-- Calculate size of part that will
						-- be read.
						let sz = min (fsz - pos) partsz'
						let p' = offsetMeterUpdate p (toBytesProcessed pos)
						let numchunks = ceiling (fromIntegral sz / fromIntegral defaultChunkSize :: Double)
						let popper = handlePopper numchunks defaultChunkSize p' fh
						let req = S3.uploadPart (bucket info) object partnum uploadid $
							 RequestBodyStream (fromIntegral sz) popper
						S3.UploadPartResponse { S3.uprETag = etag } <- sendS3Handle h req
						sendparts (offsetMeterUpdate meter (toBytesProcessed sz)) (etag:etags) (partnum + 1)
			sendparts p [] 1

		resp <- sendS3Handle h $ S3.postCompleteMultipartUpload
			(bucket info) object uploadid (zip [1..] etags)
		etag <- extractFromResourceT (Just (S3.cmurETag resp))
		vid <- extractFromResourceT (S3.cmurVersionId resp)
		return (etag, mkS3VersionID object vid)
	getcontenttype = maybe (pure Nothing) (flip getMagicMimeType f) magic

{- Implemented as a fileRetriever, that uses conduit to stream the chunks
 - out to the file. Would be better to implement a byteRetriever, but
 - that is difficult. -}
retrieve :: RemoteGitConfig -> S3HandleVar -> RemoteStateHandle -> ParsedRemoteConfig -> S3Info -> Retriever
retrieve gc hv rs c info = fileRetriever' $ \f k p iv -> withS3Handle hv $ \case
	Right h -> 
		eitherS3VersionID info rs c k (T.pack $ bucketObject info k) >>= \case
			Left failreason -> do
				warning (UnquotedString failreason)
				giveup "cannot download content"
			Right loc -> retrieveHelper gc info h loc f p iv
	Left S3HandleNeedCreds ->
		getPublicWebUrls' rs info c k >>= \case
			Left failreason -> do
				warning (UnquotedString failreason)
				giveup "cannot download content"
			Right us -> unlessM (withUrlOptions Nothing $ downloadUrl False k p iv us f) $
				giveup "failed to download content"

retrieveHelper :: RemoteGitConfig -> S3Info -> S3Handle -> (Either S3.Object S3VersionID) -> OsPath -> MeterUpdate -> Maybe IncrementalVerifier -> Annex ()
retrieveHelper gc info h loc f p iv = retrieveHelper' gc info h f p iv $
	case loc of
		Left o -> S3.getObject (bucket info) o
		Right (S3VersionID o vid) -> (S3.getObject (bucket info) o)
			{ S3.goVersionId = Just vid }

retrieveHelper' :: RemoteGitConfig -> S3Info -> S3Handle -> OsPath -> MeterUpdate -> Maybe IncrementalVerifier -> S3.GetObject -> Annex ()
retrieveHelper' gc info h f p iv req = liftIO $ runResourceT $ do
	S3.GetObjectResponse { S3.gorResponse = rsp } <- handlerestore $ 
		sendS3Handle h req
	Url.sinkResponseFile p iv zeroBytesProcessed f WriteMode rsp
  where
	needrestore st = restore info && statusCode st == 403
	handlerestore a = catchJust (Url.matchStatusCodeException needrestore) a $ \_ -> do
#if MIN_VERSION_aws(0,25,2)
		let tier = case remoteAnnexS3RestoreTier gc of
			Just "bulk" -> S3.RestoreObjectTierBulk
			Just "expedited" -> S3.RestoreObjectTierExpedited
			_ -> S3.RestoreObjectTierStandard
		let days = case remoteAnnexS3RestoreDays gc of
			Just n -> S3.RestoreObjectLifetimeDays n
			Nothing -> S3.RestoreObjectLifetimeDays 1
		let restorereq = S3.restoreObject
			(S3.goBucket req)
			(S3.goObjectName req)
			tier
			days
		restoreresp <- sendS3Handle h $ restorereq
			{ S3.roVersionId = S3.goVersionId req
			}
		case restoreresp of
			S3.RestoreObjectAccepted -> giveup "Restore initiated, try again later."
			S3.RestoreObjectAlreadyInProgress -> giveup "Restore in progress, try again later."
			S3.RestoreObjectAlreadyRestored -> a
#else
		case remoteAnnexS3RestoreTier gc of
			_ -> giveup "git-annex is built with too old a version of the aws library to support restore=yes"
#endif

remove :: S3HandleVar -> Remote -> S3Info -> Remover
remove hv r info _proof k = withS3HandleOrFail (uuid r) hv $ \h -> do
	S3.DeleteObjectResponse <- liftIO $ runResourceT $ sendS3Handle h $
		S3.DeleteObject (T.pack $ bucketObject info k) (bucket info)
	return ()

lockContentS3 :: S3HandleVar -> Remote -> RemoteStateHandle -> ParsedRemoteConfig -> S3Info -> Maybe (Key -> (VerifiedCopy -> Annex a) -> Annex a)
lockContentS3 hv r rs c info
	-- When versioning is enabled, content is never removed from the
	-- remote, so nothing needs to be done to lock the content there,
	-- beyond a sanity check that the content is in fact present.
	| versioning info = Just $ \k callback -> do
		checkVersioning info rs k
		ifM (checkKey hv rs c info k)
			( withVerifiedCopy LockedCopy (uuid r) (return (Right True)) callback
			, giveup $ "content seems to be missing from " ++ name r ++ " despite S3 versioning being enabled"
			)
	| otherwise = Nothing

checkKey :: S3HandleVar -> RemoteStateHandle -> ParsedRemoteConfig -> S3Info -> CheckPresent
checkKey hv rs c info k = withS3Handle hv $ \case
	Right h -> eitherS3VersionID info rs c k (T.pack $ bucketObject info k) >>= \case
		Left failreason -> do
			warning (UnquotedString failreason)
			giveup "cannot check content"
		Right loc -> checkKeyHelper info h loc
	Left S3HandleNeedCreds ->
		getPublicWebUrls' rs info c k >>= \case
			Left failreason -> do
				warning (UnquotedString failreason)
				giveup "cannot check content"
			Right us -> do
				let check u = withUrlOptions Nothing $ 
					Url.checkBoth u (fromKey keySize k)
				anyM check us

checkKeyHelper :: S3Info -> S3Handle -> (Either S3.Object S3VersionID) -> Annex Bool
checkKeyHelper info h loc = checkKeyHelper' info h o limit
  where
	(o, limit) = case loc of
		Left obj ->
			(obj, id)
		Right (S3VersionID obj vid) ->
			(obj, \ho -> ho { S3.hoVersionId = Just vid })

checkKeyHelper' :: S3Info -> S3Handle -> S3.Object -> (S3.HeadObject -> S3.HeadObject) -> Annex Bool
checkKeyHelper' info h o limit = liftIO $ runResourceT $ do
	rsp <- sendS3Handle h req
	extractFromResourceT (isJust $ S3.horMetadata rsp)
  where
	req = limit $ S3.headObject (bucket info) o

storeExportS3 :: S3HandleVar -> Remote -> RemoteStateHandle -> S3Info -> Maybe Magic -> OsPath -> Key -> ExportLocation -> MeterUpdate -> Annex ()
storeExportS3 hv r rs info magic f k loc p = void $ storeExportS3' hv r rs info magic f k loc p

storeExportS3' :: S3HandleVar -> Remote -> RemoteStateHandle -> S3Info -> Maybe Magic -> OsPath -> Key -> ExportLocation -> MeterUpdate -> Annex (Maybe S3Etag, Maybe S3VersionID)
storeExportS3' hv r rs info magic f k loc p = withS3Handle hv $ \case
	Right h -> go h
	Left pr -> giveupS3HandleProblem pr (uuid r)
  where
	go h = do
		let o = T.pack $ bucketExportLocation info loc
		(metag, mvid) <- storeHelper info h magic f o p
		setS3VersionID info rs k mvid
		return (metag, mvid)

retrieveExportS3 :: S3HandleVar -> Remote -> S3Info -> Key -> ExportLocation -> OsPath -> MeterUpdate -> Annex Verification
retrieveExportS3 hv r info k loc f p = verifyKeyContentIncrementally AlwaysVerify k $ \iv ->
	withS3Handle hv $ \case
		Right h -> retrieveHelper (gitconfig r) info h (Left (T.pack exportloc)) f p iv
		Left S3HandleNeedCreds -> case getPublicUrlMaker info of
			Just geturl -> either giveup return =<<
				withUrlOptions Nothing
					(Url.download' p iv (geturl exportloc) f)
			Nothing -> giveup $ needS3Creds (uuid r)
  where
	exportloc = bucketExportLocation info loc

removeExportS3 :: S3HandleVar -> Remote -> RemoteStateHandle -> S3Info -> Key -> ExportLocation -> Annex ()
removeExportS3 hv r rs info k loc = withS3Handle hv $ \case
	Right h -> do
		checkVersioning info rs k
		liftIO $ runResourceT $ do
			S3.DeleteObjectResponse <- sendS3Handle h $
				S3.DeleteObject (T.pack $ bucketExportLocation info loc) (bucket info)
			return ()
	Left p -> giveupS3HandleProblem p (uuid r)

checkPresentExportS3 :: S3HandleVar -> Remote -> S3Info -> Key -> ExportLocation -> Annex Bool
checkPresentExportS3 hv r info k loc = withS3Handle hv $ \case
	Right h -> checkKeyHelper info h (Left (T.pack $ bucketExportLocation info loc))
	Left S3HandleNeedCreds -> case getPublicUrlMaker info of
		Just geturl -> withUrlOptions Nothing $
			Url.checkBoth (geturl $ bucketExportLocation info loc) (fromKey keySize k)
		Nothing -> giveupS3HandleProblem S3HandleNeedCreds (uuid r)

-- S3 has no move primitive; copy and delete.
renameExportS3 :: S3HandleVar -> Remote -> RemoteStateHandle -> S3Info -> Key -> ExportLocation -> ExportLocation -> Annex (Maybe ())
renameExportS3 hv r rs info k src dest = Just <$> go
  where
	go = withS3Handle hv $ \case
		Right h -> do
			checkVersioning info rs k
			go' h
		Left p -> giveupS3HandleProblem p (uuid r)
	
	go' h = liftIO $ runResourceT $ do
		let co = S3.copyObject (bucket info) dstobject
			(S3.ObjectId (bucket info) srcobject Nothing)
			S3.CopyMetadata
		-- ACL is not preserved by copy.
		void $ sendS3Handle h $ co { S3.coAcl = acl info }
		void $ sendS3Handle h $ S3.DeleteObject srcobject (bucket info)
	
	srcobject = T.pack $ bucketExportLocation info src
	dstobject = T.pack $ bucketExportLocation info dest

listImportableContentsS3 :: S3HandleVar -> Remote -> S3Info -> ParsedRemoteConfig -> Annex (Maybe (ImportableContentsChunkable Annex (ContentIdentifier, ByteSize)))
listImportableContentsS3 hv r info c =
	withS3Handle hv $ \case
		Right h -> Just <$> go h
		Left p -> giveupS3HandleProblem p (uuid r)
  where
	go h = do
		ic <- liftIO $ runResourceT $ extractFromResourceT =<< startlist h
		return (ImportableContentsComplete ic)

	fileprefix = T.pack <$> getRemoteConfigValue fileprefixField c

	startlist h
		| versioning info = do
			rsp <- sendS3Handle h $ 
				S3.getBucketObjectVersions (bucket info)
			continuelistversioned Nothing h [] rsp
		| otherwise = do
			rsp <- sendS3Handle h $ 
				(S3.getBucket (bucket info))
					{ S3.gbPrefix = fileprefix }
			continuelistunversioned h [] rsp

	continuelistunversioned h l rsp
		| S3.gbrIsTruncated rsp =
			let marker = 
				S3.gbrNextMarker rsp
					<|>
				(S3.objectKey <$> lastMaybe (S3.gbrContents rsp))
			in case marker of
				Just _ -> do
					rsp' <- sendS3Handle h $
						(S3.getBucket (bucket info))
							{ S3.gbMarker = marker
							, S3.gbPrefix = fileprefix
							}
					l' <- extractFromResourceT $
						extractunversioned rsp
					continuelistunversioned h (l':l) rsp'
				Nothing -> nomore
		| otherwise = nomore
	  where
		nomore = do
			l' <- extractFromResourceT $
				extractunversioned rsp
			return $ mkImportableContentsUnversioned
				(reverse (l':l))

	continuelistversioned reuse h l rsp
		| S3.gbovrIsTruncated rsp = do
			rsp' <- sendS3Handle h $
				(S3.getBucketObjectVersions (bucket info))
					{ S3.gbovKeyMarker = S3.gbovrNextKeyMarker rsp
					, S3.gbovVersionIdMarker = S3.gbovrNextVersionIdMarker rsp
					, S3.gbovPrefix = fileprefix
					}
			(l', reuse') <- extractFromResourceT $
				extractversioned reuse rsp
			continuelistversioned reuse' h (l':l) rsp'
		| otherwise = do
			(l', _) <- extractFromResourceT $
				extractversioned reuse rsp
			return $ mkImportableContentsVersioned
				(reverse (l':l))

	extractunversioned = mapMaybe extractunversioned' . S3.gbrContents
	extractunversioned' oi = do
                  loc <- bucketImportLocation info $
                          T.unpack $ S3.objectKey oi
                  let sz  = S3.objectSize oi
                  let cid = mkS3UnversionedContentIdentifier $ S3.objectETag oi
                  return (loc, (cid, sz))
	
	extractversioned reuse = extractversioned' reuse . S3.gbovrContents
	extractversioned' reuse [] = ([], reuse)
	extractversioned' reuse (x:xs) = case extractversioned'' reuse x of
		Just (v, reuse') -> 
			let (l, reuse'') = extractversioned' reuse' xs
			in (v:l, reuse'')
		Nothing -> extractversioned' reuse xs
	extractversioned'' reuse ovi@(S3.ObjectVersion {}) = do
		loc <- bucketImportLocation info $
			T.unpack $ S3.oviKey ovi
		-- Avoid storing the same filename in memory repeatedly.
		let loc' = case reuse of
			Just reuseloc | reuseloc == loc -> reuseloc
			_ -> loc
		let sz  = S3.oviSize ovi
		let cid = mkS3VersionedContentIdentifier' ovi
		return (((loc', (cid, sz)), S3.oviLastModified ovi), Just loc')
	extractversioned'' _ (S3.DeleteMarker {}) = Nothing

mkImportableContentsUnversioned :: [[(ImportLocation, (ContentIdentifier, ByteSize))]] -> ImportableContents (ContentIdentifier, ByteSize)
mkImportableContentsUnversioned l = ImportableContents 
	{ importableContents = concat l
	, importableHistory = []
	}

mkImportableContentsVersioned :: [[((ImportLocation, (ContentIdentifier, ByteSize)), UTCTime)]] -> ImportableContents (ContentIdentifier, ByteSize)
mkImportableContentsVersioned = build . groupfiles
  where
	ovilastmodified = snd
	loc = fst . fst

	build [] = ImportableContents [] []
	build l =
		let (l', v) = latestversion l
		in ImportableContents
			{ importableContents = map fst v
			, importableHistory = case build l' of
				ImportableContents [] [] -> []
				h -> [h]
			}
	
	-- group files so all versions of a file are in a sublist,
	-- with the newest first. S3 uses such an order, so it's just a
	-- matter of breaking up the response list into sublists.
	groupfiles = groupBy (\a b -> loc a == loc b) . concat

	latestversion [] = ([], [])
	latestversion ([]:rest) = latestversion rest
	latestversion l@((first:_old):remainder) =
		go (ovilastmodified first) [first] remainder
	  where
		go mtime c [] = (removemostrecent mtime l, reverse c)
		go mtime c ([]:rest) = go mtime c rest
		go mtime c ((latest:_old):rest) = 
			let !mtime' = max mtime (ovilastmodified latest)
			in go mtime' (latest:c) rest
	
	removemostrecent _ [] = []
	removemostrecent mtime ([]:rest) = removemostrecent mtime rest
	removemostrecent mtime (i@(curr:old):rest)
		| ovilastmodified curr == mtime =
			old : removemostrecent mtime rest
		| otherwise =
			i : removemostrecent mtime rest

retrieveExportWithContentIdentifierS3 :: S3HandleVar -> Remote -> RemoteStateHandle -> S3Info -> ExportLocation -> [ContentIdentifier] -> OsPath -> Either Key (Annex Key) -> MeterUpdate -> Annex (Key, Verification)
retrieveExportWithContentIdentifierS3 hv r rs info loc (cid:_) dest gk p =
	case gk of
		Right _mkkey -> do
			k <- go Nothing
			return (k, UnVerified)
		Left k -> do
			v <- verifyKeyContentIncrementally DefaultVerify k 
				(void . go)
			return (k, v)
  where
	go iv = withS3Handle hv $ \case
		Right h -> do
			rewritePreconditionException $ retrieveHelper' (gitconfig r) info h dest p iv $
				limitGetToContentIdentifier cid $
					S3.getObject (bucket info) o
			k <- either return id gk
			case extractContentIdentifier cid o of
				Right vid -> do
					vids <- getS3VersionID rs k
					unless (vid `elem` map Just vids) $
						setS3VersionID info rs k vid
				Left _ -> noop
			return k
		Left pr -> giveupS3HandleProblem pr (uuid r)
	o = T.pack $ bucketExportLocation info loc
retrieveExportWithContentIdentifierS3 _ _ _ _ _ [] _ _ _ = giveup "missing content identifier"

{- Catch exception getObject returns when a precondition is not met,
 - and replace with a more understandable message for the user. -}
rewritePreconditionException :: Annex a -> Annex a
rewritePreconditionException a = catchJust (Url.matchStatusCodeException want) a $ 
	const $ giveup "requested version of object is not available in S3 bucket"
  where
	want st = statusCode st == 412 && 
		statusMessage st == "Precondition Failed"

-- Does not check if content on S3 is safe to overwrite, because there
-- is no atomic way to do so. When the bucket is versioned, this is
-- acceptable because listImportableContentsS3 will find versions
-- of files that were overwritten by this and no data is lost.
--
-- When the bucket is not versioned, data loss can result.
-- This is why that configuration requires --force to enable.
storeExportWithContentIdentifierS3 :: S3HandleVar -> Remote -> RemoteStateHandle -> S3Info -> Maybe Magic -> OsPath -> Key -> ExportLocation -> [ContentIdentifier] -> MeterUpdate -> Annex ContentIdentifier
storeExportWithContentIdentifierS3 hv r rs info magic src k loc _overwritablecids p
	| versioning info = go
	| otherwise = go
  where
	go = storeExportS3' hv r rs info magic src k loc p >>= \case
		(_, Just vid) -> return $
			mkS3VersionedContentIdentifier vid
		(Just etag, Nothing) -> return $
			mkS3UnversionedContentIdentifier etag
		(Nothing, Nothing) -> 
			giveup "did not get ETag for store to S3 bucket"

-- Does not guarantee that the removed object has the content identifier,
-- but when the bucket is versioned, the removed object content can still
-- be recovered (and listImportableContentsS3 will find it).
-- 
-- When the bucket is not versioned, data loss can result.
-- This is why that configuration requires --force to enable.
removeExportWithContentIdentifierS3 :: S3HandleVar -> Remote -> RemoteStateHandle -> S3Info -> Key -> ExportLocation -> [ContentIdentifier] -> Annex ()
removeExportWithContentIdentifierS3 hv r rs info k loc _removeablecids =
	removeExportS3 hv r rs info k loc

checkPresentExportWithContentIdentifierS3 :: S3HandleVar -> Remote -> S3Info -> Key -> ExportLocation -> [ContentIdentifier] -> Annex Bool
checkPresentExportWithContentIdentifierS3 hv r info _k loc knowncids =
	withS3HandleOrFail (uuid r) hv $ \h ->
		flip anyM knowncids $
			checkKeyHelper' info h o . limitHeadToContentIdentifier
  where
	o = T.pack $ bucketExportLocation info loc

getBucketLocation :: ParsedRemoteConfig -> RemoteGitConfig -> UUID -> Annex (Maybe S3.LocationConstraint)
getBucketLocation c gc u = do
	info <- extractS3Info c
	let info' = info { region = Nothing, host = Nothing }
	-- Force anonymous access, because this API call does not work
	-- when used in an authenticated context.
	hdl <- mkS3HandleVar True c gc u
	withS3HandleOrFail u hdl $ \h -> do
		r <- liftIO $ tryNonAsync $ runResourceT $
			sendS3Handle h (S3.getBucketLocation $ bucket info')
		return $ either (const Nothing) (Just . S3.gblrLocationConstraint) r

{- Generate the bucket if it does not already exist, including creating the
 - UUID file within the bucket.
 -
 - Some ACLs can allow read/write to buckets, but not querying them,
 - so first check if the UUID file already exists and we can skip creating
 - it.
 -}
genBucket :: ParsedRemoteConfig -> RemoteGitConfig -> UUID -> Annex ()
genBucket c gc u = do
	showAction "checking bucket"
	info <- extractS3Info c
	hdl <- mkS3HandleVar False c gc u
	withS3HandleOrFail u hdl $ \h ->
		go info h =<< checkUUIDFile c u info h
  where
	go _ _ (Right True) = noop
	go info h _ = do
		checkbucketexists info h >>= \case
			Right True -> noop
			Right False -> createbucket info h
			Left err -> do
				fastDebug "Remote.S3" ("createBucket threw exception: " ++ show err)
				createbucket info h
		writeUUIDFile c u info h
		
	checkbucketexists info h = liftIO $ tryNonAsync $ runResourceT $ do
		void $ sendS3Handle h (S3.getBucket $ bucket info)
		return True
	
	createbucket info h = do
		showAction $ UnquotedString $ "creating bucket in " ++ datacenter
		void $ liftIO $ runResourceT $ sendS3Handle h $ 
			(S3.putBucket (bucket info))
				{ S3.pbCannedAcl = acl info
				, S3.pbLocationConstraint = locconstraint
				, S3.pbXStorageClass = storageclass
				}
	
	locconstraint = mkLocationConstraint $ T.pack datacenter
	datacenter = fromJust $ getRemoteConfigValue datacenterField c
	-- "NEARLINE" as a storage class when creating a bucket is a
	-- nonstandard extension of Google Cloud Storage.
	storageclass = case getStorageClass c of
		sc@(S3.OtherStorageClass "NEARLINE") -> Just sc
		_ -> Nothing

{- Writes the UUID to an annex-uuid file within the bucket.
 -
 - If the file already exists in the bucket, it must match,
 - or this fails.
 -
 - Note that IA buckets can only created by having a file
 - stored in them. So this also takes care of that.
 -
 - Not done for import/export buckets.
 -}
writeUUIDFile :: ParsedRemoteConfig -> UUID -> S3Info -> S3Handle -> Annex ()
writeUUIDFile c u info h = unless (exportTree c || importTree c) $ do
	v <- checkUUIDFile c u info h
	case v of
		Right True -> noop
		Right False -> do
			warning "The bucket already exists, and its annex-uuid file indicates it is used by a different special remote."
			giveup "Cannot reuse this bucket."
		_ -> void $ liftIO $ runResourceT $ sendS3Handle h mkobject
  where
	file = T.pack $ fromOsPath $ uuidFile c
	uuidb = L.fromChunks [T.encodeUtf8 $ T.pack $ fromUUID u]

	mkobject = putObject info file (RequestBodyLBS uuidb)

{- Checks if the UUID file exists in the bucket
 - and has the specified UUID already.
 -
 - Not done for import/export buckets. -}
checkUUIDFile :: ParsedRemoteConfig -> UUID -> S3Info -> S3Handle -> Annex (Either SomeException Bool)
checkUUIDFile c u info h 
	| exportTree c || importTree c = pure (Right False)
	| otherwise = tryNonAsync $ liftIO $ runResourceT $ do
		resp <- tryS3 $ sendS3Handle h (S3.getObject (bucket info) file)
		case resp of
			Left _ -> return False
			Right r -> do
				v <- AWS.loadToMemory r
				extractFromResourceT (check v)
  where
	check (S3.GetObjectMemoryResponse _meta rsp) =
		responseStatus rsp == ok200 && responseBody rsp == uuidb

	file = T.pack $ fromOsPath $ uuidFile c
	uuidb = L.fromChunks [T.encodeUtf8 $ T.pack $ fromUUID u]

uuidFile :: ParsedRemoteConfig -> OsPath
uuidFile c = toOsPath (getFilePrefix c) <> literalOsPath "annex-uuid"

tryS3 :: ResourceT IO a -> ResourceT IO (Either S3.S3Error a)
tryS3 a = (Right <$> a) `catch` (pure . Left)

data S3Handle = S3Handle
	{ hmanager :: Manager
	, hawscfg :: AWS.Configuration
	, hs3cfg :: S3.S3Configuration AWS.NormalQuery
	}

{- Sends a request to S3 and gets back the response. -}
sendS3Handle
	:: (AWS.Transaction r a, AWS.ServiceConfiguration r ~ S3.S3Configuration)
	=> S3Handle
	-> r
	-> ResourceT IO a
sendS3Handle h r = AWS.pureAws (hawscfg h) (hs3cfg h) (hmanager h) r

type S3HandleVar = TVar (Either (Annex (Either S3HandleProblem S3Handle)) (Either S3HandleProblem S3Handle))

data S3HandleProblem = S3HandleNeedCreds

giveupS3HandleProblem :: S3HandleProblem -> UUID -> Annex a
giveupS3HandleProblem S3HandleNeedCreds u = do
	warning $ UnquotedString $ needS3Creds u
	giveup "No S3 credentials configured"

{- Prepares a S3Handle for later use. Does not connect to S3 or do anything
 - else expensive. -}
mkS3HandleVar :: Bool -> ParsedRemoteConfig -> RemoteGitConfig -> UUID -> Annex S3HandleVar
mkS3HandleVar forceanonymous c gc u = liftIO $ newTVarIO $ Left $
	if forceanonymous || isAnonymous c
		then go =<< liftIO AWS.anonymousCredentials
		else do
			mcreds <- getRemoteCredPair c gc (AWS.creds u)
			case mcreds of
				Just creds -> go =<< liftIO (genCredentials creds)
				Nothing -> return (Left S3HandleNeedCreds)
  where
	go awscreds = do
		ou <- getUrlOptions Nothing
		ua <- getUserAgent
		let awscfg = AWS.Configuration AWS.Timestamp awscreds debugMapper Nothing
		let s3cfg = s3Configuration (Just ua) c
		return $ Right $ S3Handle (httpManager ou) awscfg s3cfg

withS3Handle :: S3HandleVar -> (Either S3HandleProblem S3Handle -> Annex a) -> Annex a
withS3Handle hv a = liftIO (readTVarIO hv) >>= \case
	Right hdl -> a hdl
	Left mkhdl -> do
		hdl <- mkhdl
		liftIO $ atomically $ writeTVar hv (Right hdl)
		a hdl

withS3HandleOrFail :: UUID -> S3HandleVar -> (S3Handle -> Annex a) -> Annex a
withS3HandleOrFail u hv a = withS3Handle hv $ \case
	Right hdl -> a hdl
	Left p -> giveupS3HandleProblem p u

needS3Creds :: UUID -> String
needS3Creds u = missingCredPairFor "S3" (AWS.creds u)

s3Configuration :: Maybe UserAgent -> ParsedRemoteConfig -> S3.S3Configuration AWS.NormalQuery
#if MIN_VERSION_aws(0,24,3)
s3Configuration ua c = cfg
#else
s3Configuration _ua c = cfg
#endif
	{ S3.s3Port = port
	, S3.s3RequestStyle = case getRemoteConfigValue requeststyleField c of
		Just "path" -> S3.PathStyle
		Just s -> giveup $ "bad S3 requeststyle value: " ++ s
		Nothing -> S3.s3RequestStyle cfg
#if MIN_VERSION_aws(0,24,3)
	, S3.s3UserAgent = T.pack <$> ua
#endif
	}
  where
	h = fromJust $ getRemoteConfigValue hostField c
	datacenter = fromJust $ getRemoteConfigValue datacenterField c
	-- When the default S3 host is configured, connect directly to
	-- the S3 endpoint for the configured datacenter.
	-- When another host is configured, it's used as-is.
	endpoint
		| h == AWS.s3DefaultHost = AWS.s3HostName $ T.pack datacenter
		| otherwise = T.encodeUtf8 $ T.pack h
	port = case getRemoteConfigValue portField c of
		Just s -> 
			case reads s of
				[(p, _)]
					-- Let protocol setting override
					-- default port 80.
					| p == 80 -> case cfgproto of
						Just AWS.HTTPS -> 443
						_ -> p
					| otherwise -> p
				_ -> giveup $ "bad S3 port value: " ++ s
		Nothing -> case cfgproto of
			Just AWS.HTTPS -> 443
			Just AWS.HTTP -> 80
			Nothing -> 80
	cfgproto = case getRemoteConfigValue protocolField c of
		Just "https" -> Just AWS.HTTPS
		Just "http" -> Just AWS.HTTP
		Just s -> giveup $ "bad S3 protocol value: " ++ s
		Nothing -> Nothing
	proto = case cfgproto of
		Just v -> v
		Nothing
			| port == 443 -> AWS.HTTPS
			| otherwise -> AWS.HTTP
	cfg = if usev4 $ getRemoteConfigValue signatureField c
		then (S3.s3v4 proto endpoint False S3.SignWithEffort)
			{ S3.s3Region = r }
		else (S3.s3 proto endpoint False)
			{ S3.s3Region = r }
	
	-- Use signature v4 for all AWS hosts by default, but don't use it by
	-- default for other S3 hosts, which may not support it.
	usev4 (Just DefaultSignatureVersion)
		| h == AWS.s3DefaultHost = True
		| otherwise = False
	usev4 (Just (SignatureVersion 4)) = True
	usev4 (Just (SignatureVersion _)) = False
	usev4 (Just Anonymous) = False
	usev4 Nothing = False

	r = encodeBS <$> getRemoteConfigValue regionField c

data S3Info = S3Info
	{ bucket :: S3.Bucket
	, storageClass :: S3.StorageClass
	, bucketObject :: Key -> BucketObject
	, bucketExportLocation :: ExportLocation -> BucketObject
	, bucketImportLocation :: BucketObject -> Maybe ImportLocation
	, metaHeaders :: [(T.Text, T.Text)]
	, tagging :: [(T.Text, T.Text)]
	, partSize :: Maybe Integer
	, isIA :: Bool
	, versioning :: Bool
	, restore :: Bool
	, publicACL :: Bool
	, publicurl :: Maybe URLString
	, host :: Maybe String
	, region :: Maybe String
	}

extractS3Info :: ParsedRemoteConfig -> Annex S3Info
extractS3Info c = do
	b <- maybe
		(giveup "S3 bucket not configured")
		(return . T.pack)
		(getBucketName c)
	return $ S3Info
		{ bucket = b
		, storageClass = getStorageClass c
		, bucketObject = getBucketObject c
		, bucketExportLocation = getBucketExportLocation c
		, bucketImportLocation = getBucketImportLocation c
		, metaHeaders = getMetaHeaders c
		, tagging = getTagging c
		, partSize = getPartSize c
		, isIA = configIA c
		, versioning = fromMaybe False $
			getRemoteConfigValue versioningField c
		, restore = fromMaybe False $
			getRemoteConfigValue restoreField c
		, publicACL = fromMaybe False $
			getRemoteConfigValue publicField c
		, publicurl = getRemoteConfigValue publicurlField c
		, host = getRemoteConfigValue hostField c
		, region = getRemoteConfigValue regionField c
		}

putObject :: S3Info -> T.Text -> RequestBody -> S3.PutObject
putObject info file rbody = (S3.putObject (bucket info) file rbody)
	{ S3.poStorageClass = Just (storageClass info)
	, S3.poMetadata = metaHeaders info
	, S3.poAutoMakeBucket = isIA info
	, S3.poAcl = acl info
#if MIN_VERSION_aws(0,25,0)
	, S3.poTagging = tagging info
#endif
	}

acl :: S3Info -> Maybe S3.CannedAcl
acl info
	| publicACL info = Just S3.AclPublicRead
	| otherwise = Nothing

getBucketName :: ParsedRemoteConfig -> Maybe BucketName
getBucketName = map toLower <$$> getRemoteConfigValue bucketField

getStorageClass :: ParsedRemoteConfig -> S3.StorageClass
getStorageClass c = case getRemoteConfigValue storageclassField c of
	Just s -> S3.OtherStorageClass (T.pack s)
	_ -> S3.Standard

getPartSize :: ParsedRemoteConfig -> Maybe Integer
getPartSize c = readSize dataUnits =<< getRemoteConfigValue partsizeField c

getMetaHeaders :: ParsedRemoteConfig -> [(T.Text, T.Text)]
getMetaHeaders = map munge
	. filter (isMetaHeader . fst)
	. M.assocs
	. getRemoteConfigPassedThrough
  where
	metaprefixlen = length metaPrefix
	munge (k, v) = (T.pack $ drop metaprefixlen (fromProposedAccepted k), T.pack v)

getTagging :: ParsedRemoteConfig -> [(T.Text, T.Text)]
getTagging c = case getRemoteConfigValue taggingField c of
	Nothing -> []
	Just s -> map go $ parseQueryText (encodeBS s)
  where
	go (k, Just v) = (k, v)
	go (k, Nothing) = (k, mempty)

isMetaHeader :: RemoteConfigField -> Bool
isMetaHeader h = metaPrefix `isPrefixOf` fromProposedAccepted h

isArchiveMetaHeader :: RemoteConfigField -> Bool
isArchiveMetaHeader h = "x-archive-" `isPrefixOf` fromProposedAccepted h

metaPrefix :: String
metaPrefix = "x-amz-meta-"

getFilePrefix :: ParsedRemoteConfig -> String
getFilePrefix = fromMaybe "" . getRemoteConfigValue fileprefixField

getBucketObject :: ParsedRemoteConfig -> Key -> BucketObject
getBucketObject c = munge . serializeKey
  where
	munge s = case getRemoteConfigValue mungekeysField c :: Maybe String of
		Just "ia" -> iaMunge $ getFilePrefix c ++ s
		_ -> getFilePrefix c ++ s

getBucketExportLocation :: ParsedRemoteConfig -> ExportLocation -> BucketObject
getBucketExportLocation c loc =
	getFilePrefix c ++ fromOsPath (fromExportLocation loc)

getBucketImportLocation :: ParsedRemoteConfig -> BucketObject -> Maybe ImportLocation
getBucketImportLocation c obj
	-- The uuidFile should not be imported.
	| obj == fromOsPath uuidfile = Nothing
	-- Only import files that are under the fileprefix, when
	-- one is configured.
	| prefix `isPrefixOf` obj = Just $ mkImportLocation $
		toOsPath $ drop prefixlen obj
	| otherwise = Nothing
  where
	prefix = getFilePrefix c
	prefixlen = length prefix
	uuidfile = uuidFile c

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

configIA :: ParsedRemoteConfig -> Bool
configIA = maybe False isIAHost . getRemoteConfigValue hostField

{- Hostname to use for archive.org S3. -}
iaHost :: HostName
iaHost = "s3.us.archive.org"

isIAHost :: HostName -> Bool
isIAHost h = ".archive.org" `isSuffixOf` map toLower h

iaItemUrl :: BucketName -> URLString
iaItemUrl b = "http://archive.org/details/" ++ b

iaPublicUrl :: S3Info -> BucketObject -> URLString
iaPublicUrl info = genericPublicUrl $
	"http://archive.org/download/" ++ T.unpack (bucket info) ++ "/" 

awsPublicUrl :: S3Info -> BucketObject -> URLString
awsPublicUrl info = genericPublicUrl $ 
	"https://" ++ T.unpack (bucket info) ++ ".s3.amazonaws.com/" 

genericPublicUrl :: URLString -> BucketObject -> URLString
genericPublicUrl baseurl p = 
	baseurl Posix.</> escapeURIString skipescape p
 where
	-- Don't need to escape '/' because the bucket object
	-- is not necessarily a single url component. 
	-- But do want to escape eg '+' and ' '
	skipescape '/' = True
	skipescape c = isUnescapedInURIComponent c

genCredentials :: CredPair -> IO AWS.Credentials
genCredentials (keyid, secret) = do
	cr <- AWS.makeCredentials (tobs keyid) (tobs secret)
	tk <- fmap tobs <$> getEnv "AWS_SESSION_TOKEN"
	return (cr { AWS.iamToken = tk })
  where
	tobs = T.encodeUtf8 . T.pack

mkLocationConstraint :: AWS.Region -> S3.LocationConstraint
mkLocationConstraint "US" = S3.locationUsClassic
mkLocationConstraint r = r

debugMapper :: AWS.Logger
debugMapper level t = forward "Remote.S3" (T.unpack t)
  where
	forward = case level of
		AWS.Debug -> debug
		AWS.Warning -> debug
		AWS.Error -> debug
		AWS.Info -> \_ _ -> return ()

s3Info :: ParsedRemoteConfig -> S3Info -> [(String, String)]
s3Info c info = catMaybes
	[ Just ("bucket", fromMaybe "unknown" (getBucketName c))
	, Just ("endpoint", decodeBS (S3.s3Endpoint s3c))
	, case S3.s3Region s3c of
		Nothing -> Nothing
		Just r -> Just ("region", decodeBS r)
	, Just ("port", show (S3.s3Port s3c))
	, Just ("protocol", map toLower (show (S3.s3Protocol s3c)))
	, Just ("storage class", showstorageclass (getStorageClass c))
	, if configIA c
		then Just ("internet archive item", iaItemUrl $ fromMaybe "unknown" $ getBucketName c)
		else Nothing
	, Just ("partsize", maybe "unlimited" (roughSize storageUnits False) (getPartSize c))
	, Just ("publicurl", fromMaybe "" (publicurl info))
	, Just ("public", if publicACL info then "yes" else "no")
	, Just ("versioning", if versioning info then "yes" else "no")
	]
  where
	s3c = s3Configuration Nothing c
	showstorageclass (S3.OtherStorageClass t) = T.unpack t
	showstorageclass sc = show sc

getPublicWebUrls :: RemoteStateHandle -> S3Info -> ParsedRemoteConfig -> Key -> Annex [URLString]
getPublicWebUrls rs info c k = either (const []) id <$> getPublicWebUrls' rs info c k

getPublicWebUrls' :: RemoteStateHandle -> S3Info -> ParsedRemoteConfig -> Key -> Annex (Either String [URLString])
getPublicWebUrls' rs info c k
	| exportTree c = if versioning info
		then case publicurl info of
			Just url -> getversionid (const $ genericPublicUrl url)
			Nothing -> case host info of
				Just h | h == AWS.s3DefaultHost ->
					getversionid awsPublicUrl
				_ -> return nopublicurl
		else return (Left "exporttree used without versioning")
	| otherwise = case getPublicUrlMaker info of
		Just geturl -> return (Right [geturl $ bucketObject info k])
		Nothing -> return nopublicurl
  where
	nopublicurl = Left "No publicurl is configured for this remote"
	getversionid url = getS3VersionIDPublicUrls url info rs k >>= \case
		[] -> return (Left "Remote is configured to use versioning, but no S3 version ID is recorded for this key")
		l -> return (Right l)

getPublicUrlMaker :: S3Info -> Maybe (BucketObject -> URLString)
getPublicUrlMaker info = case publicurl info of
	Just url -> Just (genericPublicUrl url)
	Nothing -> case host info of
		Just h
			| h == AWS.s3DefaultHost ->
				Just (awsPublicUrl info)
			| isIAHost h ->
				Just (iaPublicUrl info)
		_ -> Nothing

-- S3 uses a unique version id for each object stored on it.
--
-- The Object is included in this because retrieving a particular
-- version id involves a request for an object, so this keeps track of what
-- the object is.
data S3VersionID = S3VersionID S3.Object T.Text
	deriving (Show, Eq)

-- smart constructor
mkS3VersionID :: S3.Object -> Maybe T.Text -> Maybe S3VersionID
mkS3VersionID o (Just t)
	| T.null t = Nothing
	-- AWS documentation says a version ID is at most 1024 bytes long.
	-- Since they are stored in the git-annex branch, prevent them from
	-- being very much larger than that.
	| T.length t < 2048 = Just (S3VersionID o t)
	| otherwise = Nothing
mkS3VersionID _ Nothing = Nothing

-- Format for storage in per-remote metadata.
--
-- A S3 version ID is "url ready" so does not contain '#' and so we'll use
-- that to separate it from the object id. (Could use a space, but spaces
-- in metadata values lead to an inefficient encoding.)
formatS3VersionID :: S3VersionID -> BS.ByteString
formatS3VersionID (S3VersionID o v) = T.encodeUtf8 v <> "#" <> T.encodeUtf8 o

-- Parse from value stored in per-remote metadata.
parseS3VersionID :: BS.ByteString -> Maybe S3VersionID
parseS3VersionID b = do
	let (v, rest) = B8.break (== '#') b
	o <- eitherToMaybe $ T.decodeUtf8' $ BS.drop 1 rest
	mkS3VersionID o (eitherToMaybe $ T.decodeUtf8' v)

-- For a versioned bucket, the S3VersionID is used as the
-- ContentIdentifier.
mkS3VersionedContentIdentifier :: S3VersionID -> ContentIdentifier
mkS3VersionedContentIdentifier (S3VersionID _ v) = 
	ContentIdentifier $ T.encodeUtf8 v

mkS3VersionedContentIdentifier' :: S3.ObjectVersionInfo -> ContentIdentifier
mkS3VersionedContentIdentifier' =
	ContentIdentifier . T.encodeUtf8 . S3.oviVersionId

-- S3 returns etags surrounded by double quotes, and the quotes may
-- be included here.
type S3Etag = T.Text

-- For an unversioned bucket, the S3Etag is instead used as the
-- ContentIdentifier. Prefixed by '#' since that cannot appear in a S3
-- version id.
mkS3UnversionedContentIdentifier :: S3Etag -> ContentIdentifier
mkS3UnversionedContentIdentifier t =
	ContentIdentifier $ T.encodeUtf8 $ "#" <> T.filter (/= '"') t

-- Makes a GetObject request be guaranteed to get the object version
-- matching the ContentIdentifier, or fail.
limitGetToContentIdentifier :: ContentIdentifier -> S3.GetObject -> S3.GetObject
limitGetToContentIdentifier cid req =
	limitToContentIdentifier cid
		(\etag -> req { S3.goIfMatch = etag })
		(\versionid -> req { S3.goVersionId = versionid })

limitHeadToContentIdentifier :: ContentIdentifier -> S3.HeadObject -> S3.HeadObject
limitHeadToContentIdentifier cid req =
	limitToContentIdentifier cid
		(\etag -> req { S3.hoIfMatch = etag })
		(\versionid -> req { S3.hoVersionId = versionid })

limitToContentIdentifier :: ContentIdentifier -> (Maybe S3Etag -> a) -> (Maybe T.Text -> a) -> a
limitToContentIdentifier (ContentIdentifier v) limitetag limitversionid =
	let t = either mempty id (T.decodeUtf8' v)
	in case T.take 1 t of
		"#" -> 
			let etag = T.drop 1 t
			in limitetag (Just etag)
		_ -> limitversionid (Just t)

-- A ContentIdentifier contains either a etag or a S3 version id.
extractContentIdentifier :: ContentIdentifier -> S3.Object -> Either S3Etag (Maybe S3VersionID)
extractContentIdentifier (ContentIdentifier v) o =
	let t = either mempty id (T.decodeUtf8' v)
	in case T.take 1 t of
		"#" -> Left (T.drop 1 t)
		_ -> Right (mkS3VersionID o (Just t))

setS3VersionID :: S3Info -> RemoteStateHandle -> Key -> Maybe S3VersionID -> Annex ()
setS3VersionID info rs k vid
	| versioning info = maybe noop (setS3VersionID' rs k) vid
	| otherwise = noop

setS3VersionID' :: RemoteStateHandle -> Key -> S3VersionID -> Annex ()
setS3VersionID' rs k vid = addRemoteMetaData k rs $
	updateMetaData s3VersionField v emptyMetaData
  where
	v = mkMetaValue (CurrentlySet True) (formatS3VersionID vid)

getS3VersionID :: RemoteStateHandle -> Key -> Annex [S3VersionID]
getS3VersionID rs k = do
	(RemoteMetaData _ m) <- getCurrentRemoteMetaData rs k
	return $ mapMaybe parseS3VersionID $ map unwrap $ S.toList $
		metaDataValues s3VersionField m
  where
	unwrap (MetaValue _ v) = v

s3VersionField :: MetaField
s3VersionField = mkMetaFieldUnchecked "V"

eitherS3VersionID :: S3Info -> RemoteStateHandle -> ParsedRemoteConfig -> Key -> S3.Object -> Annex (Either String (Either S3.Object S3VersionID))
eitherS3VersionID info rs c k fallback
	| versioning info = getS3VersionID rs k >>= return . \case
		[] -> if exportTree c
			then Left "Remote is configured to use versioning, but no S3 version ID is recorded for this key"
			else Right (Left fallback)
		-- It's possible for a key to be stored multiple times in
		-- a bucket with different version IDs; only use one of them.
		(v:_) -> Right (Right v)
	| otherwise = return (Right (Left fallback))

s3VersionIDPublicUrl :: (S3Info -> BucketObject -> URLString) -> S3Info -> S3VersionID -> URLString
s3VersionIDPublicUrl mk info (S3VersionID obj vid) = concat
	[ mk info (T.unpack obj)
	, "?versionId="
	, T.unpack vid -- version ID is "url ready" so no escaping needed
	]

getS3VersionIDPublicUrls :: (S3Info -> BucketObject -> URLString) -> S3Info -> RemoteStateHandle -> Key -> Annex [URLString]
getS3VersionIDPublicUrls mk info rs k =
	map (s3VersionIDPublicUrl mk info) <$> getS3VersionID rs k

-- Enable versioning on the bucket can only be done at init time;
-- setting versioning in a bucket that git-annex has already exported
-- files to risks losing the content of those un-versioned files.
enableBucketVersioning :: SetupStage -> S3Info -> ParsedRemoteConfig -> RemoteGitConfig -> UUID -> Annex ()
enableBucketVersioning ss info c gc u = do
	case ss of
		Init -> when (versioning info) $
			enableversioning (bucket info)
		Enable oldc -> checkunchanged oldc
		AutoEnable oldc -> checkunchanged oldc
  where
	enableversioning b = do
		showAction "checking bucket versioning"
		hdl <- mkS3HandleVar False c gc u
		let setversioning = S3.putBucketVersioning b S3.VersioningEnabled
		withS3HandleOrFail u hdl $ \h ->
#if MIN_VERSION_aws(0,24,3)
			liftIO $ runResourceT $
				tryS3 (sendS3Handle h setversioning) >>= \case
					Right _ -> return ()
					Left err -> do
						res <- sendS3Handle h $
							S3.getBucketVersioning b
						case S3.gbvVersioning res of
							Just S3.VersioningEnabled -> return ()
							_ -> giveup $ "This bucket does not have versioning enabled, and enabling it failed: "
								++ T.unpack (S3.s3ErrorMessage err)
#else
			void $ liftIO $ runResourceT $ sendS3Handle h setversioning
#endif

	checkunchanged oldc = do
		oldpc <- parsedRemoteConfig remote oldc
		oldinfo <- extractS3Info oldpc
		when (versioning info /= versioning oldinfo) $
			giveup "Cannot change versioning= of existing S3 remote."

-- If the remote has versioning enabled, but the version ID is for some
-- reason not being recorded, it's not safe to perform an action that
-- will remove the unversioned file. The file may be the only copy of an
-- annex object.
--
-- This code could be removed eventually, since enableBucketVersioning
-- will avoid this situation. Before that was added, some remotes
-- were created without versioning, some unversioned files exported to
-- them, and then versioning enabled, and this is to avoid data loss in
-- those cases.
checkVersioning :: S3Info -> RemoteStateHandle -> Key -> Annex ()
checkVersioning info rs k
	| versioning info = getS3VersionID rs k >>= \case
		[] -> giveup "Remote is configured to use versioning, but no S3 version ID is recorded for this key, so it cannot safely be modified."
		_ -> return ()
	| otherwise = return ()
