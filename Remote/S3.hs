{- S3 remotes
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import qualified Data.Set as S
import qualified System.FilePath.Posix as Posix
import Data.Char
import Data.String
import Network.Socket (HostName)
import Network.HTTP.Conduit (Manager)
import Network.HTTP.Client (responseStatus, responseBody, RequestBody(..))
import Network.HTTP.Types
import Network.URI
import Control.Monad.Trans.Resource
import Control.Monad.Catch
import Data.IORef
import System.Log.Logger
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Data.Maybe

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
import Remote.Helper.Messages
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
import Utility.Url (extractFromResourceT)
import Annex.Url (getUrlOptions, withUrlOptions, UrlOptions(..))
import Utility.Env

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
			, optionalStringParser partsizeField
				(FieldDesc "part size for multipart upload (eg 1GiB)")
			, optionalStringParser storageclassField
				(FieldDesc "storage class, eg STANDARD or STANDARD_IA or ONEZONE_IA")
			, optionalStringParser fileprefixField
				(FieldDesc "prefix to add to filenames in the bucket")
			, yesNoParser versioningField (Just False)
				(FieldDesc "enable versioning of bucket content")
			, yesNoParser publicField (Just False)
				(FieldDesc "allow public read access to the bucket")
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

partsizeField :: RemoteConfigField
partsizeField = Accepted "partsize"

storageclassField :: RemoteConfigField
storageclassField = Accepted "storageclass"

fileprefixField :: RemoteConfigField
fileprefixField = Accepted "fileprefix"

versioningField :: RemoteConfigField
versioningField = Accepted "versioning"

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

newtype SignatureVersion = SignatureVersion Int

signatureVersionParser :: RemoteConfigField -> FieldDesc -> RemoteConfigFieldParser
signatureVersionParser f fd =
	genParser go f (Just defver) fd
		(Just (ValueDesc "v2 or v4"))
  where
	go "v2" = Just (SignatureVersion 2)
	go "v4" = Just (SignatureVersion 4)
	go _ = Nothing

	defver = SignatureVersion 2

portField :: RemoteConfigField
portField = Accepted "port"

mungekeysField :: RemoteConfigField
mungekeysField = Accepted "mungekeys"

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = do
	c <- parsedRemoteConfig remote rc
	cst <- remoteCost gc expensiveRemoteCost
	info <- extractS3Info c
	hdl <- mkS3HandleVar c gc u
	magic <- liftIO initMagicMime
	return $ new c cst info hdl magic
  where
	new c cst info hdl magic = Just $ specialRemote c
		(store hdl this info magic)
		(retrieve hdl this rs c info)
		(remove hdl this info)
		(checkKey hdl this rs c info)
		this
	  where
		this = Remote
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = storeKeyDummy
			, retrieveKeyFile = retrieveKeyFileDummy
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
				, renameExport = renameExportS3 hdl this rs info
				}
			, importActions = ImportActions
                                { listImportableContents = listImportableContentsS3 hdl this info
				, importKey = Nothing
                                , retrieveExportWithContentIdentifier = retrieveExportWithContentIdentifierS3 hdl this rs info
                                , storeExportWithContentIdentifier = storeExportWithContentIdentifierS3 hdl this rs info magic
                                , removeExportWithContentIdentifier = removeExportWithContentIdentifierS3 hdl this rs info
                                , removeExportDirectoryWhenEmpty = Nothing
                                , checkPresentExportWithContentIdentifier = checkPresentExportWithContentIdentifierS3 hdl this info
                                }
			, whereisKey = Just (getPublicWebUrls u rs info c)
			, remoteFsck = Nothing
			, repairRepo = Nothing
			, config = c
			, getRepo = return r
			, gitconfig = gc
			, localpath = Nothing
			, readonly = False
			, appendonly = versioning info
			, availability = GloballyAvailable
			, remotetype = remote
			, mkUnavailable = gen r u (M.insert hostField (Proposed "!dne!") rc) gc rs
			, getInfo = includeCredsInfo c (AWS.creds u) (s3Info c info)
			, claimUrl = Nothing
			, checkUrl = Nothing
			, remoteStateHandle = rs
			}

s3Setup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
s3Setup ss mu mcreds c gc = do
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
		(c', encsetup) <- encryptionSetup (c `M.union` defaults) gc
		pc <- either giveup return . parseRemoteConfig c'
			=<< configParser remote c'
		c'' <- setRemoteCredPair encsetup pc gc (AWS.creds u) mcreds
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
		c' <- setRemoteCredPair noEncryptionUsed pc gc (AWS.creds u) mcreds
		-- Ensure user enters a valid bucket name, since
		-- this determines the name of the archive.org item.
		let validbucket = replace " " "-" $ map toLower $
			maybe (giveup "specify bucket=") fromProposedAccepted
				(M.lookup bucketField c')
		let archiveconfig = 
			-- IA acdepts x-amz-* as an alias for x-archive-*
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
		hdl <- mkS3HandleVar pc' gc u
		withS3HandleOrFail u hdl $
			writeUUIDFile pc' u info
		use archiveconfig pc' info
	
	checkexportimportsafe c' info =
		unlessM (Annex.getState Annex.force) $
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

storeHelper :: S3Info -> S3Handle -> Maybe Magic -> FilePath -> S3.Object -> MeterUpdate -> Annex (Maybe S3Etag, Maybe S3VersionID)
storeHelper info h magic f object p = liftIO $ case partSize info of
	Just partsz | partsz > 0 -> do
		fsz <- getFileSize (toRawFilePath f)
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
#if MIN_VERSION_aws(0,22,0)
		etag <- extractFromResourceT (Just (S3.porETag resp))
		return (etag, vid)
#else
		return (Nothing, vid)
#endif
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
		etags <- bracketIO (openBinaryFile f ReadMode) hClose $ \fh -> do
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
retrieve :: S3HandleVar -> Remote -> RemoteStateHandle -> ParsedRemoteConfig -> S3Info -> Retriever
retrieve hv r rs c info = fileRetriever $ \f k p -> withS3Handle hv $ \case
	(Just h) -> 
		eitherS3VersionID info rs c k (T.pack $ bucketObject info k) >>= \case
			Left failreason -> do
				warning failreason
				giveup "cannot download content"
			Right loc -> retrieveHelper info h loc f p
	Nothing ->
		getPublicWebUrls' (uuid r) rs info c k >>= \case
			Left failreason -> do
				warning failreason
				giveup "cannot download content"
			Right us -> unlessM (withUrlOptions $ downloadUrl k p us f) $
				giveup "failed to download content"

retrieveHelper :: S3Info -> S3Handle -> (Either S3.Object S3VersionID) -> FilePath -> MeterUpdate -> Annex ()
retrieveHelper info h loc f p = retrieveHelper' h f p $
	case loc of
		Left o -> S3.getObject (bucket info) o
		Right (S3VersionID o vid) -> (S3.getObject (bucket info) o)
			{ S3.goVersionId = Just vid }

retrieveHelper' :: S3Handle -> FilePath -> MeterUpdate -> S3.GetObject -> Annex ()
retrieveHelper' h f p req = liftIO $ runResourceT $ do
	S3.GetObjectResponse { S3.gorResponse = rsp } <- sendS3Handle h req
	Url.sinkResponseFile p zeroBytesProcessed f WriteMode rsp

remove :: S3HandleVar -> Remote -> S3Info -> Remover
remove hv r info k = withS3HandleOrFail (uuid r) hv $ \h -> do
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
		ifM (checkKey hv r rs c info k)
			( withVerifiedCopy LockedCopy (uuid r) (return True) callback
			, giveup $ "content seems to be missing from " ++ name r ++ " despite S3 versioning being enabled"
			)
	| otherwise = Nothing

checkKey :: S3HandleVar -> Remote -> RemoteStateHandle -> ParsedRemoteConfig -> S3Info -> CheckPresent
checkKey hv r rs c info k = withS3Handle hv $ \case
	Just h -> do
		showChecking r
		eitherS3VersionID info rs c k (T.pack $ bucketObject info k) >>= \case
			Left failreason -> do
				warning failreason
				giveup "cannot check content"
			Right loc -> checkKeyHelper info h loc
	Nothing ->
		getPublicWebUrls' (uuid r) rs info c k >>= \case
			Left failreason -> do
				warning failreason
				giveup "cannot check content"
			Right us -> do
				showChecking r
				let check u = withUrlOptions $ 
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

storeExportS3 :: S3HandleVar -> Remote -> RemoteStateHandle -> S3Info -> Maybe Magic -> FilePath -> Key -> ExportLocation -> MeterUpdate -> Annex ()
storeExportS3 hv r rs info magic f k loc p = void $ storeExportS3' hv r rs info magic f k loc p

storeExportS3' :: S3HandleVar -> Remote -> RemoteStateHandle -> S3Info -> Maybe Magic -> FilePath -> Key -> ExportLocation -> MeterUpdate -> Annex (Maybe S3Etag, Maybe S3VersionID)
storeExportS3' hv r rs info magic f k loc p = withS3Handle hv $ \case
	Just h -> go h
	Nothing -> giveup $ needS3Creds (uuid r)
  where
	go h = do
		let o = T.pack $ bucketExportLocation info loc
		(metag, mvid) <- storeHelper info h magic f o p
		setS3VersionID info rs k mvid
		return (metag, mvid)

retrieveExportS3 :: S3HandleVar -> Remote -> S3Info -> Key -> ExportLocation -> FilePath -> MeterUpdate -> Annex ()
retrieveExportS3 hv r info _k loc f p = do
	withS3Handle hv $ \case
		Just h -> retrieveHelper info h (Left (T.pack exportloc)) f p
		Nothing -> case getPublicUrlMaker info of
			Just geturl -> either giveup return =<<
				Url.withUrlOptions
					(Url.download' p (geturl exportloc) f)
			Nothing -> giveup $ needS3Creds (uuid r)
  where
	exportloc = bucketExportLocation info loc

removeExportS3 :: S3HandleVar -> Remote -> RemoteStateHandle -> S3Info -> Key -> ExportLocation -> Annex ()
removeExportS3 hv r rs info k loc = withS3Handle hv $ \case
	Just h -> do
		checkVersioning info rs k
		liftIO $ runResourceT $ do
			S3.DeleteObjectResponse <- sendS3Handle h $
				S3.DeleteObject (T.pack $ bucketExportLocation info loc) (bucket info)
			return ()
	Nothing -> giveup $ needS3Creds (uuid r)
  where

checkPresentExportS3 :: S3HandleVar -> Remote -> S3Info -> Key -> ExportLocation -> Annex Bool
checkPresentExportS3 hv r info k loc = withS3Handle hv $ \case
	Just h -> checkKeyHelper info h (Left (T.pack $ bucketExportLocation info loc))
	Nothing -> case getPublicUrlMaker info of
		Just geturl -> withUrlOptions $
			Url.checkBoth (geturl $ bucketExportLocation info loc) (fromKey keySize k)
		Nothing -> do
			warning $ needS3Creds (uuid r)
			giveup "No S3 credentials configured"

-- S3 has no move primitive; copy and delete.
renameExportS3 :: S3HandleVar -> Remote -> RemoteStateHandle -> S3Info -> Key -> ExportLocation -> ExportLocation -> Annex (Maybe ())
renameExportS3 hv r rs info k src dest = Just <$> go
  where
	go = withS3Handle hv $ \case
		Just h -> do
			checkVersioning info rs k
			go' h
		Nothing -> giveup $ needS3Creds (uuid r)
	
	go' h = liftIO $ runResourceT $ do
		let co = S3.copyObject (bucket info) dstobject
			(S3.ObjectId (bucket info) srcobject Nothing)
			S3.CopyMetadata
		-- ACL is not preserved by copy.
		void $ sendS3Handle h $ co { S3.coAcl = acl info }
		void $ sendS3Handle h $ S3.DeleteObject srcobject (bucket info)
	
	srcobject = T.pack $ bucketExportLocation info src
	dstobject = T.pack $ bucketExportLocation info dest

listImportableContentsS3 :: S3HandleVar -> Remote -> S3Info -> Annex (Maybe (ImportableContents (ContentIdentifier, ByteSize)))
listImportableContentsS3 hv r info =
	withS3Handle hv $ \case
		Nothing -> giveup $ needS3Creds (uuid r)
		Just h -> Just <$> go h
  where
	go h = liftIO $ runResourceT $ extractFromResourceT =<< startlist h

	startlist h
		| versioning info = do
			rsp <- sendS3Handle h $ 
				S3.getBucketObjectVersions (bucket info)
			continuelistversioned h [] rsp
		| otherwise = do
			rsp <- sendS3Handle h $ 
				S3.getBucket (bucket info)
			continuelistunversioned h [] rsp

	continuelistunversioned h l rsp
		| S3.gbrIsTruncated rsp = do
			rsp' <- sendS3Handle h $
				(S3.getBucket (bucket info))
					{ S3.gbMarker = S3.gbrNextMarker rsp
					}
			continuelistunversioned h (rsp:l) rsp'
		| otherwise = return $
			mkImportableContentsUnversioned info (reverse (rsp:l))
	
	continuelistversioned h l rsp
		| S3.gbovrIsTruncated rsp = do
			rsp' <- sendS3Handle h $
				(S3.getBucketObjectVersions (bucket info))
					{ S3.gbovKeyMarker = S3.gbovrNextKeyMarker rsp
					, S3.gbovVersionIdMarker = S3.gbovrNextVersionIdMarker rsp
					}
			continuelistversioned h (rsp:l) rsp'
		| otherwise = return $
			mkImportableContentsVersioned info (reverse (rsp:l))

mkImportableContentsUnversioned :: S3Info -> [S3.GetBucketResponse] -> ImportableContents (ContentIdentifier, ByteSize)
mkImportableContentsUnversioned info l = ImportableContents 
	{ importableContents = concatMap (mapMaybe extract . S3.gbrContents) l
	, importableHistory = []
	}
  where
	extract oi = do
		loc <- bucketImportLocation info $
			T.unpack $ S3.objectKey oi
		let sz  = S3.objectSize oi
		let cid = mkS3UnversionedContentIdentifier $ S3.objectETag oi
		return (loc, (cid, sz))

mkImportableContentsVersioned :: S3Info -> [S3.GetBucketObjectVersionsResponse] -> ImportableContents (ContentIdentifier, ByteSize)
mkImportableContentsVersioned info = build . groupfiles
  where
	build [] = ImportableContents [] []
	build l =
		let (l', v) = latestversion l
		in ImportableContents
			{ importableContents = mapMaybe extract v
			, importableHistory = case build l' of
				ImportableContents [] [] -> []
				h -> [h]
			}
  	
	extract ovi@(S3.ObjectVersion {}) = do
		loc <- bucketImportLocation info $
			T.unpack $ S3.oviKey ovi
		let sz  = S3.oviSize ovi
		let cid = mkS3VersionedContentIdentifier' ovi
		return (loc, (cid, sz))
	extract (S3.DeleteMarker {}) = Nothing
	
	-- group files so all versions of a file are in a sublist,
	-- with the newest first. S3 uses such an order, so it's just a
	-- matter of breaking up the response list into sublists.
	groupfiles = groupBy (\a b -> S3.oviKey a == S3.oviKey b) 
		. concatMap S3.gbovrContents

	latestversion [] = ([], [])
	latestversion ([]:rest) = latestversion rest
	latestversion l@((first:_old):remainder) =
		go (S3.oviLastModified first) [first] remainder
	  where
		go mtime c [] = (removemostrecent mtime l, reverse c)
		go mtime c ([]:rest) = go mtime c rest
		go mtime c ((latest:_old):rest) = 
			let !mtime' = max mtime (S3.oviLastModified latest)
			in go mtime' (latest:c) rest
	
	removemostrecent _ [] = []
	removemostrecent mtime ([]:rest) = removemostrecent mtime rest
	removemostrecent mtime (i@(curr:old):rest)
		| S3.oviLastModified curr == mtime =
			old : removemostrecent mtime rest
		| otherwise =
			i : removemostrecent mtime rest

retrieveExportWithContentIdentifierS3 :: S3HandleVar -> Remote -> RemoteStateHandle -> S3Info -> ExportLocation -> ContentIdentifier -> FilePath -> Annex Key -> MeterUpdate -> Annex Key
retrieveExportWithContentIdentifierS3 hv r rs info loc cid dest mkkey p = withS3Handle hv $ \case
	Just h -> do
		rewritePreconditionException $ retrieveHelper' h dest p $
			limitGetToContentIdentifier cid $
				S3.getObject (bucket info) o
		k <- mkkey
		case extractContentIdentifier cid o of
			Right vid -> do
				vids <- getS3VersionID rs k
				unless (vid `elem` map Just vids) $
					setS3VersionID info rs k vid
			Left _ -> noop
		return k
	Nothing -> giveup $ needS3Creds (uuid r)
  where
	o = T.pack $ bucketExportLocation info loc

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
storeExportWithContentIdentifierS3 :: S3HandleVar -> Remote -> RemoteStateHandle -> S3Info -> Maybe Magic -> FilePath -> Key -> ExportLocation -> [ContentIdentifier] -> MeterUpdate -> Annex ContentIdentifier
storeExportWithContentIdentifierS3 hv r rs info magic src k loc _overwritablecids p
	| versioning info = go
#if MIN_VERSION_aws(0,22,0)
	| otherwise = go
#else
	| otherwise = giveup "git-annex is built with too old a version of the aws library to support this operation"
#endif
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
	withS3Handle hv $ \case
		Just h -> flip anyM knowncids $
			checkKeyHelper' info h o . limitHeadToContentIdentifier
		Nothing -> do
			warning $ needS3Creds (uuid r)
			giveup "No S3 credentials configured"
  where
	o = T.pack $ bucketExportLocation info loc

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
	hdl <- mkS3HandleVar c gc u
	withS3HandleOrFail u hdl $ \h ->
		go info h =<< checkUUIDFile c u info h
  where
	go _ _ (Right True) = noop
	go info h _ = do
		r <- liftIO $ tryNonAsync $ runResourceT $ do
			void $ sendS3Handle h (S3.getBucket $ bucket info)
			return True
		case r of
			Right True -> noop
			_ -> do
				showAction $ "creating bucket in " ++ datacenter
				void $ liftIO $ runResourceT $ sendS3Handle h $ 
					(S3.putBucket (bucket info))
						{ S3.pbCannedAcl = acl info
						, S3.pbLocationConstraint = locconstraint
						, S3.pbXStorageClass = storageclass
						}
		writeUUIDFile c u info h
	
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
 -}
writeUUIDFile :: ParsedRemoteConfig -> UUID -> S3Info -> S3Handle -> Annex ()
writeUUIDFile c u info h = do
	v <- checkUUIDFile c u info h
	case v of
		Right True -> noop
		Right False -> do
			warning "The bucket already exists, and its annex-uuid file indicates it is used by a different special remote."
			giveup "Cannot reuse this bucket."
		_ -> void $ liftIO $ runResourceT $ sendS3Handle h mkobject
  where
	file = T.pack $ uuidFile c
	uuidb = L.fromChunks [T.encodeUtf8 $ T.pack $ fromUUID u]

	mkobject = putObject info file (RequestBodyLBS uuidb)

{- Checks if the UUID file exists in the bucket
 - and has the specified UUID already. -}
checkUUIDFile :: ParsedRemoteConfig -> UUID -> S3Info -> S3Handle -> Annex (Either SomeException Bool)
checkUUIDFile c u info h = tryNonAsync $ liftIO $ runResourceT $ do
	resp <- tryS3 $ sendS3Handle h (S3.getObject (bucket info) file)
	case resp of
		Left _ -> return False
		Right r -> do
			v <- AWS.loadToMemory r
			extractFromResourceT (check v)
  where
	check (S3.GetObjectMemoryResponse _meta rsp) =
		responseStatus rsp == ok200 && responseBody rsp == uuidb

	file = T.pack $ uuidFile c
	uuidb = L.fromChunks [T.encodeUtf8 $ T.pack $ fromUUID u]

uuidFile :: ParsedRemoteConfig -> FilePath
uuidFile c = getFilePrefix c ++ "annex-uuid"

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

type S3HandleVar = TVar (Either (Annex (Maybe S3Handle)) (Maybe S3Handle))

{- Prepares a S3Handle for later use. Does not connect to S3 or do anything
 - else expensive. -}
mkS3HandleVar :: ParsedRemoteConfig -> RemoteGitConfig -> UUID -> Annex S3HandleVar
mkS3HandleVar c gc u = liftIO $ newTVarIO $ Left $ do
	mcreds <- getRemoteCredPair c gc (AWS.creds u)
	case mcreds of
		Just creds -> do
			awscreds <- liftIO $ genCredentials creds
			let awscfg = AWS.Configuration AWS.Timestamp awscreds debugMapper Nothing
			ou <- getUrlOptions
			return $ Just $ S3Handle (httpManager ou) awscfg s3cfg
		Nothing -> return Nothing
  where
	s3cfg = s3Configuration c

withS3Handle :: S3HandleVar -> (Maybe S3Handle -> Annex a) -> Annex a
withS3Handle hv a = liftIO (readTVarIO hv) >>= \case
	Right hdl -> a hdl
	Left mkhdl -> do
		hdl <- mkhdl
		liftIO $ atomically $ writeTVar hv (Right hdl)
		a hdl

withS3HandleOrFail :: UUID -> S3HandleVar -> (S3Handle -> Annex a) -> Annex a
withS3HandleOrFail u hv a = withS3Handle hv $ \case
	Just hdl -> a hdl
	Nothing -> do
		warning $ needS3Creds u
		giveup "No S3 credentials configured"

needS3Creds :: UUID -> String
needS3Creds u = missingCredPairFor "S3" (AWS.creds u)

s3Configuration :: ParsedRemoteConfig -> S3.S3Configuration AWS.NormalQuery
s3Configuration c = cfg
	{ S3.s3Port = port
	, S3.s3RequestStyle = case getRemoteConfigValue requeststyleField c of
		Just "path" -> S3.PathStyle
		Just s -> giveup $ "bad S3 requeststyle value: " ++ s
		Nothing -> S3.s3RequestStyle cfg
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
	cfg = case getRemoteConfigValue signatureField c of
		Just (SignatureVersion 4) -> 
			S3.s3v4 proto endpoint False S3.SignWithEffort
		_ -> S3.s3 proto endpoint False

data S3Info = S3Info
	{ bucket :: S3.Bucket
	, storageClass :: S3.StorageClass
	, bucketObject :: Key -> BucketObject
	, bucketExportLocation :: ExportLocation -> BucketObject
	, bucketImportLocation :: BucketObject -> Maybe ImportLocation
	, metaHeaders :: [(T.Text, T.Text)]
	, partSize :: Maybe Integer
	, isIA :: Bool
	, versioning :: Bool
	, public :: Bool
	, publicurl :: Maybe URLString
	, host :: Maybe String
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
		, partSize = getPartSize c
		, isIA = configIA c
		, versioning = fromMaybe False $
			getRemoteConfigValue versioningField c
		, public = fromMaybe False $
			getRemoteConfigValue publicField c
		, publicurl = getRemoteConfigValue publicurlField c
		, host = getRemoteConfigValue hostField c
		}

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
	getFilePrefix c ++ fromRawFilePath (fromExportLocation loc)

getBucketImportLocation :: ParsedRemoteConfig -> BucketObject -> Maybe ImportLocation
getBucketImportLocation c obj
	-- The uuidFile should not be imported.
	| obj == uuidfile = Nothing
	-- Only import files that are under the fileprefix, when
	-- one is configured.
	| prefix `isPrefixOf` obj = Just $ mkImportLocation $
		toRawFilePath $ drop prefixlen obj
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
genCredentials (keyid, secret) = AWS.Credentials
	<$> pure (tobs keyid)
	<*> pure (tobs secret)
	<*> newIORef []
	<*> (fmap tobs <$> getEnv "AWS_SESSION_TOKEN")
  where
	tobs = T.encodeUtf8 . T.pack

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

s3Info :: ParsedRemoteConfig -> S3Info -> [(String, String)]
s3Info c info = catMaybes
	[ Just ("bucket", fromMaybe "unknown" (getBucketName c))
	, Just ("endpoint", w82s (BS.unpack (S3.s3Endpoint s3c)))
	, Just ("port", show (S3.s3Port s3c))
	, Just ("protocol", map toLower (show (S3.s3Protocol s3c)))
	, Just ("storage class", showstorageclass (getStorageClass c))
	, if configIA c
		then Just ("internet archive item", iaItemUrl $ fromMaybe "unknown" $ getBucketName c)
		else Nothing
	, Just ("partsize", maybe "unlimited" (roughSize storageUnits False) (getPartSize c))
	, Just ("public", if public info then "yes" else "no")
	, Just ("versioning", if versioning info then "yes" else "no")
	]
  where
	s3c = s3Configuration c
	showstorageclass (S3.OtherStorageClass t) = T.unpack t
	showstorageclass sc = show sc

getPublicWebUrls :: UUID -> RemoteStateHandle -> S3Info -> ParsedRemoteConfig -> Key -> Annex [URLString]
getPublicWebUrls u rs info c k = either (const []) id <$> getPublicWebUrls' u rs info c k

getPublicWebUrls' :: UUID -> RemoteStateHandle -> S3Info -> ParsedRemoteConfig -> Key -> Annex (Either String [URLString])
getPublicWebUrls' u rs info c k
	| not (public info) = return $ Left $ 
		"S3 bucket does not allow public access; " ++ needS3Creds u
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
		-- It's possible for a key to be stored multiple timees in
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
#if MIN_VERSION_aws(0,21,1)
enableBucketVersioning ss info c gc u = do
#else
enableBucketVersioning ss info _ _ _ = do
#endif
	case ss of
		Init -> when (versioning info) $
			enableversioning (bucket info)
		Enable oldc -> do
			oldpc <- parsedRemoteConfig remote oldc
			oldinfo <- extractS3Info oldpc
			when (versioning info /= versioning oldinfo) $
				giveup "Cannot change versioning= of existing S3 remote."
  where
	enableversioning b = do
#if MIN_VERSION_aws(0,21,1)
		showAction "enabling bucket versioning"
		hdl <- mkS3HandleVar c gc u
		withS3HandleOrFail u hdl $ \h ->
			void $ liftIO $ runResourceT $ sendS3Handle h $
				S3.putBucketVersioning b S3.VersioningEnabled
#else
		showLongNote $ unlines
			[ "This version of git-annex cannot auto-enable S3 bucket versioning."
			, "You need to manually enable versioning in the S3 console"
			, "for the bucket \"" ++ T.unpack b ++ "\""
			, "https://docs.aws.amazon.com/AmazonS3/latest/user-guide/enable-versioning.html"
			, "It's important you enable versioning before storing anything in the bucket!"
			]
#endif

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
