{- P2P protocol over HTTP
 -
 - https://git-annex.branchable.com/design/p2p_protocol_over_http/
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module P2P.Http (
	module P2P.Http,
	module P2P.Http.Types,
	module P2P.Http.State,
) where

import Annex.Common
import P2P.Http.Types
import P2P.Http.State
import P2P.Protocol hiding (Offset, Bypass, auth)
import P2P.IO
import P2P.Annex
import Annex.WorkerPool
import Annex.Concurrent
import Types.WorkerPool
import Types.Direction
import Utility.Metered

import Servant
import Servant.Client.Streaming
import qualified Servant.Types.SourceT as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI
import Data.Char
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent
import System.IO.Unsafe

type P2PHttpAPI
	=    "git-annex" :> SU :> PV3 :> "key" :> GetAPI
	:<|> "git-annex" :> SU :> PV2 :> "key" :> GetAPI
	:<|> "git-annex" :> SU :> PV1 :> "key" :> GetAPI
	:<|> "git-annex" :> SU :> PV0 :> "key" :> GetAPI
	:<|> "git-annex" :> SU :> PV3 :> "checkpresent" :> CheckPresentAPI
	:<|> "git-annex" :> SU :> PV2 :> "checkpresent" :> CheckPresentAPI
	:<|> "git-annex" :> SU :> PV1 :> "checkpresent" :> CheckPresentAPI
	:<|> "git-annex" :> SU :> PV0 :> "checkpresent" :> CheckPresentAPI
	:<|> "git-annex" :> SU :> PV3 :> "remove" :> RemoveAPI RemoveResultPlus
	:<|> "git-annex" :> SU :> PV2 :> "remove" :> RemoveAPI RemoveResultPlus
	:<|> "git-annex" :> SU :> PV1 :> "remove" :> RemoveAPI RemoveResult
	:<|> "git-annex" :> SU :> PV0 :> "remove" :> RemoveAPI RemoveResult
	:<|> "git-annex" :> SU :> PV3 :> "remove-before" :> RemoveBeforeAPI
	:<|> "git-annex" :> SU :> PV3 :> "gettimestamp" :> GetTimestampAPI
	:<|> "git-annex" :> SU :> PV3 :> "put" :> PutAPI PutResultPlus
	:<|> "git-annex" :> SU :> PV2 :> "put" :> PutAPI PutResultPlus
	:<|> "git-annex" :> SU :> PV1 :> "put" :> PutAPI PutResult
	:<|> "git-annex" :> SU :> PV0 :> "put" :> PutAPI PutResult
	:<|> "git-annex" :> SU :> PV3 :> "putoffset"
		:> PutOffsetAPI PutOffsetResultPlus
	:<|> "git-annex" :> SU :> PV2 :> "putoffset"
		:> PutOffsetAPI PutOffsetResultPlus
	:<|> "git-annex" :> SU :> PV1 :> "putoffset"
		:> PutOffsetAPI PutOffsetResult
	:<|> "git-annex" :> SU :> PV3 :> "lockcontent" :> LockContentAPI
	:<|> "git-annex" :> SU :> PV2 :> "lockcontent" :> LockContentAPI
	:<|> "git-annex" :> SU :> PV1 :> "lockcontent" :> LockContentAPI
	:<|> "git-annex" :> SU :> PV0 :> "lockcontent" :> LockContentAPI
	:<|> "git-annex" :> SU :> PV3 :> "keeplocked" :> KeepLockedAPI
	:<|> "git-annex" :> SU :> PV2 :> "keeplocked" :> KeepLockedAPI
	:<|> "git-annex" :> SU :> PV1 :> "keeplocked" :> KeepLockedAPI
	:<|> "git-annex" :> SU :> PV0 :> "keeplocked" :> KeepLockedAPI
	:<|> "git-annex" :> SU :> "key" :> GetGenericAPI

p2pHttpAPI :: Proxy P2PHttpAPI
p2pHttpAPI = Proxy

p2pHttpApp :: P2PHttpServerState -> Application
p2pHttpApp = serve p2pHttpAPI . serveP2pHttp

serveP2pHttp :: P2PHttpServerState -> Server P2PHttpAPI
serveP2pHttp st
	=    serveGet st
	:<|> serveGet st
	:<|> serveGet st
	:<|> serveGet st
	:<|> serveCheckPresent st
	:<|> serveCheckPresent st
	:<|> serveCheckPresent st
	:<|> serveCheckPresent st
	:<|> serveRemove st id
	:<|> serveRemove st id
	:<|> serveRemove st dePlus
	:<|> serveRemove st dePlus
	:<|> serveRemoveBefore st
	:<|> serveGetTimestamp st
	:<|> servePut st id
	:<|> servePut st id
	:<|> servePut st dePlus
	:<|> servePut st dePlus
	:<|> servePutOffset st id
	:<|> servePutOffset st id
	:<|> servePutOffset st dePlus
	:<|> serveLockContent st
	:<|> serveLockContent st
	:<|> serveLockContent st
	:<|> serveLockContent st
	:<|> serveKeepLocked st
	:<|> serveKeepLocked st
	:<|> serveKeepLocked st
	:<|> serveKeepLocked st
	:<|> serveGetGeneric st

type GetGenericAPI
	= CaptureKey
	:> IsSecure
	:> AuthHeader
	:> StreamGet NoFraming OctetStream
		(Headers '[DataLengthHeader] (SourceIO B.ByteString))

serveGetGeneric
	:: P2PHttpServerState
	-> B64UUID ServerSide
	-> B64Key
	-> IsSecure
	-> Maybe Auth
	-> Handler (Headers '[DataLengthHeader] (S.SourceT IO B.ByteString))
serveGetGeneric st su@(B64UUID u) k =
	-- Use V0 because it does not alter the returned data to indicate
	-- Invalid content.
	serveGet st su V0 k cu [] Nothing Nothing
  where
	-- Reuse server UUID as client UUID.
	cu = B64UUID u :: B64UUID ClientSide

type GetAPI
	= CaptureKey
	:> CU Required
	:> BypassUUIDs
	:> AssociatedFileParam
	:> OffsetParam
	:> IsSecure
	:> AuthHeader
	:> StreamGet NoFraming OctetStream
		(Headers '[DataLengthHeader] (SourceIO B.ByteString))

serveGet
	:: APIVersion v
	=> P2PHttpServerState
	-> B64UUID ServerSide
	-> v
	-> B64Key
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Maybe B64FilePath
	-> Maybe Offset
	-> IsSecure
	-> Maybe Auth
	-> Handler (Headers '[DataLengthHeader] (S.SourceT IO B.ByteString))
serveGet st su apiver (B64Key k) cu bypass baf startat sec auth = do
	conn <- getP2PConnection apiver st cu su bypass sec auth ReadAction id
	bsv <- liftIO newEmptyTMVarIO
	endv <- liftIO newEmptyTMVarIO
	validityv <- liftIO newEmptyTMVarIO
	finalv <- liftIO newEmptyTMVarIO
	annexworker <- liftIO $ async $ inAnnexWorker st $ do
		let storer _offset len = sendContentWith $ \bs -> do
			liftIO $ atomically $ putTMVar bsv (len, bs)
			liftIO $ atomically $ takeTMVar endv
			liftIO $ signalFullyConsumedByteString $
				connOhdl $ serverP2PConnection conn
			return $ \v -> do
				liftIO $ atomically $ putTMVar validityv v
				return True
		enteringStage (TransferStage Upload) $
			runFullProto (clientRunState conn) (clientP2PConnection conn) $
				void $ receiveContent Nothing nullMeterUpdate
					sizer storer getreq
	void $ liftIO $ forkIO $ waitfinal endv finalv conn annexworker
	(Len len, bs) <- liftIO $ atomically $ takeTMVar bsv
	bv <- liftIO $ newMVar (filter (not . B.null) (L.toChunks bs))
	szv <- liftIO $ newMVar 0
	let streamer = S.SourceT $ \s -> s =<< return 
		(stream (bv, szv, len, endv, validityv, finalv))
 	return $ addHeader (DataLength len) streamer
  where
	stream (bv, szv, len, endv, validityv, finalv) =
		S.fromActionStep B.null $
			modifyMVar bv $ nextchunk szv $
				checkvalidity szv len endv validityv finalv
	
	nextchunk szv checkvalid (b:[]) = do
		updateszv szv b
		ifM checkvalid
			( return ([], b)
			-- The key's content is invalid, but
			-- the amount of data is the same as the
			-- DataLengthHeader indicated. Truncate
			-- the response by one byte to indicate
			-- to the client that it's not valid.
			, return ([], B.take (B.length b - 1) b)
			)
	nextchunk szv checkvalid (b:bs) = do
		updateszv szv b
		return (bs, b)
	nextchunk _szv checkvalid [] = do
		void checkvalid
		-- Result ignored because 0 bytes of data are sent,
		-- so even if the key is invalid, if that's the
		-- amount of data that the DataLengthHeader indicates,
		-- we've successfully served an empty key.
		return ([], mempty)
	
	updateszv szv b = modifyMVar szv $ \sz ->
		let !sz' = sz + fromIntegral (B.length b)
		in return (sz', ())

	-- Returns False when the key's content is invalid, but the
	-- amount of data sent was the same as indicated by the
	-- DataLengthHeader.
	checkvalidity szv len endv validityv finalv =
		ifM (atomically $ isEmptyTMVar endv)
			( do
				atomically $ putTMVar endv ()
				validity <- atomically $ takeTMVar validityv
				sz <- takeMVar szv
				atomically $ putTMVar finalv ()
				return $ case validity of
					Nothing -> True
					Just Valid -> True
					Just Invalid -> sz /= len
			, pure True
			)
	
	waitfinal endv finalv conn annexworker = do
		-- Wait for everything to be transferred before
		-- stopping the annexworker. The validityv will usually
		-- be written to at the end. If the client disconnects
		-- early that does not happen, so catch STM exception.
		liftIO $ void $ tryNonAsync $ atomically $ takeTMVar finalv
		-- Make sure the annexworker is not left blocked on endv
		-- if the client disconnected early.
		void $ liftIO $ atomically $ tryPutTMVar endv ()
		void $ tryNonAsync $ wait annexworker
		void $ tryNonAsync $ releaseP2PConnection conn
	
	sizer = pure $ Len $ case startat of
		Just (Offset o) -> fromIntegral o
		Nothing -> 0
	
	getreq offset = P2P.Protocol.GET offset (ProtoAssociatedFile af) k
	
	af = AssociatedFile $ case baf of
		Just (B64FilePath f) -> Just f
		Nothing -> Nothing

clientGet
	:: ClientEnv
	-> ProtocolVersion
	-> B64Key
	-> B64UUID ServerSide
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Maybe B64FilePath
	-> Maybe Offset
	-> Maybe Auth
	-> IO ()
clientGet clientenv (ProtocolVersion ver) k su cu bypass af o auth =
	withClientM (cli k cu bypass af o auth) clientenv $ \case
		Left err -> throwM err
		Right respheaders -> do
			let dl = case lookupResponseHeader @DataLengthHeader' respheaders of
				Header h -> h
				_ -> error "missing data length header"
			liftIO $ print ("datalength", dl :: DataLength)
			b <- S.unSourceT (getResponse respheaders) gatherByteString
			liftIO $ print "got it all, writing to file 'got'"
			L.writeFile "got" b
  where
	cli =case ver of
		3 -> v3 su V3
		2 -> v2 su V2
		1 -> v1 su V1
		0 -> v0 su V0
		_ -> error "unsupported protocol version"
	
	v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI

gatherByteString :: S.StepT IO B.ByteString -> IO L.ByteString
gatherByteString = unsafeInterleaveIO . go
  where
	go S.Stop = return LI.Empty
	go (S.Error err) = giveup err
	go (S.Skip s) = go s
	go (S.Effect ms) = ms >>= go
	go (S.Yield v s) = LI.Chunk v <$> unsafeInterleaveIO (go s)

type CheckPresentAPI
	= KeyParam
	:> CU Required
	:> BypassUUIDs
	:> IsSecure
	:> AuthHeader
	:> Post '[JSON] CheckPresentResult

serveCheckPresent
	:: APIVersion v
	=> P2PHttpServerState
	-> B64UUID ServerSide
	-> v
	-> B64Key
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> IsSecure
	-> Maybe Auth
	-> Handler CheckPresentResult
serveCheckPresent st su apiver (B64Key k) cu bypass sec auth = do
	res <- withP2PConnection apiver st cu su bypass sec auth ReadAction id
		$ \conn -> liftIO $ proxyClientNetProto conn $ checkPresent k
	case res of
		Right b -> return (CheckPresentResult b)
		Left err -> throwError $ err500 { errBody = encodeBL err }

clientCheckPresent
	:: ClientEnv
	-> ProtocolVersion
	-> B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> Maybe Auth
	-> IO Bool
clientCheckPresent clientenv (ProtocolVersion ver) key cu su bypass auth =
	withClientM (cli su key cu bypass auth) clientenv $ \case
		Left err -> throwM err
		Right (CheckPresentResult res) -> return res
  where
	cli = case ver of
		3 -> flip v3 V3
		2 -> flip v2 V2
		1 -> flip v1 V1
		0 -> flip v0 V0
		_ -> error "unsupported protocol version"
	
	_ :<|> _ :<|> _ :<|> _ :<|>
		v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI

type RemoveAPI result
	= KeyParam
	:> CU Required
	:> BypassUUIDs
	:> IsSecure
	:> AuthHeader
	:> Post '[JSON] result
	
serveRemove
	:: APIVersion v
	=> P2PHttpServerState
	-> (RemoveResultPlus -> t)
	-> B64UUID ServerSide
	-> v
	-> B64Key
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> IsSecure
	-> Maybe Auth
	-> Handler t
serveRemove st resultmangle su apiver (B64Key k) cu bypass sec auth = do
	res <- withP2PConnection apiver st cu su bypass sec auth RemoveAction id
		$ \conn ->
			liftIO $ proxyClientNetProto conn $ remove Nothing k
	case res of
		(Right b, plusuuids) -> return $ resultmangle $ 
			RemoveResultPlus b (map B64UUID (fromMaybe [] plusuuids))
		(Left err, _) -> throwError $
			err500 { errBody = encodeBL err }

clientRemove
	:: ClientEnv
	-> ProtocolVersion
	-> B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> Maybe Auth
	-> IO RemoveResultPlus
clientRemove clientenv (ProtocolVersion ver) key cu su bypass auth =
	withClientM cli clientenv $ \case
		Left err -> throwM err
		Right res -> return res
  where
	cli = case ver of
		3 -> v3 su V3 key cu bypass auth
		2 -> v2 su V2 key cu bypass auth
		1 -> plus <$> v1 su V1 key cu bypass auth
		0 -> plus <$> v0 su V0 key cu bypass auth
		_ -> error "unsupported protocol version"
	
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI

type RemoveBeforeAPI
	= KeyParam
	:> CU Required
	:> BypassUUIDs
	:> QueryParam' '[Required] "timestamp" Timestamp
	:> IsSecure
	:> AuthHeader
	:> Post '[JSON] RemoveResultPlus

serveRemoveBefore
	:: APIVersion v
	=> P2PHttpServerState
	-> B64UUID ServerSide
	-> v
	-> B64Key
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Timestamp
	-> IsSecure
	-> Maybe Auth
	-> Handler RemoveResultPlus
serveRemoveBefore st su apiver (B64Key k) cu bypass (Timestamp ts) sec auth = do
	res <- withP2PConnection apiver st cu su bypass sec auth RemoveAction id
		$ \conn ->
			liftIO $ proxyClientNetProto conn $
				removeBeforeRemoteEndTime ts k
	case res of
		(Right b, plusuuids) -> return $ 
			RemoveResultPlus b (map B64UUID (fromMaybe [] plusuuids))
		(Left err, _) -> throwError $
			err500 { errBody = encodeBL err }

clientRemoveBefore
	:: ClientEnv
	-> ProtocolVersion
	-> B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> Timestamp
	-> Maybe Auth
	-> IO RemoveResultPlus
clientRemoveBefore clientenv (ProtocolVersion ver) key cu su bypass ts auth =
	withClientM (cli su key cu bypass ts auth) clientenv $ \case
		Left err -> throwM err
		Right res -> return res
  where
	cli = case ver of
		3 -> flip v3 V3
		_ -> error "unsupported protocol version"
	
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		v3 :<|> _ = client p2pHttpAPI

type GetTimestampAPI
	= CU Required
	:> BypassUUIDs
	:> IsSecure
	:> AuthHeader
	:> Post '[JSON] GetTimestampResult

serveGetTimestamp
	:: APIVersion v
	=> P2PHttpServerState
	-> B64UUID ServerSide
	-> v
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> IsSecure
	-> Maybe Auth
	-> Handler GetTimestampResult
serveGetTimestamp st su apiver cu bypass sec auth = do
	res <- withP2PConnection apiver st cu su bypass sec auth ReadAction id
		$ \conn ->
			liftIO $ proxyClientNetProto conn getTimestamp
	case res of
		Right ts -> return $ GetTimestampResult (Timestamp ts)
		Left err -> throwError $
			err500 { errBody = encodeBL err }

clientGetTimestamp
	:: ClientEnv
	-> ProtocolVersion
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> Maybe Auth
	-> IO GetTimestampResult
clientGetTimestamp clientenv (ProtocolVersion ver) cu su bypass auth = 
	withClientM (cli su cu bypass auth) clientenv $ \case
		Left err -> throwM err
		Right res -> return res
  where
	cli = case ver of
		3 -> flip v3 V3
		_ -> error "unsupported protocol version"
	
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|>
		v3 :<|> _ = client p2pHttpAPI

type PutAPI result
	= DataLengthHeaderRequired
	:> KeyParam
	:> CU Required
	:> BypassUUIDs
	:> AssociatedFileParam
	:> OffsetParam
	:> StreamBody NoFraming OctetStream (SourceIO B.ByteString)
	:> IsSecure
	:> AuthHeader
	:> Post '[JSON] result

servePut
	:: APIVersion v
	=> P2PHttpServerState
	-> (PutResultPlus -> t)
	-> B64UUID ServerSide
	-> v
	-> DataLength
	-> B64Key
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Maybe B64FilePath
	-> Maybe Offset
	-> S.SourceT IO B.ByteString
	-> IsSecure
	-> Maybe Auth
	-> Handler t
servePut st resultmangle su apiver (DataLength len) (B64Key k) cu bypass baf moffset stream sec auth = do
	validityv <- liftIO newEmptyTMVarIO
	let validitycheck = local $ runValidityCheck $
		liftIO $ atomically $ readTMVar validityv
	tooshortv <- liftIO newEmptyTMVarIO
	content <- liftIO $ S.unSourceT stream (gather validityv tooshortv)
	res <- withP2PConnection' apiver st cu su bypass sec auth WriteAction
		(\cst -> cst { connectionWaitVar = False }) $ \conn ->
			liftIO (protoaction conn content validitycheck)
				`finally` checktooshort conn tooshortv
	case res of
		Right (Right (Just plusuuids)) -> return $ resultmangle $
			PutResultPlus True (map B64UUID plusuuids)
		Right (Right Nothing) -> return $ resultmangle $
			PutResultPlus False []
		Right (Left protofail) -> throwError $
			err500 { errBody = encodeBL (describeProtoFailure protofail) }
		Left err -> throwError $
			err500 { errBody = encodeBL (show err) }
  where
	protoaction conn content validitycheck = inAnnexWorker st $
		enteringStage (TransferStage Download) $
			runFullProto (clientRunState conn) (clientP2PConnection conn) $
				protoaction' content validitycheck
	
	protoaction' content validitycheck = put' k af $ \offset' ->
		let offsetdelta = offset' - offset
		in case compare offset' offset of
			EQ -> sendContent' nullMeterUpdate (Len len)
				content validitycheck
			GT -> sendContent' nullMeterUpdate
				(Len (len - fromIntegral offsetdelta))
				(L.drop (fromIntegral offsetdelta) content)
				validitycheck
			LT -> sendContent' nullMeterUpdate
				(Len 0)
				mempty
				(return Invalid)
	
	offset = case moffset of
		Just (Offset o) -> o
		Nothing -> 0

	af = AssociatedFile $ case baf of
		Just (B64FilePath f) -> Just f
		Nothing -> Nothing

	-- Streams the ByteString from the client. Avoids returning a longer
	-- than expected ByteString by truncating to the expected length. 
	-- Returns a shorter than expected ByteString when the data is not
	-- valid.
	gather validityv tooshortv = unsafeInterleaveIO . go 0
	  where
		go n S.Stop = do
			atomically $ do
				writeTMVar validityv $
					if n == len then Valid else Invalid
				writeTMVar tooshortv (n /= len)
			return LI.Empty
		go n (S.Error _err) = do
			atomically $ do
				writeTMVar validityv Invalid
				writeTMVar tooshortv (n /= len)
			return LI.Empty
		go n (S.Skip s) = go n s
		go n (S.Effect ms) = ms >>= go n
		go n (S.Yield v s) =
			let !n' = n + fromIntegral (B.length v)
			in if n' > len
				then do
					atomically $ do
						writeTMVar validityv Invalid
						writeTMVar tooshortv True
					return $ LI.Chunk
						(B.take (fromIntegral (len - n')) v)
						LI.Empty
				else LI.Chunk v <$> unsafeInterleaveIO (go n' s)
			
	-- The connection can no longer be used when too short a DATA has
	-- been written to it.
	checktooshort conn tooshortv =
		liftIO $ whenM (atomically $ fromMaybe True <$> tryTakeTMVar tooshortv) $
			closeP2PConnection conn

clientPut
	:: ClientEnv
	-> ProtocolVersion
	-> B64Key
	-> B64UUID ServerSide
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Maybe Auth
	-> Maybe Offset
	-> AssociatedFile
	-> FilePath
	-> FileSize
	-> Annex Bool
	-> Annex PutResultPlus
clientPut clientenv (ProtocolVersion ver) k su cu bypass auth moffset af contentfile contentfilesize validitycheck = do
	checkv <- liftIO newEmptyTMVarIO
	checkresultv <- liftIO newEmptyTMVarIO
	let checker = do
		liftIO $ atomically $ takeTMVar checkv
		validitycheck >>= liftIO . atomically . putTMVar checkresultv
	checkerthread <- liftIO . async =<< forkState checker
	v <- liftIO $ withBinaryFile contentfile ReadMode $ \h -> do
		when (offset /= 0) $
			hSeek h AbsoluteSeek offset
		withClientM (cli (stream h checkv checkresultv)) clientenv return
	case v of
		Left err -> do
			void $ liftIO $ atomically $ tryPutTMVar checkv ()
			join $ liftIO (wait checkerthread)
			throwM err
		Right res -> do
			join $ liftIO (wait checkerthread)
			return res
  where
	stream h checkv checkresultv = S.SourceT $ \a -> do
		bl <- L.hGetContents h
		v <- newMVar (0, filter (not . B.null) (L.toChunks bl))
		a (go v)
	  where
		go v = S.fromActionStep B.null $
			modifyMVar v $ \case
				(n, (b:[])) -> do
					let !n' = n + B.length b
					ifM (checkvalid n')
						( return ((n', []), b)
						-- The key's content is invalid, but
						-- the amount of data is the same as the
						-- DataLengthHeader indicates. Truncate
						-- the stream by one byte to indicate
						-- to the server that it's not valid.
						, return ((n' - 1, []), B.take (B.length b - 1) b)
						)
				(n, []) -> do
					void $ checkvalid n
					return ((n, []), mempty)
				(n, (b:bs)) ->
					let !n' = n + B.length b
					in return ((n', bs), b)

		checkvalid n = do
			void $ liftIO $ atomically $ tryPutTMVar checkv ()
			valid <- liftIO $ atomically $ readTMVar checkresultv
			if not valid
				then return (n /= fromIntegral nlen)
				else return True

	baf = case af of
		AssociatedFile Nothing -> Nothing
		AssociatedFile (Just f) -> Just (B64FilePath f)

	len = DataLength nlen

	nlen = contentfilesize - offset

	offset = case moffset of
		Nothing -> 0
		Just (Offset o) -> fromIntegral o

	cli src = case ver of
		3 -> v3 su V3 len k cu bypass baf moffset src auth
		2 -> v2 su V2 len k cu bypass baf moffset src auth
		1 -> plus <$> v1 su V1 len k cu bypass baf moffset src auth
		0 -> plus <$> v0 su V0 len k cu bypass baf moffset src auth
		_ -> error "unsupported protocol version"

	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|>
		_ :<|>
		v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI

type PutOffsetAPI result
	= KeyParam
	:> CU Required
	:> BypassUUIDs
	:> Post '[JSON] result

servePutOffset
	:: APIVersion v
	=> P2PHttpServerState
	-> (PutOffsetResultPlus -> t)
	-> B64UUID ServerSide
	-> v
	-> B64Key
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Handler t
servePutOffset st resultmangle su apiver (B64Key k) cu bypass = undefined


clientPutOffset
	:: B64UUID ServerSide
	-> ProtocolVersion
	-> B64Key
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> ClientM PutOffsetResultPlus
clientPutOffset su (ProtocolVersion ver) = case ver of
	3 -> v3 su V3
	2 -> v2 su V2
	_ -> error "unsupported protocol version"
  where
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|>
		_ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		v3 :<|> v2 :<|> _ = client p2pHttpAPI

type LockContentAPI
	= KeyParam
	:> CU Required
	:> BypassUUIDs
	:> Post '[JSON] LockResult

serveLockContent
	:: APIVersion v
	=> P2PHttpServerState
	-> B64UUID ServerSide
	-> v
	-> B64Key
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Handler LockResult
serveLockContent = undefined -- TODO

clientLockContent
	:: B64UUID ServerSide
	-> ProtocolVersion
	-> B64Key
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> ClientM LockResult
clientLockContent su (ProtocolVersion ver) = case ver of
	3 -> v3 su V3
	2 -> v2 su V2
	1 -> v1 su V1
	0 -> v0 su V0
	_ -> error "unsupported protocol version"
  where
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|>
		_ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|>
		v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI

type KeepLockedAPI
	= LockIDParam
	:> CU Required
	:> BypassUUIDs
	:> Header "Connection" ConnectionKeepAlive
	:> Header "Keep-Alive" KeepAlive
	:> StreamBody NewlineFraming JSON (SourceIO UnlockRequest)
	:> Post '[JSON] LockResult

serveKeepLocked
	:: APIVersion v
	=> P2PHttpServerState
	-> B64UUID ServerSide
	-> v
	-> LockID
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Maybe ConnectionKeepAlive
	-> Maybe KeepAlive
	-> S.SourceT IO UnlockRequest
	-> Handler LockResult
serveKeepLocked st su apiver lckid cu _ _ _ unlockrequeststream = do
	_ <- liftIO $ S.unSourceT unlockrequeststream go
	return (LockResult False Nothing)
  where
	go S.Stop = dropLock lckid st
	go (S.Error _err) = dropLock lckid st
	go (S.Skip s)    = go s
	go (S.Effect ms) = ms >>= go
	go (S.Yield (UnlockRequest False) s) = go s
	go (S.Yield (UnlockRequest True) _) = dropLock lckid st

clientKeepLocked'
	:: B64UUID ServerSide
	-> ProtocolVersion
	-> LockID
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Maybe ConnectionKeepAlive
	-> Maybe KeepAlive
	-> S.SourceT IO UnlockRequest
	-> ClientM LockResult
clientKeepLocked' su (ProtocolVersion ver) = case ver of
	3 -> v3 su V3
	2 -> v2 su V2
	1 -> v1 su V1
	0 -> v0 su V0
	_ -> error "unsupported protocol version"
  where
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|>
		_ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI

clientKeepLocked
	:: ClientEnv
	-> ProtocolVersion
	-> LockID
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> TMVar Bool
	-> IO ()
clientKeepLocked clientenv protover lckid cu su bypass keeplocked = do
	let cli = clientKeepLocked' su protover lckid cu bypass
		(Just connectionKeepAlive) (Just keepAlive)
		(S.fromStepT unlocksender)
	withClientM cli clientenv $ \case
		Left err  -> throwM err
		Right (LockResult _ _) ->
			liftIO $ print "end of lock connection to server"
  where
	unlocksender =
		S.Yield (UnlockRequest False) $ S.Effect $ do
			liftIO $ print "sent keep locked request"
			return $ S.Effect $ do
				stilllock <- liftIO $ atomically $ takeTMVar keeplocked
				if stilllock
					then return unlocksender
					else do
						liftIO $ print "sending unlock request"
						return $ S.Yield (UnlockRequest True) S.Stop

type PV3 = Capture "v3" V3

type PV2 = Capture "v2" V2

type PV1 = Capture "v1" V1

type PV0 = Capture "v0" V0

type SU = Capture "serveruuid" (B64UUID ServerSide)

type CU req = QueryParam' '[req] "clientuuid" (B64UUID ClientSide)

type BypassUUIDs = QueryParams "bypass" (B64UUID Bypass)

type CaptureKey = Capture "key" B64Key

type KeyParam = QueryParam' '[Required] "key" B64Key

type AssociatedFileParam = QueryParam "associatedfile" B64FilePath
	
type OffsetParam = QueryParam "offset" Offset

type DataLengthHeader = Header DataLengthHeader' DataLength

type DataLengthHeaderRequired = Header' '[Required] DataLengthHeader' DataLength

type DataLengthHeader' = "X-git-annex-data-length"

type LockIDParam = QueryParam' '[Required] "lockid" LockID

type AuthHeader = Header "Authorization" Auth
