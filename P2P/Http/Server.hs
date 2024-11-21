{- P2P protocol over HTTP, server
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

module P2P.Http.Server (
	module P2P.Http,
	module P2P.Http.Server,
	module P2P.Http.Types,
	module P2P.Http.State,
) where

import Annex.Common
import P2P.Http
import P2P.Http.Types
import P2P.Http.State
import P2P.Protocol hiding (Offset, Bypass, auth)
import qualified P2P.Protocol
import P2P.IO
import P2P.Annex
import Annex.WorkerPool
import Types.WorkerPool
import Types.Direction
import Utility.Metered
import Utility.STM

import Servant
import qualified Servant.Types.SourceT as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI
import Control.Concurrent.Async
import Control.Concurrent
import System.IO.Unsafe
import Data.Either

p2pHttpApp :: TMVar P2PHttpServerState -> Application
p2pHttpApp = serve p2pHttpAPI . serveP2pHttp

serveP2pHttp :: TMVar P2PHttpServerState -> Server P2PHttpAPI
serveP2pHttp st
	=    serveGet st
	:<|> serveGet st
	:<|> serveGet st
	:<|> serveGet st
	:<|> serveGet st
	:<|> serveCheckPresent st
	:<|> serveCheckPresent st
	:<|> serveCheckPresent st
	:<|> serveCheckPresent st
	:<|> serveCheckPresent st
	:<|> serveRemove st id
	:<|> serveRemove st id
	:<|> serveRemove st id
	:<|> serveRemove st dePlus
	:<|> serveRemove st dePlus
	:<|> serveRemoveBefore st
	:<|> serveRemoveBefore st
	:<|> serveGetTimestamp st
	:<|> serveGetTimestamp st
	:<|> servePut st id
	:<|> servePut' st id
	:<|> servePut' st id
	:<|> servePut' st dePlus
	:<|> servePut' st dePlus
	:<|> servePutOffset st id
	:<|> servePutOffset st id
	:<|> servePutOffset st id
	:<|> servePutOffset st dePlus
	:<|> serveLockContent st
	:<|> serveLockContent st
	:<|> serveLockContent st
	:<|> serveLockContent st
	:<|> serveLockContent st
	:<|> serveKeepLocked st
	:<|> serveKeepLocked st
	:<|> serveKeepLocked st
	:<|> serveKeepLocked st
	:<|> serveKeepLocked st
	:<|> serveGetGeneric st

serveGetGeneric
	:: TMVar P2PHttpServerState
	-> B64UUID ServerSide
	-> B64Key
	-> Maybe (B64UUID ClientSide)
	-> [B64UUID Bypass]
	-> IsSecure
	-> Maybe Auth
	-> Handler (Headers '[DataLengthHeader] (S.SourceT IO B.ByteString))
serveGetGeneric st su@(B64UUID u) k mcu bypass =
	-- Use V0 because it does not alter the returned data to indicate
	-- Invalid content.
	serveGet st su V0 k (fromMaybe scu mcu) bypass Nothing Nothing
  where
	-- Reuse server UUID as client UUID.
	scu = B64UUID u :: B64UUID ClientSide

serveGet
	:: APIVersion v
	=> TMVar P2PHttpServerState
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
serveGet mst su apiver (B64Key k) cu bypass baf startat sec auth = do
	(conn, st) <- getP2PConnection apiver mst cu su bypass sec auth ReadAction id
	bsv <- liftIO newEmptyTMVarIO
	endv <- liftIO newEmptyTMVarIO
	validityv <- liftIO newEmptyTMVarIO
	finalv <- liftIO newEmptyTMVarIO
	annexworker <- liftIO $ async $ inAnnexWorker st $ do
		let storer _offset len = sendContentWith $ \bs -> liftIO $ do
			atomically $ putTMVar bsv (len, bs)
			atomically $ takeTMVar endv
			signalFullyConsumedByteString $
				connOhdl $ serverP2PConnection conn
			return $ \v -> do
				liftIO $ atomically $ putTMVar validityv v
				return True
		let noothermessages = const Nothing
		enteringStage (TransferStage Upload) $
			runFullProto (clientRunState conn) (clientP2PConnection conn) $
				void $ receiveContent Nothing nullMeterUpdate
					sizer storer noothermessages getreq
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
	nextchunk szv _checkvalid (b:bs) = do
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
				atomically $ putTMVar endv ()
				return $ case validity of
					Nothing -> True
					Just Valid -> True
					Just Invalid -> sz /= len
			, pure True
			)
	
	waitfinal endv finalv conn annexworker = do
		-- Wait for everything to be transferred before
		-- stopping the annexworker. The finalv will usually
		-- be written to at the end. If the client disconnects
		-- early that does not happen, so catch STM exception.
		alltransferred <- isRight
			<$> tryNonAsync (liftIO $ atomically $ takeTMVar finalv)
		-- Make sure the annexworker is not left blocked on endv
		-- if the client disconnected early.
		void $ liftIO $ atomically $ tryPutTMVar endv ()
		void $ tryNonAsync $ if alltransferred
			then releaseP2PConnection conn
			else closeP2PConnection conn
		void $ tryNonAsync $ wait annexworker
	
	sizer = pure $ Len $ case startat of
		Just (Offset o) -> fromIntegral o
		Nothing -> 0
	
	getreq offset = P2P.Protocol.GET offset af k
	
	af = ProtoAssociatedFile $ b64FilePathToAssociatedFile baf

serveCheckPresent
	:: APIVersion v
	=> TMVar P2PHttpServerState
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
		$ \(conn, _) -> liftIO $ proxyClientNetProto conn $ checkPresent k
	case res of
		Right b -> return (CheckPresentResult b)
		Left err -> throwError $ err500 { errBody = encodeBL err }

serveRemove
	:: APIVersion v
	=> TMVar P2PHttpServerState
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
		$ \(conn, _) ->
			liftIO $ proxyClientNetProto conn $ remove Nothing k
	case res of
		(Right b, plusuuids) -> return $ resultmangle $ 
			RemoveResultPlus b (map B64UUID (fromMaybe [] plusuuids))
		(Left err, _) -> throwError $
			err500 { errBody = encodeBL err }

serveRemoveBefore
	:: APIVersion v
	=> TMVar P2PHttpServerState
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
		$ \(conn, _) ->
			liftIO $ proxyClientNetProto conn $
				removeBeforeRemoteEndTime ts k
	case res of
		(Right b, plusuuids) -> return $ 
			RemoveResultPlus b (map B64UUID (fromMaybe [] plusuuids))
		(Left err, _) -> throwError $
			err500 { errBody = encodeBL err }

serveGetTimestamp
	:: APIVersion v
	=> TMVar P2PHttpServerState
	-> B64UUID ServerSide
	-> v
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> IsSecure
	-> Maybe Auth
	-> Handler GetTimestampResult
serveGetTimestamp st su apiver cu bypass sec auth = do
	res <- withP2PConnection apiver st cu su bypass sec auth ReadAction id
		$ \(conn, _) ->
			liftIO $ proxyClientNetProto conn getTimestamp
	case res of
		Right ts -> return $ GetTimestampResult (Timestamp ts)
		Left err -> throwError $
			err500 { errBody = encodeBL err }

servePut
	:: APIVersion v
	=> TMVar P2PHttpServerState
	-> (PutResultPlus -> t)
	-> B64UUID ServerSide
	-> v
	-> Maybe Bool
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
servePut mst resultmangle su apiver (Just True) _ k cu bypass baf _ _ sec auth = do
	res <- withP2PConnection' apiver mst cu su bypass sec auth WriteAction
		(\cst -> cst { connectionWaitVar = False }) (liftIO . protoaction)
	servePutResult resultmangle res
  where
	protoaction conn = servePutAction conn k baf $ \_offset -> do
		net $ sendMessage DATA_PRESENT
		checkSuccessPlus
servePut mst resultmangle su apiver _datapresent (DataLength len) k cu bypass baf moffset stream sec auth = do
	validityv <- liftIO newEmptyTMVarIO
	let validitycheck = local $ runValidityCheck $
		liftIO $ atomically $ readTMVar validityv
	tooshortv <- liftIO newEmptyTMVarIO
	content <- liftIO $ S.unSourceT stream (gather validityv tooshortv)
	res <- withP2PConnection' apiver mst cu su bypass sec auth WriteAction
		(\cst -> cst { connectionWaitVar = False }) $ \(conn, st) -> do
			liftIO $ void $ async $ checktooshort conn tooshortv
			liftIO (protoaction conn st content validitycheck)
	servePutResult resultmangle res
  where
	protoaction conn st content validitycheck = 
		servePutAction (conn, st) k baf $ \offset' ->
			let offsetdelta = offset' - offset
			in case compare offset' offset of
				EQ -> sendContent' nullMeterUpdate (Len len)
					content validitycheck
				GT -> sendContent' nullMeterUpdate
					(Len (len - fromIntegral offsetdelta))
					(L.drop (fromIntegral offsetdelta) content)
					validitycheck
				LT -> sendContent' nullMeterUpdate
					(Len len)
					content
					(validitycheck >>= \_ -> return Invalid)
	
	offset = case moffset of
		Just (Offset o) -> o
		Nothing -> 0

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
	checktooshort conn tooshortv = do
		liftIO $ whenM (atomically $ takeTMVar tooshortv) $
			closeP2PConnection conn

servePutAction
	:: (P2PConnectionPair, PerRepoServerState)
	-> B64Key
	-> Maybe B64FilePath
	-> (P2P.Protocol.Offset -> Proto (Maybe [UUID]))
	-> IO (Either SomeException (Either ProtoFailure (Maybe [UUID])))
servePutAction (conn, st) (B64Key k) baf a = inAnnexWorker st $
	enteringStage (TransferStage Download) $
		runFullProto (clientRunState conn) (clientP2PConnection conn) $
			put' k af a
  where
	af = b64FilePathToAssociatedFile baf

servePutResult :: (PutResultPlus -> t) -> Either SomeException (Either ProtoFailure (Maybe [UUID])) -> Handler t
servePutResult resultmangle res = case res of
	Right (Right (Just plusuuids)) -> return $ resultmangle $
		PutResultPlus True (map B64UUID plusuuids)
	Right (Right Nothing) -> return $ resultmangle $
		PutResultPlus False []
	Right (Left protofail) -> throwError $
		err500 { errBody = encodeBL (describeProtoFailure protofail) }
	Left err -> throwError $
		err500 { errBody = encodeBL (show err) }

servePut'
	:: APIVersion v
	=> TMVar P2PHttpServerState
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
servePut' st resultmangle su v = servePut st resultmangle su v Nothing

servePutOffset
	:: APIVersion v
	=> TMVar P2PHttpServerState
	-> (PutOffsetResultPlus -> t)
	-> B64UUID ServerSide
	-> v
	-> B64Key
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> IsSecure
	-> Maybe Auth
	-> Handler t
servePutOffset st resultmangle su apiver (B64Key k) cu bypass sec auth = do
	res <- withP2PConnection apiver st cu su bypass sec auth WriteAction
		(\cst -> cst { connectionWaitVar = False }) $ \(conn, _) ->
			liftIO $ proxyClientNetProto conn $ getPutOffset k af
	case res of
		Right offset -> return $ resultmangle $
			PutOffsetResultPlus (Offset offset)
		Left plusuuids -> return $ resultmangle $
			PutOffsetResultAlreadyHavePlus (map B64UUID plusuuids)
  where
	af = AssociatedFile Nothing

serveLockContent
	:: APIVersion v
	=> TMVar P2PHttpServerState
	-> B64UUID ServerSide
	-> v
	-> B64Key
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> IsSecure
	-> Maybe Auth
	-> Handler LockResult
serveLockContent mst su apiver (B64Key k) cu bypass sec auth = do
	(conn, st) <- getP2PConnection apiver mst cu su bypass sec auth LockAction id
	let lock = do
		lockresv <- newEmptyTMVarIO
		unlockv <- newEmptyTMVarIO
		annexworker <- async $ inAnnexWorker st $ do
			lockres <- runFullProto (clientRunState conn) (clientP2PConnection conn) $ do
				net $ sendMessage (LOCKCONTENT k)
				checkSuccess
			liftIO $ atomically $ putTMVar lockresv lockres
			liftIO $ atomically $ takeTMVar unlockv
			void $ runFullProto (clientRunState conn) (clientP2PConnection conn) $ do
				net $ sendMessage UNLOCKCONTENT
		atomically (takeTMVar lockresv) >>= \case
			Right True -> return (Just (annexworker, unlockv))
			_ -> return Nothing
	let unlock (annexworker, unlockv) = do
		atomically $ putTMVar unlockv ()
		void $ wait annexworker
		releaseP2PConnection conn
	liftIO $ mkLocker lock unlock >>= \case
		Just (locker, lockid) -> do
			liftIO $ storeLock lockid locker st
			return $ LockResult True (Just lockid)
		Nothing -> return $ LockResult False Nothing

serveKeepLocked
	:: APIVersion v
	=> TMVar P2PHttpServerState
	-> B64UUID ServerSide
	-> v
	-> LockID
	-> Maybe (B64UUID ClientSide)
	-> [B64UUID Bypass]
	-> IsSecure
	-> Maybe Auth
	-> Maybe ConnectionKeepAlive
	-> Maybe KeepAlive
	-> S.SourceT IO UnlockRequest
	-> Handler LockResult
serveKeepLocked mst su _apiver lckid _cu _bypass sec auth _ _ unlockrequeststream = do
	checkAuthActionClass mst su sec auth LockAction $ \st _ -> do
		liftIO $ keepingLocked lckid st
		_ <- liftIO $ S.unSourceT unlockrequeststream (go st)
		return (LockResult False Nothing)
  where
	go st S.Stop = dropLock lckid st
	go st (S.Error _err) = dropLock lckid st
	go st (S.Skip s)    = go st s
	go st (S.Effect ms) = ms >>= go st
	go st (S.Yield (UnlockRequest False) s) = go st s
	go st (S.Yield (UnlockRequest True) _) = dropLock lckid st
