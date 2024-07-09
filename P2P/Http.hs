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
{-# LANGUAGE OverloadedStrings #-}

module P2P.Http (
	module P2P.Http,
	module P2P.Http.Types,
	module P2P.Http.State,
) where

import Annex.Common
import P2P.Http.Types
import P2P.Http.State
import qualified P2P.Protocol as P2P

import Servant
import Servant.Client.Streaming
import qualified Servant.Types.SourceT as S
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Data.ByteString as B
import Control.Concurrent
import Control.Concurrent.STM

type P2PHttpAPI
	=    "git-annex" :> PV3 :> "key" :> CaptureKey :> GetAPI
	:<|> "git-annex" :> PV2 :> "key" :> CaptureKey :> GetAPI
	:<|> "git-annex" :> PV1 :> "key" :> CaptureKey :> GetAPI
	:<|> "git-annex" :> PV0 :> "key" :> CaptureKey :> GetAPI
	:<|> "git-annex" :> PV3 :> "checkpresent" :> CheckPresentAPI
	:<|> "git-annex" :> PV2 :> "checkpresent" :> CheckPresentAPI
	:<|> "git-annex" :> PV1 :> "checkpresent" :> CheckPresentAPI
	:<|> "git-annex" :> PV0 :> "checkpresent" :> CheckPresentAPI
	:<|> "git-annex" :> PV3 :> "remove" :> RemoveAPI RemoveResultPlus
	:<|> "git-annex" :> PV2 :> "remove" :> RemoveAPI RemoveResultPlus
	:<|> "git-annex" :> PV1 :> "remove" :> RemoveAPI RemoveResult
	:<|> "git-annex" :> PV0 :> "remove" :> RemoveAPI RemoveResult
	:<|> "git-annex" :> PV3 :> "remove-before" :> RemoveBeforeAPI
	:<|> "git-annex" :> PV3 :> "gettimestamp" :> GetTimestampAPI
	:<|> "git-annex" :> PV3 :> "put" :> DataLengthHeader
		:> PutAPI PutResultPlus
	:<|> "git-annex" :> PV2 :> "put" :> DataLengthHeader
		:> PutAPI PutResultPlus
	:<|> "git-annex" :> PV1 :> "put" :> DataLengthHeader
		:> PutAPI PutResult
	:<|> "git-annex" :> PV0 :> "put"
		:> PutAPI PutResult
	:<|> "git-annex" :> PV3 :> "putoffset"
		:> PutOffsetAPI PutOffsetResultPlus
	:<|> "git-annex" :> PV2 :> "putoffset"
		:> PutOffsetAPI PutOffsetResultPlus
	:<|> "git-annex" :> PV1 :> "putoffset"
		:> PutOffsetAPI PutOffsetResult
	:<|> "git-annex" :> PV3 :> "lockcontent" :> LockContentAPI
	:<|> "git-annex" :> PV2 :> "lockcontent" :> LockContentAPI
	:<|> "git-annex" :> PV1 :> "lockcontent" :> LockContentAPI
	:<|> "git-annex" :> PV0 :> "lockcontent" :> LockContentAPI
	:<|> "git-annex" :> PV3 :> "keeplocked" :> KeepLockedAPI
	:<|> "git-annex" :> PV2 :> "keeplocked" :> KeepLockedAPI
	:<|> "git-annex" :> PV1 :> "keeplocked" :> KeepLockedAPI
	:<|> "git-annex" :> PV0 :> "keeplocked" :> KeepLockedAPI
	:<|> "git-annex" :> "key" :> CaptureKey :> GetGenericAPI

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
	:<|> (\v -> servePut st dePlus v Nothing)
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

type GetGenericAPI = StreamGet NoFraming OctetStream (SourceIO B.ByteString)

serveGetGeneric :: P2PHttpServerState -> B64Key -> Handler (S.SourceT IO B.ByteString)
serveGetGeneric = undefined

type GetAPI
	= ClientUUID Optional
	:> ServerUUID Optional
	:> BypassUUIDs
	:> AssociatedFileParam
	:> OffsetParam
	:> StreamGet NoFraming OctetStream
		(Headers '[DataLengthHeader] (SourceIO B.ByteString))

serveGet
	:: APIVersion v
	=> P2PHttpServerState
	-> v
	-> B64Key
	-> Maybe (B64UUID ClientSide)
	-> Maybe (B64UUID ServerSide)
	-> [B64UUID Bypass]
	-> Maybe B64FilePath
	-> Maybe Offset
	-> Handler (Headers '[DataLengthHeader] (S.SourceT IO B.ByteString))
serveGet = undefined

clientGet
	:: P2P.ProtocolVersion
	-> B64Key
	-> Maybe (B64UUID ClientSide)
	-> Maybe (B64UUID ServerSide)
	-> [B64UUID Bypass]
	-> Maybe B64FilePath
	-> Maybe Offset
	-> ClientM (Headers '[DataLengthHeader] (S.SourceT IO B.ByteString))
clientGet (P2P.ProtocolVersion ver) = case ver of
	3 -> v3 V3
	2 -> v2 V2
	1 -> v1 V1
	0 -> v0 V0
	_ -> error "unsupported protocol version"
  where
	v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI

type CheckPresentAPI
	= KeyParam
	:> ClientUUID Required
	:> ServerUUID Required
	:> BypassUUIDs
	:> Post '[JSON] CheckPresentResult

serveCheckPresent
	:: APIVersion v
	=> P2PHttpServerState
	-> v
	-> B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> Handler CheckPresentResult
serveCheckPresent st apiver (B64Key k) cu su bypass = do
	res <- liftIO $ inP2PConnection st cu su bypass $ P2P.checkPresent k
	case res of
		Right (Right b) -> return (CheckPresentResult b)
		Right (Left err) ->
			throwError $ err500 { errBody = encodeBL err }
		Left err ->
			throwError $ err500 { errBody = encodeBL err }

clientCheckPresent
	:: P2P.ProtocolVersion
	-> B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> ClientM CheckPresentResult
clientCheckPresent (P2P.ProtocolVersion ver) = case ver of
	3 -> v3 V3
	2 -> v2 V2
	1 -> v1 V1
	0 -> v0 V0
	_ -> error "unsupported protocol version"
  where
	_ :<|> _ :<|> _ :<|> _ :<|>
		v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI

type RemoveAPI result
	= KeyParam
	:> ClientUUID Required
	:> ServerUUID Required
	:> BypassUUIDs
	:> Post '[JSON] result
	
serveRemove
	:: APIVersion v
	=> P2PHttpServerState
	-> (RemoveResultPlus -> t)
	-> v
	-> B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> Handler t
serveRemove = undefined

clientRemove
	:: P2P.ProtocolVersion
	-> B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> ClientM RemoveResultPlus
clientRemove (P2P.ProtocolVersion ver) k cu su bypass = case ver of
	3 -> v3 V3 k cu su bypass
	2 -> v2 V2 k cu su bypass
	1 -> plus <$> v1 V1 k cu su bypass
	0 -> plus <$> v0 V0 k cu su bypass
	_ -> error "unsupported protocol version"
  where
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI
	
type RemoveBeforeAPI
	= KeyParam
	:> ClientUUID Required
	:> ServerUUID Required
	:> BypassUUIDs
	:> QueryParam' '[Required] "timestamp" Timestamp
	:> Post '[JSON] RemoveResult

serveRemoveBefore
	:: APIVersion v
	=> P2PHttpServerState
	-> v
	-> B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> Timestamp
	-> Handler RemoveResult
serveRemoveBefore = undefined

clientRemoveBefore
	:: P2P.ProtocolVersion
	-> B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> Timestamp
	-> ClientM RemoveResult
clientRemoveBefore (P2P.ProtocolVersion ver) = case ver of
	3 -> v3 V3
	_ -> error "unsupported protocol version"
  where
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		v3 :<|> _ = client p2pHttpAPI


type GetTimestampAPI
	= ClientUUID Required
	:> ServerUUID Required
	:> BypassUUIDs
	:> Post '[JSON] GetTimestampResult

serveGetTimestamp
	:: APIVersion v
	=> P2PHttpServerState
	-> v
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> Handler GetTimestampResult
serveGetTimestamp = undefined

clientGetTimestamp
	:: P2P.ProtocolVersion
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> ClientM GetTimestampResult
clientGetTimestamp (P2P.ProtocolVersion ver) = case ver of
	3 -> v3 V3
	_ -> error "unsupported protocol version"
  where
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|>
		v3 :<|> _ = client p2pHttpAPI

type PutAPI result
	= KeyParam
	:> ClientUUID Required
	:> ServerUUID Required
	:> BypassUUIDs
	:> AssociatedFileParam
	:> OffsetParam
	:> Header' '[Required] "X-git-annex-data-length" DataLength
	:> StreamBody NoFraming OctetStream (SourceIO B.ByteString)
	:> Post '[JSON] result

servePut
	:: APIVersion v
	=> P2PHttpServerState
	-> (PutResultPlus -> t)
	-> v
	-> Maybe Integer
	-> B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> Maybe B64FilePath
	-> Maybe Offset
	-> DataLength
	-> S.SourceT IO B.ByteString
	-> Handler t
servePut = undefined

clientPut
	:: P2P.ProtocolVersion
	-> Maybe Integer
	-> B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> Maybe B64FilePath
	-> Maybe Offset
	-> DataLength
	-> S.SourceT IO B.ByteString
	-> ClientM PutResultPlus
clientPut (P2P.ProtocolVersion ver) sz k cu su bypass af o l src = case ver of
	3 -> v3 V3 sz k cu su bypass af o l src
	2 -> v2 V2 sz k cu su bypass af o l src
	1 -> plus <$> v1 V1 sz k cu su bypass af o l src
	0 -> plus <$> v0 V0 k cu su bypass af o l src
	_ -> error "unsupported protocol version"
  where
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|>
		_ :<|>
		v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI

type PutOffsetAPI result
	= KeyParam
	:> ClientUUID Required
	:> ServerUUID Required
	:> BypassUUIDs
	:> Post '[JSON] result

servePutOffset
	:: APIVersion v
	=> P2PHttpServerState
	-> (PutOffsetResultPlus -> t)
	-> v
	-> B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> Handler t
servePutOffset = undefined

clientPutOffset
	:: P2P.ProtocolVersion
	-> B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> ClientM PutOffsetResultPlus
clientPutOffset (P2P.ProtocolVersion ver) = case ver of
	3 -> v3 V3
	2 -> v2 V2
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
	:> ClientUUID Required
	:> ServerUUID Required
	:> BypassUUIDs
	:> Post '[JSON] LockResult

serveLockContent
	:: APIVersion v
	=> P2PHttpServerState
	-> v
	-> B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> Handler LockResult
serveLockContent = undefined

clientLockContent
	:: P2P.ProtocolVersion
	-> B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> ClientM LockResult
clientLockContent (P2P.ProtocolVersion ver) = case ver of
	3 -> v3 V3
	2 -> v2 V2
	1 -> v1 V1
	0 -> v0 V0
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
	:> ClientUUID Required
	:> ServerUUID Required
	:> BypassUUIDs
	:> Header "Connection" ConnectionKeepAlive
	:> Header "Keep-Alive" KeepAlive
	:> StreamBody NewlineFraming JSON (SourceIO UnlockRequest)
	:> Post '[JSON] LockResult

serveKeepLocked
	:: APIVersion v
	=> P2PHttpServerState
	-> v
	-> LockID
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> Maybe ConnectionKeepAlive
	-> Maybe KeepAlive
	-> S.SourceT IO UnlockRequest
	-> Handler LockResult
serveKeepLocked st apiver lckid cu su _ _ _ unlockrequeststream = do
	_ <- liftIO $ S.unSourceT unlockrequeststream go
	return (LockResult False Nothing)
  where
	go S.Stop = dropLock lckid st
	go (S.Error _err) = dropLock lckid st
	go (S.Skip s)    = go s
	go (S.Effect ms) = ms >>= go
	go (S.Yield (UnlockRequest False) s) = go s
	go (S.Yield (UnlockRequest True) _) = dropLock lckid st

clientKeepLocked
	:: P2P.ProtocolVersion
	-> LockID
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> Maybe ConnectionKeepAlive
	-> Maybe KeepAlive
	-> S.SourceT IO UnlockRequest
	-> ClientM LockResult
clientKeepLocked (P2P.ProtocolVersion ver) = case ver of
	3 -> v3 V3
	2 -> v2 V2
	1 -> v1 V1
	0 -> v0 V0
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

clientKeepLocked'
	:: ClientEnv
	-> P2P.ProtocolVersion
	-> LockID
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> TMVar Bool
	-> IO ()
clientKeepLocked' clientenv protover lckid cu su bypass keeplocked = do
	let cli = clientKeepLocked protover lckid cu su bypass
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

testClientLock = do
	mgr <- newManager defaultManagerSettings
	burl <- parseBaseUrl "http://localhost:8080/"
	keeplocked <- newEmptyTMVarIO
	_ <- forkIO $ do
		print "running, press enter to drop lock"
		_ <- getLine
		atomically $ writeTMVar keeplocked False
	clientKeepLocked' (mkClientEnv mgr burl)
		(P2P.ProtocolVersion 3)
		(B64UUID (toUUID ("lck" :: String)))
		(B64UUID (toUUID ("cu" :: String)))
		(B64UUID (toUUID ("su" :: String)))
		[]
		keeplocked

type PV3 = Capture "v3" V3

type PV2 = Capture "v2" V2

type PV1 = Capture "v1" V1

type PV0 = Capture "v0" V0

type ClientUUID req = QueryParam' '[req] "clientuuid" (B64UUID ClientSide)

type ServerUUID req = QueryParam' '[req] "serveruuid" (B64UUID ServerSide)

type BypassUUIDs = QueryParams "bypass" (B64UUID Bypass)

type CaptureKey = Capture "key" B64Key

type KeyParam = QueryParam' '[Required] "key" B64Key

type AssociatedFileParam = QueryParam "associatedfile" B64FilePath
	
type OffsetParam = QueryParam "offset" Offset

type DataLengthHeader = Header "X-git-annex-data-length" Integer

type LockIDParam = QueryParam' '[Required] "lockid" LockID
