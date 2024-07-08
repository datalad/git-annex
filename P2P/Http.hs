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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module P2P.Http where

import Annex.Common
import qualified P2P.Protocol as P2P
import Utility.Base64
import Utility.MonotonicClock

import Servant
import Servant.Client.Streaming
import Servant.Client.Core.RunClient
import qualified Servant.Client.Core.Request as R
import qualified Servant.Types.SourceT as S
import Servant.API.WebSocket
import qualified Network.WebSockets as Websocket
import qualified Network.WebSockets.Client as Websocket
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Client (newManager, defaultManagerSettings, path)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import Text.Read (readMaybe)
import Data.Aeson hiding (Key)
import Data.Maybe
import Data.Foldable
import Control.Monad.Reader
import Control.DeepSeq
import GHC.Generics

type P2PHttpAPI
	= "git-annex" :> "v3" :> "key" :> CaptureKey
		:> GetAPI '[DataLengthHeader]
	:<|> "git-annex" :> "v2" :> "key" :> CaptureKey
		:> GetAPI '[DataLengthHeader]
	:<|> "git-annex" :> "v1" :> "key" :> CaptureKey
		:> GetAPI '[DataLengthHeader]
	:<|> "git-annex" :> "v0" :> "key" :> CaptureKey
		:> GetAPI '[]
	:<|> "git-annex" :> "v3" :> "checkpresent" :> CheckPresentAPI
	:<|> "git-annex" :> "v2" :> "checkpresent" :> CheckPresentAPI
	:<|> "git-annex" :> "v1" :> "checkpresent" :> CheckPresentAPI
	:<|> "git-annex" :> "v0" :> "checkpresent" :> CheckPresentAPI
	:<|> "git-annex" :> "v3" :> "remove" :> RemoveAPI RemoveResultPlus
	:<|> "git-annex" :> "v2" :> "remove" :> RemoveAPI RemoveResultPlus
	:<|> "git-annex" :> "v1" :> "remove" :> RemoveAPI RemoveResult
	:<|> "git-annex" :> "v0" :> "remove" :> RemoveAPI RemoveResult
	:<|> "git-annex" :> "v3" :> "remove-before" :> RemoveBeforeAPI
	:<|> "git-annex" :> "v3" :> "gettimestamp" :> GetTimestampAPI
	:<|> "git-annex" :> "v3" :> "put" :> DataLengthHeader
		:> PutAPI PutResultPlus
	:<|> "git-annex" :> "v2" :> "put" :> DataLengthHeader
		:> PutAPI PutResultPlus
	:<|> "git-annex" :> "v1" :> "put" :> DataLengthHeader
		:> PutAPI PutResult
	:<|> "git-annex" :> "v0" :> "put"
		:> PutAPI PutResult
	:<|> "git-annex" :> "v3" :> "putoffset"
		:> PutOffsetAPI PutOffsetResultPlus
	:<|> "git-annex" :> "v2" :> "putoffset"
		:> PutOffsetAPI PutOffsetResultPlus
	:<|> "git-annex" :> "v1" :> "putoffset"
		:> PutOffsetAPI PutOffsetResult
	:<|> "git-annex" :> "v3" :> "lockcontent" :> LockContentAPI
	:<|> "git-annex" :> "v2" :> "lockcontent" :> LockContentAPI
	:<|> "git-annex" :> "v1" :> "lockcontent" :> LockContentAPI
	:<|> "git-annex" :> "v0" :> "lockcontent" :> LockContentAPI
	:<|> "git-annex" :> "key" :> CaptureKey :> GetAPI '[]

p2pHttpAPI :: Proxy P2PHttpAPI
p2pHttpAPI = Proxy

p2pHttp :: Application
p2pHttp = serve p2pHttpAPI serveP2pHttp

serveP2pHttp :: Server P2PHttpAPI
serveP2pHttp
	= serveGet
	:<|> serveGet
	:<|> serveGet
	:<|> serveGet0
	:<|> serveCheckPresent
	:<|> serveCheckPresent
	:<|> serveCheckPresent
	:<|> serveCheckPresent
	:<|> serveRemove id
	:<|> serveRemove id
	:<|> serveRemove dePlus
	:<|> serveRemove dePlus
	:<|> serveRemoveBefore
	:<|> serveGetTimestamp
	:<|> servePut id
	:<|> servePut id
	:<|> servePut dePlus
	:<|> servePut dePlus Nothing
	:<|> servePutOffset id
	:<|> servePutOffset id
	:<|> servePutOffset dePlus
	:<|> serveLockContent
	:<|> serveLockContent
	:<|> serveLockContent
	:<|> serveLockContent
	:<|> serveGet0

type GetAPI headers
	= ClientUUID Optional
	:> ServerUUID Optional
	:> BypassUUIDs
	:> AssociatedFileParam
	:> OffsetParam
	:> StreamGet NoFraming OctetStream
		(Headers headers (SourceIO B.ByteString))

serveGet
	:: B64Key
	-> Maybe (B64UUID ClientSide)
	-> Maybe (B64UUID ServerSide)
	-> [B64UUID Bypass]
	-> Maybe B64FilePath
	-> Maybe Offset
	-> Handler (Headers '[DataLengthHeader] (S.SourceT IO B.ByteString))
serveGet = undefined

serveGet0
	:: B64Key
	-> Maybe (B64UUID ClientSide)
	-> Maybe (B64UUID ServerSide)
	-> [B64UUID Bypass]
	-> Maybe B64FilePath
	-> Maybe Offset
	-> Handler (Headers '[] (S.SourceT IO B.ByteString))
serveGet0 = undefined

clientGet
	:: P2P.ProtocolVersion
	-> B64Key
	-> Maybe (B64UUID ClientSide)
	-> Maybe (B64UUID ServerSide)
	-> [B64UUID Bypass]
	-> Maybe B64FilePath
	-> Maybe Offset
	-> ClientM (Headers '[DataLengthHeader] (S.SourceT IO B.ByteString))
clientGet (P2P.ProtocolVersion ver) k cu su bypass af o = case ver of
	3 -> v3 k cu su bypass af o
	2 -> v2 k cu su bypass af o
	1 -> error "XXX" -- TODO v1  
	0 -> error "XXX" -- TODO v0
	_ -> error "unsupported protocol version"
  where
	_ :<|> v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI

type CheckPresentAPI
	= KeyParam
	:> ClientUUID Required
	:> ServerUUID Required
	:> BypassUUIDs
	:> Post '[JSON] CheckPresentResult

serveCheckPresent
	:: B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> Handler CheckPresentResult
serveCheckPresent = undefined

clientCheckPresent
	:: P2P.ProtocolVersion
	-> B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> ClientM CheckPresentResult
clientCheckPresent (P2P.ProtocolVersion ver) = case ver of
	3 -> v3
	2 -> v2
	1 -> v1
	0 -> v0
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
	:: (RemoveResultPlus -> t)
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
	3 -> v3 k cu su bypass
	2 -> v2 k cu su bypass
	1 -> plus <$> v1 k cu su bypass
	0 -> plus <$> v0 k cu su bypass
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
	:: B64Key
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
	3 -> v3
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
	:: B64UUID ClientSide
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
	3 -> v3
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
	:: (PutResultPlus -> t)
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
	3 -> v3 sz k cu su bypass af o l src
	2 -> v2 sz k cu su bypass af o l src
	1 -> plus <$> v1 sz k cu su bypass af o l src
	0 -> plus <$> v0 k cu su bypass af o l src
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
	:: (PutOffsetResultPlus -> t)
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
	3 -> v3
	2 -> v2
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
	:> WebSocket

serveLockContent
	:: B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> Websocket.Connection
	-> Handler ()
serveLockContent = undefined

data WebSocketClient = WebSocketClient R.Request

-- XXX this is enough to let servant-client work, but it's not yet
-- possible to run a WebSocketClient.
instance RunClient m => HasClient m WebSocket where
	type Client m WebSocket = WebSocketClient
	clientWithRoute _pm Proxy req = WebSocketClient req
	hoistClientMonad _ _ _ w = w

clientLockContent
	:: B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> WebSocketClient
clientLockContent = v3
  where
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|>
		_ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|>
		v3 :<|> _ = client p2pHttpAPI
	-- XXX add other protocol versions

--XXX test code
query :: ClientM PutOffsetResultPlus
query = do
	clientPutOffset (P2P.ProtocolVersion 3)
		(B64Key (fromJust $ deserializeKey "WORM--foo"))
		(B64UUID (toUUID ("client" :: String)))	
		(B64UUID (toUUID ("server" :: String)))	
		[]

--XXX test code
query' :: WebSocketClient
query' = do
	clientLockContent (B64Key (fromJust $ deserializeKey "WORM--foo"))
		(B64UUID (toUUID ("client" :: String)))	
		(B64UUID (toUUID ("server" :: String)))	
		[]

runWebSocketClient :: WebSocketClient -> Websocket.ClientApp a -> ClientM a
runWebSocketClient (WebSocketClient req) app = do
	clientenv <- ask
	let burl = baseUrl clientenv
	let creq = defaultMakeClientRequest burl req
	case baseUrlScheme burl of
		Http -> liftIO $ Websocket.runClient 
			(baseUrlHost burl)
			(baseUrlPort burl)
			(decodeBS (path creq))
			app
		Https -> error "TODO" -- XXX

--XXX test code
run :: IO ()
run = do
  manager' <- newManager defaultManagerSettings
  let WebSocketClient wscreq = query'
  _ <- runClientM (runWebSocketClient query' wsapp) 
  	(mkClientEnv manager' (BaseUrl Http "localhost" 8081 ""))
  return ()
  where
	wsapp conn = Websocket.sendTextData conn ("hello, world" :: T.Text)

type ClientUUID req = QueryParam' '[req] "clientuuid" (B64UUID ClientSide)

type ServerUUID req = QueryParam' '[req] "serveruuid" (B64UUID ServerSide)

type BypassUUIDs = QueryParams "bypass" (B64UUID Bypass)

type CaptureKey = Capture "key" B64Key

type KeyParam = QueryParam' '[Required] "key" B64Key

type AssociatedFileParam = QueryParam "associatedfile" B64FilePath
	
type OffsetParam = QueryParam "offset" Offset

type DataLengthHeader = Header "X-git-annex-data-length" Integer

-- Phantom types for B64UIID
data ClientSide
data ServerSide
data Bypass
data Plus

-- Keys, UUIDs, and filenames are base64 encoded since Servant uses 
-- Text and so needs UTF-8.
newtype B64Key = B64Key Key
	deriving (Show)

newtype B64UUID t = B64UUID UUID
	deriving (Show, Generic, NFData)

newtype B64FilePath = B64FilePath RawFilePath
	deriving (Show)

newtype DataLength = DataLength Integer
	deriving (Show)

newtype CheckPresentResult = CheckPresentResult Bool
	deriving (Show)

newtype RemoveResult = RemoveResult Bool
	deriving (Show)

data RemoveResultPlus = RemoveResultPlus Bool [B64UUID Plus]
	deriving (Show)

newtype GetTimestampResult = GetTimestampResult Timestamp
	deriving (Show)

newtype PutResult = PutResult Bool
	deriving (Eq, Show)

data PutResultPlus = PutResultPlus Bool [B64UUID Plus]
	deriving (Show)

newtype PutOffsetResult = PutOffsetResult Offset
	deriving (Show)

data PutOffsetResultPlus = PutOffsetResultPlus Offset [B64UUID Plus]
	deriving (Show, Generic, NFData)

newtype Offset = Offset P2P.Offset
	deriving (Show, Generic, NFData)

newtype Timestamp = Timestamp MonotonicTimestamp
	deriving (Show)

instance ToHttpApiData B64Key where
	toUrlPiece (B64Key k) = TE.decodeUtf8Lenient $
		toB64 (serializeKey' k)

instance FromHttpApiData B64Key where
	parseUrlPiece t = case fromB64Maybe (TE.encodeUtf8 t) of
		Nothing -> Left "unable to base64 decode key"
		Just b -> maybe (Left "key parse error") (Right . B64Key)
			(deserializeKey' b)

instance ToHttpApiData (B64UUID t) where
	toUrlPiece (B64UUID u) = TE.decodeUtf8Lenient $
		toB64 (fromUUID u)

instance FromHttpApiData (B64UUID t) where
	parseUrlPiece t = case fromB64Maybe (TE.encodeUtf8 t) of
		Nothing -> Left "unable to base64 decode UUID"
		Just b -> case toUUID b of
			u@(UUID _) -> Right (B64UUID u)
			NoUUID -> Left "empty UUID"

instance ToHttpApiData B64FilePath where
	toUrlPiece (B64FilePath f) = TE.decodeUtf8Lenient $ toB64 f

instance FromHttpApiData B64FilePath where
	parseUrlPiece t = case fromB64Maybe (TE.encodeUtf8 t) of
		Nothing -> Left "unable to base64 decode filename"
		Just b -> Right (B64FilePath b)

instance ToHttpApiData Offset where
	toUrlPiece (Offset (P2P.Offset n)) = T.pack (show n)

instance FromHttpApiData Offset where
	parseUrlPiece t = case readMaybe (T.unpack t) of
		Nothing -> Left "offset parse error"
		Just n -> Right (Offset (P2P.Offset n))

instance ToHttpApiData Timestamp where
	toUrlPiece (Timestamp (MonotonicTimestamp n)) = T.pack (show n)

instance FromHttpApiData Timestamp where
	parseUrlPiece t = case readMaybe (T.unpack t) of
		Nothing -> Left "timestamp parse error"
		Just n -> Right (Timestamp (MonotonicTimestamp n))

instance ToHttpApiData DataLength where
	toUrlPiece (DataLength n) = T.pack (show n)

instance FromHttpApiData DataLength where
	parseUrlPiece t = case readMaybe (T.unpack t) of
		Nothing -> Left "X-git-annex-data-length parse error"
		Just n -> Right (DataLength n)

instance ToJSON PutResult where
	toJSON (PutResult b) =
		object ["stored" .= b]

instance FromJSON PutResult where
	parseJSON = withObject "PutResult" $ \v -> PutResult
		<$> v .: "stored"

instance ToJSON PutResultPlus where
	toJSON (PutResultPlus b us) = object
		[ "stored" .= b
		, "plusuuids" .= plusList us
		]

instance FromJSON PutResultPlus where
	parseJSON = withObject "PutResultPlus" $ \v -> PutResultPlus
		<$> v .: "stored"
		<*> v .: "plusuuids"

instance ToJSON CheckPresentResult where
	toJSON (CheckPresentResult b) = object
		["present" .= b]

instance FromJSON CheckPresentResult where
	parseJSON = withObject "CheckPresentResult" $ \v -> CheckPresentResult
		<$> v .: "present"

instance ToJSON RemoveResult where
	toJSON (RemoveResult b) = object
		["removed" .= b]

instance FromJSON RemoveResult where
	parseJSON = withObject "RemoveResult" $ \v -> RemoveResult
		<$> v .: "removed"

instance ToJSON RemoveResultPlus where
	toJSON (RemoveResultPlus b us) = object
		[ "removed" .= b
		, "plusuuids" .= plusList us
		]

instance FromJSON RemoveResultPlus where
	parseJSON = withObject "RemoveResultPlus" $ \v -> RemoveResultPlus
		<$> v .: "removed"
		<*> v .: "plusuuids"

instance ToJSON GetTimestampResult where
	toJSON (GetTimestampResult (Timestamp (MonotonicTimestamp t))) = object
		["timestamp" .= t]

instance FromJSON GetTimestampResult where
	parseJSON = withObject "GetTimestampResult" $ \v ->
		GetTimestampResult . Timestamp . MonotonicTimestamp
			<$> v .: "timestamp"

instance ToJSON PutOffsetResult where
	toJSON (PutOffsetResult (Offset (P2P.Offset o))) = object
		["offset" .= o]

instance FromJSON PutOffsetResult where
	parseJSON = withObject "PutOffsetResult" $ \v ->
		PutOffsetResult
			<$> (Offset . P2P.Offset <$> v .: "offset")

instance ToJSON PutOffsetResultPlus where
	toJSON (PutOffsetResultPlus (Offset (P2P.Offset o)) us) = object
		[ "offset" .= o
		, "plusuuids" .= plusList us
		]

instance FromJSON PutOffsetResultPlus where
	parseJSON = withObject "PutOffsetResultPlus" $ \v ->
		PutOffsetResultPlus
			<$> (Offset . P2P.Offset <$> v .: "offset")
			<*> v .: "plusuuids"

instance FromJSON (B64UUID t) where
	parseJSON (String t) = case fromB64Maybe (TE.encodeUtf8 t) of
		Just s -> pure (B64UUID (toUUID s))
		_ -> mempty
	parseJSON _ = mempty

plusList :: [B64UUID Plus] -> [String]
plusList = map (\(B64UUID u) -> fromUUID u)

class PlusClass plus unplus where
	dePlus :: plus -> unplus
	plus :: unplus -> plus

instance PlusClass RemoveResultPlus RemoveResult where
	dePlus (RemoveResultPlus b _) = RemoveResult b
	plus (RemoveResult b) = RemoveResultPlus b mempty

instance PlusClass PutResultPlus PutResult where
	dePlus (PutResultPlus b _) = PutResult b
	plus (PutResult b) = PutResultPlus b mempty

instance PlusClass PutOffsetResultPlus PutOffsetResult where
	dePlus (PutOffsetResultPlus o _) = PutOffsetResult o
	plus (PutOffsetResult o) = PutOffsetResultPlus o mempty
