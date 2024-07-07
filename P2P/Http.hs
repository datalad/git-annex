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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module P2P.Http where

import Annex.Common
import qualified P2P.Protocol as P2P
import Utility.Base64
import Utility.MonotonicClock

import Servant
import qualified Servant.Types.SourceT as S
import Servant.API.WebSocket
import qualified Network.WebSockets as Websocket
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import Text.Read
import Data.Aeson hiding (Key)

type P2PHttpAPI
	= "git-annex" :> "key" :> CaptureKey :> GetAPI '[]
	:<|> "git-annex" :> "v3" :> "key" :> CaptureKey
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
	:<|> "git-annex" :> "v3" :> "lockcontent" :> LockContentAPI
	:<|> "git-annex" :> "v2" :> "lockcontent" :> LockContentAPI
	:<|> "git-annex" :> "v1" :> "lockcontent" :> LockContentAPI
	:<|> "git-annex" :> "v0" :> "lockcontent" :> LockContentAPI
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

p2pHttpAPI :: Proxy P2PHttpAPI
p2pHttpAPI = Proxy

p2pHttp :: Application
p2pHttp = serve p2pHttpAPI serveP2pHttp

serveP2pHttp :: Server P2PHttpAPI
serveP2pHttp
	= serveGet0
	:<|> serveGet
	:<|> serveGet
	:<|> serveGet
	:<|> serveGet0
	:<|> serveCheckPresent
	:<|> serveCheckPresent
	:<|> serveCheckPresent
	:<|> serveCheckPresent
	:<|> serveLockContent
	:<|> serveLockContent
	:<|> serveLockContent
	:<|> serveLockContent
	:<|> serveRemove id
	:<|> serveRemove id
	:<|> serveRemove dePlus
	:<|> serveRemove dePlus
	:<|> serveRemoveBefore
	:<|> serveGetTimestamp
	:<|> servePut id
	:<|> servePut id
	:<|> servePut dePlus
	:<|> servePut0
	:<|> servePutOffset id
	:<|> servePutOffset id
	:<|> servePutOffset dePlus

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

servePut0
	:: B64Key
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> Maybe B64FilePath
	-> Maybe Offset
	-> DataLength
	-> S.SourceT IO B.ByteString
	-> Handler PutResult
servePut0 = undefined

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

type ClientUUID req = QueryParam' '[req] "clientuuid" (B64UUID ClientSide)

data ClientSide

type ServerUUID req = QueryParam' '[req] "serveruuid" (B64UUID ServerSide)

data ServerSide

type BypassUUIDs = QueryParams "bypass" (B64UUID Bypass)

data Bypass

type CaptureKey = Capture "key" B64Key

type KeyParam = QueryParam' '[Required] "key" B64Key

type AssociatedFileParam = QueryParam "associatedfile" B64FilePath
	
type OffsetParam = QueryParam "offset" Offset

type DataLengthHeader = Header "X-git-annex-data-length" Integer

-- Keys, UUIDs, and filenames are base64 encoded since Servant uses 
-- Text and so needs UTF-8.
newtype B64Key = B64Key Key
newtype B64UUID t = B64UUID UUID
newtype B64FilePath = B64FilePath RawFilePath

newtype DataLength = DataLength Integer

newtype CheckPresentResult = CheckPresentResult Bool

newtype RemoveResult = RemoveResult Bool

data RemoveResultPlus = RemoveResultPlus Bool [UUID]

newtype GetTimestampResult = GetTimestampResult Timestamp

newtype PutResult = PutResult Bool
	deriving (Eq, Show)

data PutResultPlus = PutResultPlus Bool [UUID]

newtype PutOffsetResult = PutOffsetResult Offset

data PutOffsetResultPlus = PutOffsetResultPlus Offset [UUID]

newtype Offset = Offset P2P.Offset

newtype Timestamp = Timestamp MonotonicTimestamp

class DePlus plus unplus where
	dePlus :: plus -> unplus

instance DePlus RemoveResultPlus RemoveResult where
	dePlus (RemoveResultPlus b _) = RemoveResult b

instance DePlus PutResultPlus PutResult where
	dePlus (PutResultPlus b _) = PutResult b

instance DePlus PutOffsetResultPlus PutOffsetResult where
	dePlus (PutOffsetResultPlus o _) = PutOffsetResult o

instance FromHttpApiData B64Key where
	parseUrlPiece t = case fromB64Maybe (TE.encodeUtf8 t) of
		Nothing -> Left "unable to base64 decode key"
		Just b -> maybe (Left "key parse error") (Right . B64Key)
			(deserializeKey' b)

instance FromHttpApiData (B64UUID t) where
	parseUrlPiece t = case fromB64Maybe (TE.encodeUtf8 t) of
		Nothing -> Left "unable to base64 decode UUID"
		Just b -> case toUUID b of
			u@(UUID _) -> Right (B64UUID u)
			NoUUID -> Left "empty UUID"

instance FromHttpApiData B64FilePath where
	parseUrlPiece t = case fromB64Maybe (TE.encodeUtf8 t) of
		Nothing -> Left "unable to base64 decode filename"
		Just b -> Right (B64FilePath b)

instance FromHttpApiData Offset where
	parseUrlPiece t = case readMaybe (T.unpack t) of
		Nothing -> Left "offset parse error"
		Just n -> Right (Offset (P2P.Offset n))

instance FromHttpApiData Timestamp where
	parseUrlPiece t = case readMaybe (T.unpack t) of
		Nothing -> Left "timestamp parse error"
		Just n -> Right (Timestamp (MonotonicTimestamp n))

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
		, "plusuuids" .= map (fromUUID :: UUID -> String) us
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
		, "plusuuids" .= map (fromUUID :: UUID -> String) us
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
		, "plusuuids" .= map (fromUUID :: UUID -> String) us
		]

instance FromJSON PutOffsetResultPlus where
	parseJSON = withObject "PutOffsetResultPlus" $ \v ->
		PutOffsetResultPlus
			<$> (Offset . P2P.Offset <$> v .: "offset")
			<*> v .: "plusuuids"
