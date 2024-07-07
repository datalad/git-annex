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
{-# LANGUAGE OverloadedStrings #-}

module P2P.Http where

import Annex.Common
import qualified P2P.Protocol as P2P
import Utility.Base64
import Utility.MonotonicClock

import Servant
import Servant.API.WebSocket
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

type P2PAPI
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

type GetAPI headers
	= CommonParams Optional
	:> AssociatedFileParam
	:> OffsetParam
	:> StreamGet NoFraming OctetStream
		(Headers headers (SourceIO B.ByteString))

type CheckPresentAPI
	= KeyParam
	:> CommonParams Required
	:> Post '[JSON] CheckPresentResult
		
type LockContentAPI
	= KeyParam
	:> CommonParams Required
	:> WebSocket

type RemoveAPI result
	= KeyParam
	:> CommonParams Required
	:> Post '[JSON] result
		
type RemoveBeforeAPI
	= KeyParam
	:> CommonParams Required
	:> QueryParam' '[Required] "timestamp" MonotonicTimestamp
	:> Post '[JSON] RemoveResult
		
type GetTimestampAPI
	= CommonParams Required
	:> Post '[JSON] GetTimestampResult
		
type PutAPI result
	= KeyParam
	:> CommonParams Required
	:> AssociatedFileParam
	:> OffsetParam
	:> Header' '[Required] "X-git-annex-data-length" DataLength
	:> CommonParams Required
	:> StreamBody NoFraming OctetStream (SourceIO B.ByteString)
	:> Post '[JSON] result

type PutOffsetAPI result
	= KeyParam
	:> CommonParams Required
	:> Post '[JSON] result

type CommonParams req
	= QueryParam' '[req] "clientuuid" B64UUID 
	:> QueryParam' '[req] "serveruuid" B64UUID
	:> QueryParams "bypass" B64UUID

type CaptureKey = Capture "key" B64Key

type KeyParam = QueryParam' '[Required] "key"

type AssociatedFileParam = QueryParam "associatedfile" B64FilePath
	
type OffsetParam = QueryParam "offset" P2P.Offset

type DataLengthHeader = Header "X-git-annex-data-length" Integer

newtype DataLength = DataLength Integer

newtype CheckPresentResult = CheckPresentResult Bool

newtype RemoveResult = RemoveResult Bool

data RemoveResultPlus = RemoveResultPlus Bool [UUID]

newtype GetTimestampResult = GetTimestmapResult MonotonicTimestamp

newtype PutResult = PutResult Bool

data PutResultPlus = PutResultPlus Bool [UUID]

newtype PutOffsetResult = PutOffsetResult P2P.Offset

data PutOffsetResultPlus = PutOffsetResultPlus P2P.Offset [UUID]

-- Keys, UUIDs, and filenames are base64 encoded since Servant uses 
-- Text and so needs UTF-8.
newtype B64Key = B64Key Key
newtype B64UUID = B64UUID UUID
newtype B64FilePath = B64FilePath RawFilePath

instance FromHttpApiData B64Key where
	parseUrlPiece t = case fromB64Maybe (TE.encodeUtf8 t) of
		Nothing -> Left "unable to base64 decode key"
		Just b -> maybe (Left "key parse error") (Right . B64Key)
			(deserializeKey' b)

instance FromHttpApiData B64UUID where
	parseUrlPiece t = case fromB64Maybe (TE.encodeUtf8 t) of
		Nothing -> Left "unable to base64 decode UUID"
		Just b -> case toUUID b of
			u@(UUID _) -> Right (B64UUID u)
			NoUUID -> Left "empty UUID"

instance FromHttpApiData B64FilePath where
	parseUrlPiece t = case fromB64Maybe (TE.encodeUtf8 t) of
		Nothing -> Left "unable to base64 decode filename"
		Just b -> Right (B64FilePath b)
