{- git-annex command
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.P2PHttp where

import Command
import qualified P2P.Protocol as P2P
import Utility.Base64
import Utility.MonotonicClock

import Servant
import Servant.API.WebSocket
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

cmd :: Command
cmd = command "p2phttp" SectionPlumbing
	"communicate in P2P protocol over http"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = error "TODO"

type API
	= "git-annex" 
		:> (("key" :> CaptureKey) :<|> ("v3" :> "key" :> CaptureKey))
		:> CommonParams Optional
		:> AssociatedFileParam
		:> OffsetParam
		:> StreamGet NoFraming OctetStream (SourceIO B.ByteString)
	:<|> "git-annex" :> "v3" :> "checkpresent"
		:> KeyParam
		:> CommonParams Required
		:> Post '[JSON] CheckPresentResult
	:<|> "git-annex" :> "v3" :> "lockcontent"
		:> KeyParam
		:> CommonParams Required
		:> WebSocket
	:<|> "git-annex" :> "v3" :> "remove"
		:> KeyParam
		:> CommonParams Required
		:> Post '[JSON] RemoveResult
	:<|> "git-annex" :> "v3" :> "remove-before"
		:> KeyParam
		:> CommonParams Required
		:> QueryParam' '[Required] "timestamp" MonotonicTimestamp
		:> Post '[JSON] RemoveResult
	:<|> "git-annex" :> "v3" :> "gettimestamp"
		:> CommonParams Required
		:> Post '[JSON] GetTimestampResult
	:<|> "git-annex" :> "v3" :> "put"
		:> KeyParam
		:> AssociatedFileParam
		:> OffsetParam
		:> Header' '[Required] "X-git-annex-object-size" ObjectSize
		:> CommonParams Required
		:> StreamBody NoFraming OctetStream (SourceIO B.ByteString)
		:> Post '[JSON] PutResult

type CommonParams req
	= QueryParam' '[req] "clientuuid" B64UUID 
	:> QueryParam' '[req] "serveruuid" B64UUID
	:> QueryParams "bypass" B64UUID

type CaptureKey = Capture "key" B64Key

type KeyParam = QueryParam' '[Required] "key"

type AssociatedFileParam = QueryParam "associatedfile" B64FilePath
	
type OffsetParam = QueryParam "offset" P2P.Offset

type GetKey
	= Capture "key" B64Key
	:> CommonParams Optional
	:> AssociatedFileParam
	:> OffsetParam
	:> StreamGet NoFraming OctetStream (SourceIO B.ByteString)

newtype ObjectSize = ObjectSize Integer

newtype CheckPresentResult = CheckPresentResult Bool

newtype RemoveResult = RemoveResult Bool

newtype GetTimestampResult = GetTimestmapResult MonotonicTimestamp

newtype PutResult = PutResult Bool

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
