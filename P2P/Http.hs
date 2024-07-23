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
) where

import P2P.Http.Types

import Servant
import qualified Data.ByteString as B

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

type GetGenericAPI
	= CaptureKey
	:> IsSecure
	:> AuthHeader
	:> StreamGet NoFraming OctetStream
		(Headers '[DataLengthHeader] (SourceIO B.ByteString))

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

type CheckPresentAPI
	= KeyParam
	:> CU Required
	:> BypassUUIDs
	:> IsSecure
	:> AuthHeader
	:> Post '[JSON] CheckPresentResult

type RemoveAPI result
	= KeyParam
	:> CU Required
	:> BypassUUIDs
	:> IsSecure
	:> AuthHeader
	:> Post '[JSON] result
	
type RemoveBeforeAPI
	= KeyParam
	:> CU Required
	:> BypassUUIDs
	:> QueryParam' '[Required] "timestamp" Timestamp
	:> IsSecure
	:> AuthHeader
	:> Post '[JSON] RemoveResultPlus

type GetTimestampAPI
	= CU Required
	:> BypassUUIDs
	:> IsSecure
	:> AuthHeader
	:> Post '[JSON] GetTimestampResult

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

type PutOffsetAPI result
	= KeyParam
	:> CU Required
	:> BypassUUIDs
	:> IsSecure
	:> AuthHeader
	:> Post '[JSON] result

type LockContentAPI
	= KeyParam
	:> CU Required
	:> BypassUUIDs
	:> IsSecure
	:> AuthHeader
	:> Post '[JSON] LockResult

type KeepLockedAPI
	= LockIDParam
	:> CU Optional
	:> BypassUUIDs
	:> IsSecure
	:> AuthHeader
	:> Header "Connection" ConnectionKeepAlive
	:> Header "Keep-Alive" KeepAlive
	:> StreamBody NewlineFraming JSON (SourceIO UnlockRequest)
	:> Post '[JSON] LockResult

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

type PV3 = Capture "v3" V3
type PV2 = Capture "v2" V2
type PV1 = Capture "v1" V1
type PV0 = Capture "v0" V0

