{- protocol used by "git-annex transferrer"
 -
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Transferrer where

import Annex.Common
import Types.Messages
import qualified Utility.SimpleProtocol as Proto
import Utility.Format
import Utility.Metered (TotalSize(..))

import Data.Char
import qualified Data.ByteString.Lazy as L

-- Sent to start a transfer.
data TransferRequest
	= UploadRequest TransferRemote Key TransferAssociatedFile
	| DownloadRequest TransferRemote Key TransferAssociatedFile
	| AssistantUploadRequest TransferRemote Key TransferAssociatedFile
	| AssistantDownloadRequest TransferRemote Key TransferAssociatedFile
	deriving (Show)

transferRequestRemote :: TransferRequest -> TransferRemote
transferRequestRemote (UploadRequest r _ _) = r
transferRequestRemote (DownloadRequest r _ _) = r
transferRequestRemote (AssistantUploadRequest r _ _) = r
transferRequestRemote (AssistantDownloadRequest r _ _) = r

data TransferRemote
	= TransferRemoteUUID UUID
	| TransferRemoteName RemoteName
	deriving (Show, Eq)

newtype TransferAssociatedFile = TransferAssociatedFile AssociatedFile
	deriving (Show)

data TransferResponse
	= TransferOutput SerializedOutput
	-- ^ any number may be sent before TransferResult
	| TransferResult Bool
	deriving (Show)

data TransferSerializedOutputResponse = TransferSerializedOutputResponse SerializedOutputResponse
	deriving (Show)

instance Proto.Sendable TransferRequest where
	formatMessage (UploadRequest r kd af) =
		[ "u"
		, Proto.serialize r
		, Proto.serialize kd
		, Proto.serialize af
		]
	formatMessage (DownloadRequest r kd af) =
		[ "d"
		, Proto.serialize r
		, Proto.serialize kd
		, Proto.serialize af
		]
	formatMessage (AssistantUploadRequest r kd af) =
		[ "au"
		, Proto.serialize r
		, Proto.serialize kd
		, Proto.serialize af
		]
	formatMessage (AssistantDownloadRequest r kd af) =
		[ "ad"
		, Proto.serialize r
		, Proto.serialize kd
		, Proto.serialize af
		]

instance Proto.Receivable TransferRequest where
	parseCommand "u" = Proto.parse3 UploadRequest
	parseCommand "d" = Proto.parse3 DownloadRequest
	parseCommand "au" = Proto.parse3 AssistantUploadRequest
	parseCommand "ad" = Proto.parse3 AssistantDownloadRequest
	parseCommand _ = Proto.parseFail

instance Proto.Sendable TransferResponse where
	formatMessage (TransferOutput (OutputMessage m)) =
		["om", Proto.serialize (decodeBS (encode_c isUtf8Byte m))]
	formatMessage (TransferOutput (OutputError e)) =
		["oe", Proto.serialize (decodeBS (encode_c isUtf8Byte (encodeBS e)))]
	formatMessage (TransferOutput BeginProgressMeter) =
		["opb"]
	formatMessage (TransferOutput (UpdateProgressMeterTotalSize (TotalSize sz))) =
		["ops", Proto.serialize sz]
	formatMessage (TransferOutput (UpdateProgressMeter n)) =
		["op", Proto.serialize n]
	formatMessage (TransferOutput EndProgressMeter) =
		["ope"]
	formatMessage (TransferOutput BeginPrompt) =
		["oprb"]
	formatMessage (TransferOutput EndPrompt) =
		["opre"]
	formatMessage (TransferOutput (JSONObject b)) =
		["oj", Proto.serialize (decodeBS (encode_c isUtf8Byte (L.toStrict b)))]
	formatMessage (TransferResult True) =
		["t"]
	formatMessage (TransferResult False) =
		["f"]

instance Proto.Receivable TransferResponse where
	parseCommand "om" = Proto.parse1 $
		TransferOutput . OutputMessage . decode_c . encodeBS
	parseCommand "oe" = Proto.parse1 $
		TransferOutput . OutputError . decodeBS . decode_c . encodeBS
	parseCommand "opb" = Proto.parse0 $
		TransferOutput BeginProgressMeter
	parseCommand "ops" = Proto.parse1 $
		TransferOutput . UpdateProgressMeterTotalSize . TotalSize
	parseCommand "op" = Proto.parse1 $
		TransferOutput . UpdateProgressMeter
	parseCommand "ope" = Proto.parse0 $
		TransferOutput EndProgressMeter
	parseCommand "oprb" = Proto.parse0 $
		TransferOutput BeginPrompt
	parseCommand "opre" = Proto.parse0 $
		TransferOutput EndPrompt
	parseCommand "oj" = Proto.parse1 $
		TransferOutput . JSONObject . L.fromStrict . decode_c . encodeBS
	parseCommand "t" = Proto.parse0 $
		TransferResult True
	parseCommand "f" = Proto.parse0 $
		TransferResult False
	parseCommand _ = Proto.parseFail

instance Proto.Sendable TransferSerializedOutputResponse where
	formatMessage (TransferSerializedOutputResponse ReadyPrompt) = ["opr"]

instance Proto.Receivable TransferSerializedOutputResponse where
	parseCommand "opr" = Proto.parse0 (TransferSerializedOutputResponse ReadyPrompt)
	parseCommand _ = Proto.parseFail

instance Proto.Serializable TransferRemote where
	serialize (TransferRemoteUUID u) = 'u':fromUUID u
	-- A remote name could contain whitespace or newlines, which needs
	-- to be escaped for the protocol. Use C-style encoding.
	serialize (TransferRemoteName r) = 'r':decodeBS (encode_c is_space_or_unicode (encodeBS r))
	  where
		is_space_or_unicode c = isUtf8Byte c || isSpace (chr (fromIntegral c))

	deserialize ('u':u) = Just (TransferRemoteUUID (toUUID u))
	deserialize ('r':r) = Just (TransferRemoteName (decodeBS (decode_c (encodeBS r))))
	deserialize _ = Nothing

instance Proto.Serializable TransferAssociatedFile where
	-- Comes last, so whitespace is ok. But, in case the filename
	-- contains eg a newline, escape it. Use C-style encoding.
	serialize (TransferAssociatedFile (AssociatedFile (Just f))) =
		fromRawFilePath (encode_c isUtf8Byte (fromOsPath f))
	serialize (TransferAssociatedFile (AssociatedFile Nothing)) = ""

	deserialize "" = Just $ TransferAssociatedFile $
		AssociatedFile Nothing
	deserialize s = Just $ TransferAssociatedFile $
		AssociatedFile $ Just $ toOsPath $ decode_c $ toRawFilePath s
