{- protocol used by "git-annex transferrer"
 -
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Transferrer where

import Annex.Common
import Types.Messages
import Git.Types (RemoteName)
import qualified Utility.SimpleProtocol as Proto
import Utility.Format
import Utility.Metered (TotalSize(..))

import Data.Char

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
		["om", Proto.serialize (encode_c (decodeBS m))]
	formatMessage (TransferOutput (OutputError e)) =
		["oe", Proto.serialize (encode_c e)]
	formatMessage (TransferOutput (StartProgressMeter (Just (TotalSize n)))) =
		["ops", Proto.serialize n]
	formatMessage (TransferOutput (StartProgressMeter Nothing)) =
		["opsx"]
	formatMessage (TransferOutput (UpdateProgressMeter n)) =
		["op", Proto.serialize n]
	formatMessage (TransferOutput EndProgressMeter) =
		["ope"]
	formatMessage (TransferOutput StartPrompt) =
		["oprs"]
	formatMessage (TransferOutput EndPrompt) =
		["opre"]
	formatMessage (TransferOutput (JSONObject b)) =
		["oj", Proto.serialize (encode_c (decodeBL b))]
	formatMessage (TransferResult True) =
		["t"]
	formatMessage (TransferResult False) =
		["f"]

instance Proto.Receivable TransferResponse where
	parseCommand "om" = Proto.parse1 $
		TransferOutput . OutputMessage . encodeBS . decode_c
	parseCommand "oe" = Proto.parse1 $
		TransferOutput . OutputError . decode_c
	parseCommand "ops" = Proto.parse1 $
		TransferOutput . StartProgressMeter . Just . TotalSize
	parseCommand "opsx" = Proto.parse0 $
		TransferOutput (StartProgressMeter Nothing)
	parseCommand "op" = Proto.parse1 $
		TransferOutput . UpdateProgressMeter
	parseCommand "ope" = Proto.parse0 $
		TransferOutput EndProgressMeter
	parseCommand "oprs" = Proto.parse0 $
		TransferOutput StartPrompt
	parseCommand "opre" = Proto.parse0 $
		TransferOutput EndPrompt
	parseCommand "oj" = Proto.parse1 $
		TransferOutput . JSONObject . encodeBL . decode_c
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
	serialize (TransferRemoteName r) = 'r':encode_c' isSpace r

	deserialize ('u':u) = Just (TransferRemoteUUID (toUUID u))
	deserialize ('r':r) = Just (TransferRemoteName (decode_c r))
	deserialize _ = Nothing

instance Proto.Serializable TransferAssociatedFile where
	-- Comes last, so whitespace is ok. But, in case the filename
	-- contains eg a newline, escape it. Use C-style encoding.
	serialize (TransferAssociatedFile (AssociatedFile (Just f))) =
		encode_c (fromRawFilePath f)
	serialize (TransferAssociatedFile (AssociatedFile Nothing)) = ""

	deserialize "" = Just $ TransferAssociatedFile $
		AssociatedFile Nothing
	deserialize s = Just $ TransferAssociatedFile $
		AssociatedFile $ Just $ toRawFilePath $ decode_c s
