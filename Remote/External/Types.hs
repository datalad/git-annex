{- External special remote data types.
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Remote.External.Types (
	External(..),
	newExternal,
	ExternalType,
	ExternalState(..),
	PrepareStatus(..),
	Proto.parseMessage,
	Proto.Sendable(..),
	Proto.Receivable(..),
	Request(..),
	SafeKey,
	mkSafeKey,
	needsPREPARE,
	Response(..),
	RemoteRequest(..),
	RemoteResponse(..),
	AsyncMessage(..),
	ErrorMsg,
	Setting,
	ProtocolVersion,
	supportedProtocolVersions,
) where

import Annex.Common
import Types.StandardGroups (PreferredContentExpression)
import Utility.Metered (BytesProcessed(..))
import Types.Transfer (Direction(..))
import Config.Cost (Cost)
import Types.Remote (RemoteConfig)
import Types.Export
import Types.Availability (Availability(..))
import Types.Key
import Utility.Url (URLString)
import qualified Utility.SimpleProtocol as Proto

import Control.Concurrent.STM
import Network.URI
import Data.Char

data External = External
	{ externalType :: ExternalType
	, externalUUID :: UUID
	, externalState :: TVar [ExternalState]
	-- ^ Contains states for external special remote processes
	-- that are not currently in use.
	, externalLastPid :: TVar PID
	, externalDefaultConfig :: RemoteConfig
	, externalGitConfig :: RemoteGitConfig
	}

newExternal :: ExternalType -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex External
newExternal externaltype u c gc = liftIO $ External
	<$> pure externaltype
	<*> pure u
	<*> atomically (newTVar [])
	<*> atomically (newTVar 0)
	<*> pure c
	<*> pure gc

type ExternalType = String

data ExternalState = ExternalState
	{ externalSend :: Handle
	, externalReceive :: Handle
	, externalShutdown :: IO ()
	, externalPid :: PID
	, externalPrepared :: TVar PrepareStatus
	, externalConfig :: TVar RemoteConfig
	}

type PID = Int

data PrepareStatus = Unprepared | Prepared | FailedPrepare ErrorMsg

-- The protocol does not support keys with spaces in their names;
-- SafeKey can only be constructed for keys that are safe to use with the
-- protocol.
newtype SafeKey = SafeKey Key
	deriving (Show)

mkSafeKey :: Key -> Either String SafeKey
mkSafeKey k 
	| any isSpace (keyName k) = Left $ concat
		[ "Sorry, this file cannot be stored on an external special remote because its key's name contains a space. "
		, "To avoid this problem, you can run: git-annex migrate --backend="
		, formatKeyVariety (keyVariety k)
		, " and pass it the name of the file"
		]
	| otherwise = Right (SafeKey k)

fromSafeKey :: SafeKey -> Key
fromSafeKey (SafeKey k) = k

instance Proto.Serializable SafeKey where
	serialize = Proto.serialize . fromSafeKey
	deserialize = fmap SafeKey . Proto.deserialize

-- Messages that can be sent to the external remote to request it do something.
data Request 
	= PREPARE 
	| INITREMOTE
	| GETCOST
	| GETAVAILABILITY
	| CLAIMURL URLString
	| CHECKURL URLString
	| TRANSFER Direction SafeKey FilePath
	| CHECKPRESENT SafeKey
	| REMOVE SafeKey
	| WHEREIS SafeKey
	| EXPORTSUPPORTED
	| EXPORT ExportLocation
	| TRANSFEREXPORT Direction SafeKey FilePath
	| CHECKPRESENTEXPORT SafeKey
	| REMOVEEXPORT SafeKey
	| REMOVEEXPORTDIRECTORY ExportDirectory
	| RENAMEEXPORT SafeKey ExportLocation
	deriving (Show)

-- Does PREPARE need to have been sent before this request?
needsPREPARE :: Request -> Bool
needsPREPARE PREPARE = False
needsPREPARE INITREMOTE = False
needsPREPARE EXPORTSUPPORTED = False
needsPREPARE _ = True

instance Proto.Sendable Request where
	formatMessage PREPARE = ["PREPARE"]
	formatMessage INITREMOTE = ["INITREMOTE"]
	formatMessage GETCOST = ["GETCOST"]
	formatMessage GETAVAILABILITY = ["GETAVAILABILITY"]
	formatMessage (CLAIMURL url) = [ "CLAIMURL", Proto.serialize url ]
	formatMessage (CHECKURL url) = [ "CHECKURL", Proto.serialize url ]
	formatMessage (TRANSFER direction key file) =
		[ "TRANSFER"
		, Proto.serialize direction
		, Proto.serialize key
		, Proto.serialize file
		]
	formatMessage (CHECKPRESENT key) =
		[ "CHECKPRESENT", Proto.serialize key ]
	formatMessage (REMOVE key) = [ "REMOVE", Proto.serialize key ]
	formatMessage (WHEREIS key) = [ "WHEREIS", Proto.serialize key ]
	formatMessage EXPORTSUPPORTED = ["EXPORTSUPPORTED"]
	formatMessage (EXPORT loc) = [ "EXPORT", Proto.serialize loc ]
	formatMessage (TRANSFEREXPORT direction key file) = 
		[ "TRANSFEREXPORT"
		, Proto.serialize direction
		, Proto.serialize key
		, Proto.serialize file
		]
	formatMessage (CHECKPRESENTEXPORT key) =
		[ "CHECKPRESENTEXPORT", Proto.serialize key ]
	formatMessage (REMOVEEXPORT key) =
		[ "REMOVEEXPORT", Proto.serialize key ]
	formatMessage (REMOVEEXPORTDIRECTORY dir) =
		[ "REMOVEEXPORTDIRECTORY", Proto.serialize dir ]
	formatMessage (RENAMEEXPORT key newloc) =
		[ "RENAMEEXPORT"
		, Proto.serialize key
		, Proto.serialize newloc
		]

-- Responses the external remote can make to requests.
data Response
	= PREPARE_SUCCESS
	| PREPARE_FAILURE ErrorMsg
	| TRANSFER_SUCCESS Direction Key
	| TRANSFER_FAILURE Direction Key ErrorMsg
	| CHECKPRESENT_SUCCESS Key
	| CHECKPRESENT_FAILURE Key
	| CHECKPRESENT_UNKNOWN Key ErrorMsg
	| REMOVE_SUCCESS Key
	| REMOVE_FAILURE Key ErrorMsg
	| COST Cost
	| AVAILABILITY Availability
	| INITREMOTE_SUCCESS
	| INITREMOTE_FAILURE ErrorMsg
	| CLAIMURL_SUCCESS
	| CLAIMURL_FAILURE
	| CHECKURL_CONTENTS Size FilePath
	| CHECKURL_MULTI [(URLString, Size, FilePath)]
	| CHECKURL_FAILURE ErrorMsg
	| WHEREIS_SUCCESS String
	| WHEREIS_FAILURE
	| EXPORTSUPPORTED_SUCCESS
	| EXPORTSUPPORTED_FAILURE
	| REMOVEEXPORTDIRECTORY_SUCCESS
	| REMOVEEXPORTDIRECTORY_FAILURE
	| RENAMEEXPORT_SUCCESS Key
	| RENAMEEXPORT_FAILURE Key
	| UNSUPPORTED_REQUEST
	deriving (Show)

instance Proto.Receivable Response where
	parseCommand "PREPARE-SUCCESS" = Proto.parse0 PREPARE_SUCCESS
	parseCommand "PREPARE-FAILURE" = Proto.parse1 PREPARE_FAILURE
	parseCommand "TRANSFER-SUCCESS" = Proto.parse2 TRANSFER_SUCCESS
	parseCommand "TRANSFER-FAILURE" = Proto.parse3 TRANSFER_FAILURE
	parseCommand "CHECKPRESENT-SUCCESS" = Proto.parse1 CHECKPRESENT_SUCCESS
	parseCommand "CHECKPRESENT-FAILURE" = Proto.parse1 CHECKPRESENT_FAILURE
	parseCommand "CHECKPRESENT-UNKNOWN" = Proto.parse2 CHECKPRESENT_UNKNOWN
	parseCommand "REMOVE-SUCCESS" = Proto.parse1 REMOVE_SUCCESS
	parseCommand "REMOVE-FAILURE" = Proto.parse2 REMOVE_FAILURE
	parseCommand "COST" = Proto.parse1 COST
	parseCommand "AVAILABILITY" = Proto.parse1 AVAILABILITY
	parseCommand "INITREMOTE-SUCCESS" = Proto.parse0 INITREMOTE_SUCCESS
	parseCommand "INITREMOTE-FAILURE" = Proto.parse1 INITREMOTE_FAILURE
	parseCommand "CLAIMURL-SUCCESS" = Proto.parse0 CLAIMURL_SUCCESS
	parseCommand "CLAIMURL-FAILURE" = Proto.parse0 CLAIMURL_FAILURE
	parseCommand "CHECKURL-CONTENTS" = Proto.parse2 CHECKURL_CONTENTS
	parseCommand "CHECKURL-MULTI" = Proto.parse1 CHECKURL_MULTI
	parseCommand "CHECKURL-FAILURE" = Proto.parse1 CHECKURL_FAILURE
	parseCommand "WHEREIS-SUCCESS" = Just . WHEREIS_SUCCESS
	parseCommand "WHEREIS-FAILURE" = Proto.parse0 WHEREIS_FAILURE
	parseCommand "EXPORTSUPPORTED-SUCCESS" = Proto.parse0 EXPORTSUPPORTED_SUCCESS
	parseCommand "EXPORTSUPPORTED-FAILURE" = Proto.parse0 EXPORTSUPPORTED_FAILURE
	parseCommand "REMOVEEXPORTDIRECTORY-SUCCESS" = Proto.parse0 REMOVEEXPORTDIRECTORY_SUCCESS
	parseCommand "REMOVEEXPORTDIRECTORY-FAILURE" = Proto.parse0 REMOVEEXPORTDIRECTORY_FAILURE
	parseCommand "RENAMEEXPORT-SUCCESS" = Proto.parse1 RENAMEEXPORT_SUCCESS
	parseCommand "RENAMEEXPORT-FAILURE" = Proto.parse1 RENAMEEXPORT_FAILURE
	parseCommand "UNSUPPORTED-REQUEST" = Proto.parse0 UNSUPPORTED_REQUEST
	parseCommand _ = Proto.parseFail

-- Requests that the external remote can send at any time it's in control.
data RemoteRequest
	= VERSION ProtocolVersion
	| PROGRESS BytesProcessed
	| DIRHASH Key
	| DIRHASH_LOWER Key
	| SETCONFIG Setting String
	| GETCONFIG Setting
	| SETCREDS Setting String String
	| GETCREDS Setting
	| GETUUID
	| GETGITDIR
	| SETWANTED PreferredContentExpression
	| GETWANTED
	| SETSTATE Key String
	| GETSTATE Key
	| SETURLPRESENT Key URLString
	| SETURLMISSING Key URLString
	| SETURIPRESENT Key URI
	| SETURIMISSING Key URI
	| GETURLS Key String
	| DEBUG String
	deriving (Show)

instance Proto.Receivable RemoteRequest where
	parseCommand "VERSION" = Proto.parse1 VERSION
	parseCommand "PROGRESS" = Proto.parse1 PROGRESS
	parseCommand "DIRHASH" = Proto.parse1 DIRHASH
	parseCommand "DIRHASH-LOWER" = Proto.parse1 DIRHASH_LOWER
	parseCommand "SETCONFIG" = Proto.parse2 SETCONFIG
	parseCommand "GETCONFIG" = Proto.parse1 GETCONFIG
	parseCommand "SETCREDS" = Proto.parse3 SETCREDS
	parseCommand "GETCREDS" = Proto.parse1 GETCREDS
	parseCommand "GETUUID" = Proto.parse0 GETUUID
	parseCommand "GETGITDIR" = Proto.parse0 GETGITDIR
	parseCommand "SETWANTED" = Proto.parse1 SETWANTED
	parseCommand "GETWANTED" = Proto.parse0 GETWANTED
	parseCommand "SETSTATE" = Proto.parse2 SETSTATE
	parseCommand "GETSTATE" = Proto.parse1 GETSTATE
	parseCommand "SETURLPRESENT" = Proto.parse2 SETURLPRESENT
	parseCommand "SETURLMISSING" = Proto.parse2 SETURLMISSING
	parseCommand "SETURIPRESENT" = Proto.parse2 SETURIPRESENT
	parseCommand "SETURIMISSING" = Proto.parse2 SETURIMISSING
	parseCommand "GETURLS" = Proto.parse2 GETURLS
	parseCommand "DEBUG" = Proto.parse1 DEBUG
	parseCommand _ = Proto.parseFail

-- Responses to RemoteRequest.
data RemoteResponse
	= VALUE String
	| CREDS String String
	deriving (Show)

instance Proto.Sendable RemoteResponse where
	formatMessage (VALUE s) = [ "VALUE", Proto.serialize s ]
	formatMessage (CREDS login password) = [ "CREDS", Proto.serialize login, Proto.serialize password ]

-- Messages that can be sent at any time by either git-annex or the remote.
data AsyncMessage
	= ERROR ErrorMsg
	deriving (Show)

instance Proto.Sendable AsyncMessage where
	formatMessage (ERROR err) = [ "ERROR", Proto.serialize err ]

instance Proto.Receivable AsyncMessage where
	parseCommand "ERROR" = Proto.parse1 ERROR
	parseCommand _ = Proto.parseFail

-- Data types used for parameters when communicating with the remote.
-- All are serializable.
type ErrorMsg = String
type Setting = String
type ProtocolVersion = Int
type Size = Maybe Integer

supportedProtocolVersions :: [ProtocolVersion]
supportedProtocolVersions = [1]

instance Proto.Serializable Direction where
	serialize Upload = "STORE"
	serialize Download = "RETRIEVE"

	deserialize "STORE" = Just Upload
	deserialize "RETRIEVE" = Just Download
	deserialize _ = Nothing

instance Proto.Serializable ProtocolVersion where
	serialize = show
	deserialize = readish

instance Proto.Serializable Cost where
	serialize = show
	deserialize = readish

instance Proto.Serializable Size where
	serialize (Just s) = show s
	serialize Nothing = "UNKNOWN"
	deserialize "UNKNOWN" = Just Nothing
	deserialize s = maybe Nothing (Just . Just) (readish s)

instance Proto.Serializable Availability where
	serialize GloballyAvailable = "GLOBAL"
	serialize LocallyAvailable = "LOCAL"

	deserialize "GLOBAL" = Just GloballyAvailable
	deserialize "LOCAL" = Just LocallyAvailable
	deserialize _ = Nothing

instance Proto.Serializable BytesProcessed where
	serialize (BytesProcessed n) = show n
	deserialize = BytesProcessed <$$> readish

instance Proto.Serializable [(URLString, Size, FilePath)] where
	serialize = unwords . map go
	  where
		go (url, sz, f) = url ++ " " ++ maybe "UNKNOWN" show sz ++ " " ++ f
	deserialize = Just . go [] . words
	  where
		go c (url:sz:f:rest) = go ((url, readish sz, f):c) rest
		go c _ = reverse c

instance Proto.Serializable URI where
	serialize = show
	deserialize = parseURI

instance Proto.Serializable ExportLocation where
	serialize = fromExportLocation
	deserialize = Just . mkExportLocation

instance Proto.Serializable ExportDirectory where
	serialize = fromExportDirectory
	deserialize = Just . mkExportDirectory
