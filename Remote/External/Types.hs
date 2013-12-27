{- External special remote data types.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Remote.External.Types (
	External(..),
	newExternal,
	ExternalType,
	ExternalLock,
	withExternalLock,
	ExternalState(..),
	parseMessage,
	Sendable(..),
	Receivable(..),
	Request(..),
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

import Common.Annex
import Annex.Exception
import Types.Key (file2key, key2file)
import Utility.Metered (BytesProcessed(..))
import Logs.Transfer (Direction(..))
import Config.Cost (Cost)
import Types.Remote (RemoteConfig)

import Data.Char
import Control.Concurrent.STM

-- If the remote is not yet running, the ExternalState TMVar is empty.
-- The 
data External = External
	{ externalType :: ExternalType
	, externalUUID :: UUID
	-- Empty until the remote is running.
	, externalState :: TMVar ExternalState
	-- Empty when a remote is in use.
	, externalLock :: TMVar ExternalLock
	-- Never left empty.
	, externalConfig :: TMVar RemoteConfig
	}

newExternal :: ExternalType -> UUID -> RemoteConfig -> Annex External
newExternal externaltype u c = liftIO $ External
	<$> pure externaltype
	<*> pure u
	<*> atomically newEmptyTMVar
	<*> atomically (newTMVar ExternalLock)
	<*> atomically (newTMVar c)

type ExternalType = String

data ExternalState = ExternalState
	{ externalSend :: Handle
	, externalReceive :: Handle
	, externalPid :: ProcessHandle
	, externalPrepared :: Bool
	}

-- Constructor is not exported, and only created by newExternal.
data ExternalLock = ExternalLock

withExternalLock :: External -> (ExternalLock -> Annex a) -> Annex a
withExternalLock external = bracketIO setup cleanup
  where
	setup = atomically $ takeTMVar v
	cleanup = atomically . putTMVar v
	v = externalLock external

-- Messages that git-annex can send.
class Sendable m where
	formatMessage :: m -> [String]

-- Messages that git-annex can receive.
class Receivable m where
	-- Passed the first word of the message, returns
	-- a Parser that can be be fed the rest of the message to generate
	-- the value.
	parseCommand :: String -> Parser m

parseMessage :: (Receivable m) => String -> Maybe m
parseMessage s = parseCommand command rest
  where
	(command, rest) = splitWord s

-- Messages that can be sent to the external remote to request it do something.
data Request 
	= PREPARE 
	| INITREMOTE
	| GETCOST
	| TRANSFER Direction Key FilePath
	| CHECKPRESENT Key
	| REMOVE Key
	deriving (Show)

-- Does PREPARE need to have been sent before this request?
needsPREPARE :: Request -> Bool
needsPREPARE PREPARE = False
needsPREPARE INITREMOTE = False
needsPREPARE _ = True

instance Sendable Request where
	formatMessage PREPARE = ["PREPARE"]
	formatMessage INITREMOTE = ["INITREMOTE"]
	formatMessage GETCOST = ["GETCOST"]
	formatMessage (TRANSFER direction key file) =
		[ "TRANSFER", serialize direction, serialize key, serialize file ]
	formatMessage (CHECKPRESENT key) = [ "CHECKPRESENT", serialize key ]
	formatMessage (REMOVE key) = [ "REMOVE", serialize key ]

-- Responses the external remote can make to requests.
data Response
	= PREPARE_SUCCESS
	| TRANSFER_SUCCESS Direction Key
	| TRANSFER_FAILURE Direction Key ErrorMsg
	| CHECKPRESENT_SUCCESS Key
	| CHECKPRESENT_FAILURE Key
	| CHECKPRESENT_UNKNOWN Key ErrorMsg
	| REMOVE_SUCCESS Key
	| REMOVE_FAILURE Key ErrorMsg
	| COST Cost
	| INITREMOTE_SUCCESS
	| INITREMOTE_FAILURE ErrorMsg
	| UNSUPPORTED_REQUEST
	deriving (Show)

instance Receivable Response where
	parseCommand "PREPARE-SUCCESS" = parse0 PREPARE_SUCCESS
	parseCommand "TRANSFER-SUCCESS" = parse2 TRANSFER_SUCCESS
	parseCommand "TRANSFER-FAILURE" = parse3 TRANSFER_FAILURE
	parseCommand "CHECKPRESENT-SUCCESS" = parse1 CHECKPRESENT_SUCCESS
	parseCommand "CHECKPRESENT-FAILURE" = parse1 CHECKPRESENT_FAILURE
	parseCommand "CHECKPRESENT-UNKNOWN" = parse2 CHECKPRESENT_UNKNOWN
	parseCommand "REMOVE-SUCCESS" = parse1 REMOVE_SUCCESS
	parseCommand "REMOVE-FAILURE" = parse2 REMOVE_FAILURE
	parseCommand "COST" = parse1 COST
	parseCommand "INITREMOTE-SUCCESS" = parse0 INITREMOTE_SUCCESS
	parseCommand "INITREMOTE-FAILURE" = parse1 INITREMOTE_FAILURE
	parseCommand "UNSUPPORTED-REQUEST" = parse0 UNSUPPORTED_REQUEST
	parseCommand _ = parseFail

-- Requests that the external remote can send at any time it's in control.
data RemoteRequest
	= VERSION ProtocolVersion
	| PROGRESS BytesProcessed
	| DIRHASH Key
	| SETCONFIG Setting String
	| GETCONFIG Setting
	| SETCREDS Setting String String
	| GETCREDS Setting
	deriving (Show)

instance Receivable RemoteRequest where
	parseCommand "VERSION" = parse1 VERSION
	parseCommand "PROGRESS" = parse1 PROGRESS
	parseCommand "DIRHASH" = parse1 DIRHASH
	parseCommand "SETCONFIG" = parse2 SETCONFIG
	parseCommand "GETCONFIG" = parse1 GETCONFIG
	parseCommand "SETCREDS" = parse3 SETCREDS
	parseCommand "GETCREDS" = parse1 GETCREDS
	parseCommand _ = parseFail

-- Responses to RemoteRequest.
data RemoteResponse
	= VALUE String
	| CREDS String String
	deriving (Show)

instance Sendable RemoteResponse where
	formatMessage (VALUE s) = [ "VALUE", serialize s ]
	formatMessage (CREDS login password) = [ "CREDS", serialize login, serialize password ]

-- Messages that can be sent at any time by either git-annex or the remote.
data AsyncMessage
	= ERROR ErrorMsg
	deriving (Show)

instance Sendable AsyncMessage where
	formatMessage (ERROR err) = [ "ERROR", serialize err ]

instance Receivable AsyncMessage where
	parseCommand "ERROR" = parse1 ERROR
	parseCommand _ = parseFail

-- Data types used for parameters when communicating with the remote.
-- All are serializable.
type ErrorMsg = String
type Setting = String
type ProtocolVersion = Int

supportedProtocolVersions :: [ProtocolVersion]
supportedProtocolVersions = [1]

class Serializable a where
	serialize :: a -> String
	deserialize :: String -> Maybe a

instance Serializable Direction where
	serialize Upload = "STORE"
	serialize Download = "RETRIEVE"

	deserialize "STORE" = Just Upload
	deserialize "RETRIEVE" = Just Download
	deserialize _ = Nothing

instance Serializable Key where
	serialize = key2file
	deserialize = file2key

instance Serializable [Char] where
	serialize = id
	deserialize = Just

instance Serializable ProtocolVersion where
	serialize = show
	deserialize = readish

instance Serializable Cost where
	serialize = show
	deserialize = readish

instance Serializable BytesProcessed where
	serialize (BytesProcessed n) = show n
	deserialize = BytesProcessed <$$> readish

{- Parsing the parameters of messages. Using the right parseN ensures
 - that the string is split into exactly the requested number of words,
 - which allows the last parameter of a message to contain arbitrary
 - whitespace, etc, without needing any special quoting.
 -}
type Parser a = String -> Maybe a

parseFail :: Parser a
parseFail _ = Nothing

parse0 :: a -> Parser a
parse0 mk "" = Just mk
parse0 _ _ = Nothing

parse1 :: Serializable p1 => (p1 -> a) -> Parser a
parse1 mk p1 = mk <$> deserialize p1

parse2 :: (Serializable p1, Serializable p2) => (p1 -> p2 -> a) -> Parser a
parse2 mk s = mk <$> deserialize p1 <*> deserialize p2
  where
	(p1, p2) = splitWord s

parse3 :: (Serializable p1, Serializable p2, Serializable p3) => (p1 -> p2 -> p3 -> a) -> Parser a
parse3 mk s = mk <$> deserialize p1 <*> deserialize p2 <*> deserialize p3
  where
	(p1, rest) = splitWord s
	(p2, p3) = splitWord rest

splitWord :: String -> (String, String)
splitWord = separate isSpace
