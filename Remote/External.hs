{- External special remote interface.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Remote.External (remote) where

import Data.Char

import Common.Annex
import Types.Remote
import Types.Key
import qualified Git
import Config
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Crypto
import Utility.Metered
import Logs.Transfer
import Config.Cost

remote :: RemoteType
remote = RemoteType {
	typename = "hook",
	enumerate = findSpecialRemotes "hooktype",
	generate = gen,
	setup = undefined
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = do
	cst <- remoteCost gc expensiveRemoteCost
	return $ Just $ encryptableRemote c
		(storeEncrypted $ getGpgEncParams (c,gc))
		retrieveEncrypted
		Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
			storeKey = store,
			retrieveKeyFile = retrieve,
			retrieveKeyFileCheap = retrieveCheap,
			removeKey = remove,
			hasKey = checkPresent r,
			hasKeyCheap = False,
			whereisKey = Nothing,
			remoteFsck = Nothing,
			repairRepo = Nothing,
			config = c,
			localpath = Nothing,
			repo = r,
			gitconfig = gc,
			readonly = False,
			globallyAvailable = False,
			remotetype = remote
		}

store :: Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store k _f _p = undefined

storeEncrypted :: [CommandParam] -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted gpgOpts (cipher, enck) k _p = undefined

retrieve :: Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
retrieve k _f d _p = undefined

retrieveCheap :: Key -> FilePath -> Annex Bool
retrieveCheap _ _ = undefined

retrieveEncrypted :: (Cipher, Key) -> Key -> FilePath -> MeterUpdate -> Annex Bool
retrieveEncrypted (cipher, enck) _ f _p = undefined

remove :: Key -> Annex Bool
remove k = undefined

checkPresent :: Git.Repo -> Key -> Annex (Either String Bool)
checkPresent r k = undefined

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
	| COST_UNKNOWN
	| INITREMOTE_SUCCESS
	| INITREMOTE_FAILURE ErrorMsg
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
	parseCommand "COST_UNKNOWN" = parse0 COST_UNKNOWN
	parseCommand "INITREMOTE-SUCCESS" = parse0 INITREMOTE_SUCCESS
	parseCommand "INITREMOTE-FAILURE" = parse1 INITREMOTE_FAILURE
	parseCommand _ = parseFail

-- Requests that the external remote can send at any time it's in control.
data RemoteRequest
	= VERSION Int
	| PROGRESS Direction Key Int
	| DIRHASH Key
	| SETCONFIG Setting String
	| GETCONFIG Setting
	| SETSTATE Key String
	| GETSTATE Key
	deriving (Show)

instance Receivable RemoteRequest where
	parseCommand "VERSION" = parse1 VERSION
	parseCommand "PROGRESS" = parse3 PROGRESS
	parseCommand "DIRHASH" = parse1 DIRHASH
	parseCommand "SETCONFIG" = parse2 SETCONFIG
	parseCommand "GETCONFIG" = parse1 GETCONFIG
	parseCommand "SETSTATE" = parse2 SETSTATE
	parseCommand "GETSTATE" = parse1 GETSTATE
	parseCommand _ = parseFail

-- Responses to RemoteRequest.
data RemoteResponse
	= VALUE String
	deriving (Show)

instance Sendable RemoteResponse where
	formatMessage (VALUE s) = [ "VALUE", serialize s ]

-- Messages that can be sent at any time by either git-annex or the remote.
data AsyncMessages 
	= ERROR ErrorMsg
	deriving (Show)

instance Sendable AsyncMessages where
	formatMessage (ERROR err) = [ "ERROR", serialize err ]

instance Receivable AsyncMessages where
	parseCommand "ERROR" = parse1 ERROR
	parseCommand _ = parseFail

-- Data types used for parameters when communicating with the remote.
-- All are serializable.
type ErrorMsg = String
type Setting = String

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

instance Serializable Cost where
	serialize = show
	deserialize = readish

instance Serializable Int where
	serialize = show
	deserialize = readish

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
