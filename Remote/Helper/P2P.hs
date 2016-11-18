{- P2P protocol
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}

module Remote.Helper.P2P (
	AuthToken(..),
	ProtoF(..),
	runPure,
	protoDump,
	auth,
	remove,
	get,
	put,
	serve,
) where

import qualified Utility.SimpleProtocol as Proto
import Types.Key
import Types.UUID
import Utility.Applicative
import Utility.PartialPrelude

import Control.Monad
import Control.Monad.Free
import Control.Monad.Free.TH
import qualified Data.ByteString.Lazy as L

newtype AuthToken = AuthToken String
	deriving (Show)

newtype Offset = Offset Integer
	deriving (Show)

newtype Len = Len Integer
	deriving (Show)

-- | Messages in the protocol. The peer that makes the connection
-- always initiates requests, and the other peer makes responses to them.
data Message
	= AUTH UUID AuthToken -- uuid of the peer that is authenticating
	| AUTH_SUCCESS UUID -- uuid of the remote peer
	| AUTH_FAILURE
	| REMOVE Key
	| GET Offset Key
	| PUT Key
	| PUT_FROM Offset
	| ALREADY_HAVE
	| SUCCESS
	| FAILURE
	| DATA Len -- followed by bytes
	| PROTO_ERROR String
	deriving (Show)

-- | Free monad for implementing actions that use the protocol.
data ProtoF next
	= SendMessage Message next
	| GetMessage (Message -> next)
	| SendBytes Len L.ByteString next
	| ReceiveBytes Len (L.ByteString -> next)
	-- ^ Lazily reads bytes from peer. Stops once Len are read,
	-- or if connection is lost, and in either case returns the bytes
	-- that were read. This allows resuming interrupted transfers.
	| KeyFileSize Key (Len -> next)
	-- ^ Checks size of key file (dne = 0)
	| ReadKeyFile Key Offset (L.ByteString -> next)
	| WriteKeyFile Key Offset Len L.ByteString (Bool -> next)
	-- ^ Writes to key file starting at an offset. Returns True
	-- once the whole content of the key is stored in the key file.
	--
	-- Note: The ByteString may not contain the entire remaining content
	-- of the key. Only once the key file size == Len has the whole
	-- content been transferred.
	| CheckAuthToken UUID AuthToken (Bool -> next)
	| SetPresent Key UUID next
	| CheckPresent Key (Bool -> next)
	-- ^ Checks if the whole content of the key is locally present.
	| RemoveKeyFile Key (Bool -> next)
	-- ^ If the key file is not present, still succeeds.
	-- May fail if not enough copies to safely drop, etc.
	deriving (Functor)

type Proto = Free ProtoF

$(makeFree ''ProtoF)

-- | Running Proto actions purely, to see what they do.
runPure :: Show r => Proto r -> [Message] -> [(String, Maybe Message)]
runPure (Pure r) _ = [("result: " ++ show r, Nothing)]
runPure (Free (SendMessage m next)) ms = (">",  Just m):runPure next ms
runPure (Free (GetMessage _)) [] = [("not enough Messages provided", Nothing)]
runPure (Free (GetMessage next)) (m:ms) = ("<", Just m):runPure (next m) ms
runPure (Free (SendBytes _ _ next)) ms = ("> bytes", Nothing):runPure next ms
runPure (Free (ReceiveBytes _ next)) ms = ("< bytes", Nothing):runPure (next L.empty) ms
runPure (Free (KeyFileSize _ next)) ms = runPure (next (Len 100)) ms
runPure (Free (ReadKeyFile _ _ next)) ms = runPure (next L.empty) ms
runPure (Free (WriteKeyFile _ _ _ _ next)) ms = runPure (next True) ms
runPure (Free (CheckAuthToken _ _ next)) ms = runPure (next True) ms
runPure (Free (SetPresent _ _ next)) ms = runPure next ms
runPure (Free (CheckPresent _ next)) ms = runPure (next False) ms
runPure (Free (RemoveKeyFile _ next)) ms = runPure (next False) ms

protoDump :: [(String, Maybe Message)] -> String
protoDump = unlines . map protoDump'

protoDump' :: (String, Maybe Message) -> String
protoDump' (s, Nothing) = s
protoDump' (s, Just m) = s ++ " " ++ unwords (Proto.formatMessage m)

auth :: UUID -> AuthToken -> Proto (Maybe UUID)
auth myuuid t = do
	sendMessage (AUTH myuuid t)
	r <- getMessage
	case r of
		AUTH_SUCCESS theiruuid -> return $ Just theiruuid
		AUTH_FAILURE -> return Nothing
		_ -> do
			sendMessage (PROTO_ERROR "auth failed")
			return Nothing

remove :: Key -> Proto Bool
remove key = do
	sendMessage (REMOVE key)
	checkSuccess

get :: Key -> Proto Bool
get key = receiveContent key (`GET` key)

put :: Key -> Proto Bool
put key = do
	sendMessage (PUT key)
	r <- getMessage
	case r of
		PUT_FROM offset -> sendContent key offset
		ALREADY_HAVE -> return True
		_ -> do
			sendMessage (PROTO_ERROR "expected PUT_FROM")
			return False

-- | Serve the protocol.
--
-- Note that if the client sends an unexpected message, the server will
-- respond with PTOTO_ERROR, and always continues processing messages.
-- Since the protocol is not versioned, this is necessary to handle
-- protocol changes robustly, since the client can detect when it's
-- talking to a server that does not support some new feature, and fall
-- back.
--
-- When the client sends PROTO_ERROR to the server, the server gives up,
-- since it's not clear what state the client is is, and so not possible to
-- recover.
serve :: UUID -> Proto ()
serve myuuid = go Nothing
  where
	go autheduuid = do
		r <- getMessage
		case r of
			AUTH theiruuid authtoken -> do
				ok <- checkAuthToken theiruuid authtoken
				if ok
					then do
						sendMessage (AUTH_SUCCESS myuuid)
						go (Just theiruuid)
					else do
						sendMessage AUTH_FAILURE
						go autheduuid
			PROTO_ERROR _ -> return ()
			_ -> do
				case autheduuid of
					Just theiruuid -> authed theiruuid r
					Nothing -> sendMessage (PROTO_ERROR "must AUTH first")
				go autheduuid
	
	authed _theiruuid r = case r of
		REMOVE key -> do
			ok <- removeKeyFile key
			sendMessage $ if ok then SUCCESS else FAILURE
		PUT key -> do
			have <- checkPresent key
			if have
				then sendMessage ALREADY_HAVE
				else do
					ok <- receiveContent key PUT_FROM
					when ok $
						setPresent key myuuid
		-- setPresent not called because the peer may have
		-- requested the data but not permanatly stored it.
		GET offset key -> void $ sendContent key offset
		_ -> sendMessage (PROTO_ERROR "unexpected command")

sendContent :: Key -> Offset -> Proto Bool
sendContent key offset = do
	(len, content) <- readKeyFile' key offset
	sendMessage (DATA len)
	sendBytes len content
	checkSuccess

receiveContent :: Key -> (Offset -> Message) -> Proto Bool
receiveContent key mkmsg = do
	Len n <- keyFileSize key
	let offset = Offset n
	sendMessage (mkmsg offset)
	r <- getMessage
	case r of
		DATA len -> do
			ok <- writeKeyFile key offset len =<< receiveBytes len
			sendMessage $ if ok then SUCCESS else FAILURE
			return ok
		_ -> do
			sendMessage (PROTO_ERROR "expected DATA")
			return False

checkSuccess :: Proto Bool
checkSuccess = do
	ack <- getMessage
	case ack of
		SUCCESS -> return True
		FAILURE -> return False
		_ -> do
			sendMessage (PROTO_ERROR "expected SUCCESS or FAILURE")
			return False

-- Reads key file from an offset. The Len should correspond to
-- the length of the ByteString, but to avoid buffering the content
-- in memory, is gotten using keyFileSize.
readKeyFile' :: Key -> Offset -> Proto (Len, L.ByteString)
readKeyFile' key (Offset offset) = do
	(Len totallen) <- keyFileSize key
	let len = totallen - offset
	if len <= 0
		then return (Len 0, L.empty)
		else do
			content <- readKeyFile key (Offset offset)
			return (Len len, content)

instance Proto.Sendable Message where
	formatMessage (AUTH uuid authtoken) = ["AUTH", Proto.serialize uuid, Proto.serialize authtoken]
	formatMessage (AUTH_SUCCESS uuid) = ["AUTH-SUCCESS",  Proto.serialize uuid]
	formatMessage AUTH_FAILURE = ["AUTH-FAILURE"]
	formatMessage (REMOVE key) = ["REMOVE", Proto.serialize key]
	formatMessage (GET offset key) = ["GET", Proto.serialize offset, Proto.serialize key]
	formatMessage (PUT key) = ["PUT", Proto.serialize key]
	formatMessage (PUT_FROM offset) = ["PUT-FROM", Proto.serialize offset]
	formatMessage ALREADY_HAVE = ["ALREADY-HAVE"]
	formatMessage SUCCESS = ["SUCCESS"]
	formatMessage FAILURE = ["FAILURE"]
	formatMessage (DATA leng) = ["DATA", Proto.serialize leng]
	formatMessage (PROTO_ERROR err) = ["PROTO-ERROR", Proto.serialize err]

instance Proto.Receivable Message where
	parseCommand "AUTH" = Proto.parse2 AUTH
	parseCommand "AUTH-SUCCESS" = Proto.parse1 AUTH_SUCCESS
	parseCommand "AUTH-FAILURE" = Proto.parse0 AUTH_FAILURE
	parseCommand "REMOVE" = Proto.parse1 REMOVE
	parseCommand "GET" = Proto.parse2 GET
	parseCommand "PUT" = Proto.parse1 PUT
	parseCommand "PUT-FROM" = Proto.parse1 PUT_FROM
	parseCommand "ALREADY-HAVE" = Proto.parse0 ALREADY_HAVE
	parseCommand "SUCCESS" = Proto.parse0 SUCCESS
	parseCommand "FAILURE" = Proto.parse0 FAILURE
	parseCommand "DATA" = Proto.parse1 DATA
	parseCommand "PROTO-ERROR" = Proto.parse1 PROTO_ERROR
	parseCommand _ = Proto.parseFail

instance Proto.Serializable Offset where
	serialize (Offset n) = show n
	deserialize = Offset <$$> readish

instance Proto.Serializable Len where
	serialize (Len n) = show n
	deserialize = Len <$$> readish

instance Proto.Serializable AuthToken where
	serialize (AuthToken s) = s
	deserialize = Just . AuthToken
