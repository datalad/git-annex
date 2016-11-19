{- P2P protocol
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts, RankNTypes #-}

module Remote.Helper.P2P (
	AuthToken(..),
	ProtoF(..),
	runPure,
	protoDump,
	auth,
	checkPresent,
	lockContentWhile,
	remove,
	get,
	put,
	connect,
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
import Control.Monad.Catch
import System.Exit (ExitCode(..))
import System.IO (Handle)
import qualified Data.ByteString.Lazy as L

newtype AuthToken = AuthToken String
	deriving (Show)

newtype Offset = Offset Integer
	deriving (Show)

newtype Len = Len Integer
	deriving (Show)

-- | Service as used by the connect message is gitremote-helpers(1)
data Service = UploadPack | ReceivePack
	deriving (Show)

data RelayData
	= RelayData L.ByteString
	| RelayMessage Message

-- | Messages in the protocol. The peer that makes the connection
-- always initiates requests, and the other peer makes responses to them.
data Message
	= AUTH UUID AuthToken -- uuid of the peer that is authenticating
	| AUTH_SUCCESS UUID -- uuid of the remote peer
	| AUTH_FAILURE
	| CONNECT Service
	| CONNECTDONE ExitCode
	| CHECKPRESENT Key
	| LOCKCONTENT Key
	| UNLOCKCONTENT
	| REMOVE Key
	| GET Offset Key
	| PUT Key
	| PUT_FROM Offset
	| ALREADY_HAVE
	| SUCCESS
	| FAILURE
	| DATA Len -- followed by bytes of data
	| ERROR String
	deriving (Show)

-- | Free monad for implementing actions that use the protocol.
data ProtoF next
	= SendMessage Message next
	| ReceiveMessage (Message -> next)
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
	| CheckContentPresent Key (Bool -> next)
	-- ^ Checks if the whole content of the key is locally present.
	| RemoveKeyFile Key (Bool -> next)
	-- ^ If the key file is not present, still succeeds.
	-- May fail if not enough copies to safely drop, etc.
	| TryLockContent Key (Bool -> Proto ()) next
	| WriteHandle Handle L.ByteString next
	-- ^ Try to lock the content of a key,  preventing it
	-- from being deleted, and run the provided protocol action.
	| Relay Handle (RelayData -> Proto (Maybe ExitCode)) (ExitCode -> next)
	-- ^ Waits for data to be written to the Handle, and for messages
	-- to be received from the peer, and passes the data to the
	-- callback, continuing until it returns an ExitCode.
	| RelayService Service
		(Handle -> RelayData -> Proto (Maybe ExitCode))
		(ExitCode -> next)
	-- ^ Runs a service, and waits for it to output to stdout,
	-- and for messages to be received from the peer, and passes
	-- the data to the callback (which is also passed the service's
	-- stdin Handle), continuing uniil the service exits.
	deriving (Functor)

type Proto = Free ProtoF

$(makeFree ''ProtoF)

-- | Running Proto actions purely, to see what they do.
runPure :: Show r => Proto r -> [Message] -> [(String, Maybe Message)]
runPure (Pure r) _ = [("result: " ++ show r, Nothing)]
runPure (Free (SendMessage m next)) ms = (">",  Just m):runPure next ms
runPure (Free (ReceiveMessage _)) [] = [("not enough Messages provided", Nothing)]
runPure (Free (ReceiveMessage next)) (m:ms) = ("<", Just m):runPure (next m) ms
runPure (Free (SendBytes _ _ next)) ms = ("> bytes", Nothing):runPure next ms
runPure (Free (ReceiveBytes _ next)) ms = ("< bytes", Nothing):runPure (next L.empty) ms
runPure (Free (KeyFileSize _ next)) ms = runPure (next (Len 100)) ms
runPure (Free (ReadKeyFile _ _ next)) ms = runPure (next L.empty) ms
runPure (Free (WriteKeyFile _ _ _ _ next)) ms = runPure (next True) ms
runPure (Free (CheckAuthToken _ _ next)) ms = runPure (next True) ms
runPure (Free (SetPresent _ _ next)) ms = runPure next ms
runPure (Free (CheckContentPresent _ next)) ms = runPure (next False) ms
runPure (Free (RemoveKeyFile _ next)) ms = runPure (next True) ms
runPure (Free (TryLockContent _ p next)) ms = runPure (p True >> next) ms
runPure (Free (WriteHandle _ _ next)) ms = runPure next ms
runPure (Free (Relay _ _ next)) ms = runPure (next ExitSuccess) ms
runPure (Free (RelayService _ _ next)) ms = runPure (next ExitSuccess) ms

protoDump :: [(String, Maybe Message)] -> String
protoDump = unlines . map protoDump'

protoDump' :: (String, Maybe Message) -> String
protoDump' (s, Nothing) = s
protoDump' (s, Just m) = s ++ " " ++ unwords (Proto.formatMessage m)

auth :: UUID -> AuthToken -> Proto (Maybe UUID)
auth myuuid t = do
	sendMessage (AUTH myuuid t)
	r <- receiveMessage
	case r of
		AUTH_SUCCESS theiruuid -> return $ Just theiruuid
		AUTH_FAILURE -> return Nothing
		_ -> do
			sendMessage (ERROR "auth failed")
			return Nothing

checkPresent :: Key -> Proto Bool
checkPresent key = do
	sendMessage (CHECKPRESENT key)
	checkSuccess

{- Locks content to prevent it from being dropped, while running an action.
 -
 - Note that this only guarantees that the content is locked as long as the
 - connection to the peer remains up. If the connection is unexpectededly
 - dropped, the peer will then unlock the content.
 -}
lockContentWhile 
	:: MonadMask m 
	=> (forall r. Proto r -> m r)
	-> Key
	-> (Bool -> m ())
	-> m ()
lockContentWhile runproto key a = bracket setup cleanup a
  where
	setup = runproto $ do
		sendMessage (LOCKCONTENT key)
		checkSuccess
	cleanup True = runproto $ sendMessage UNLOCKCONTENT
	cleanup False = return ()

remove :: Key -> Proto Bool
remove key = do
	sendMessage (REMOVE key)
	checkSuccess

get :: Key -> Proto Bool
get key = receiveContent key (`GET` key)

put :: Key -> Proto Bool
put key = do
	sendMessage (PUT key)
	r <- receiveMessage
	case r of
		PUT_FROM offset -> sendContent key offset
		ALREADY_HAVE -> return True
		_ -> do
			sendMessage (ERROR "expected PUT_FROM")
			return False

connect :: Service -> Handle -> Handle -> Proto ExitCode
connect service hin hout = do
	sendMessage (CONNECT service)
	relay hin (relayCallback hout)

relayCallback :: Handle -> RelayData -> Proto (Maybe ExitCode)
relayCallback hout (RelayMessage (DATA len)) = do
	writeHandle hout =<< receiveBytes len
	return Nothing
relayCallback _ (RelayMessage (CONNECTDONE exitcode)) =
	return (Just exitcode)
relayCallback _ (RelayMessage _) = do
	sendMessage (ERROR "expected DATA or CONNECTDONE")
	return (Just (ExitFailure 1))
relayCallback _ (RelayData b) = do
	let len = Len $ fromIntegral $ L.length b
	sendMessage (DATA len)
	sendBytes len b
	return Nothing

-- | Serve the protocol.
--
-- Note that if the client sends an unexpected message, the server will
-- respond with PTOTO_ERROR, and always continues processing messages.
-- Since the protocol is not versioned, this is necessary to handle
-- protocol changes robustly, since the client can detect when it's
-- talking to a server that does not support some new feature, and fall
-- back.
--
-- When the client sends ERROR to the server, the server gives up,
-- since it's not clear what state the client is is, and so not possible to
-- recover.
serve :: UUID -> Proto ()
serve myuuid = go Nothing
  where
	go autheduuid = do
		r <- receiveMessage
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
			ERROR _ -> return ()
			_ -> do
				case autheduuid of
					Just theiruuid -> authed theiruuid r
					Nothing -> sendMessage (ERROR "must AUTH first")
				go autheduuid
	
	authed _theiruuid r = case r of
		LOCKCONTENT key -> tryLockContent key $ \locked -> do
			sendSuccess locked
			when locked $ do
				r' <- receiveMessage
				case r' of
					UNLOCKCONTENT -> return ()
					_ -> sendMessage (ERROR "expected UNLOCKCONTENT")
		CHECKPRESENT key -> sendSuccess =<< checkContentPresent key
		REMOVE key -> sendSuccess =<< removeKeyFile key
		PUT key -> do
			have <- checkContentPresent key
			if have
				then sendMessage ALREADY_HAVE
				else do
					ok <- receiveContent key PUT_FROM
					when ok $
						setPresent key myuuid
		-- setPresent not called because the peer may have
		-- requested the data but not permanatly stored it.
		GET offset key -> void $ sendContent key offset
		CONNECT service -> do
			exitcode <- relayService service relayCallback
			sendMessage (CONNECTDONE exitcode)
		_ -> sendMessage (ERROR "unexpected command")

sendContent :: Key -> Offset -> Proto Bool
sendContent key offset = do
	(len, content) <- readKeyFileLen key offset
	sendMessage (DATA len)
	sendBytes len content
	checkSuccess

receiveContent :: Key -> (Offset -> Message) -> Proto Bool
receiveContent key mkmsg = do
	Len n <- keyFileSize key
	let offset = Offset n
	sendMessage (mkmsg offset)
	r <- receiveMessage
	case r of
		DATA len -> do
			ok <- writeKeyFile key offset len =<< receiveBytes len
			sendSuccess ok
			return ok
		_ -> do
			sendMessage (ERROR "expected DATA")
			return False

checkSuccess :: Proto Bool
checkSuccess = do
	ack <- receiveMessage
	case ack of
		SUCCESS -> return True
		FAILURE -> return False
		_ -> do
			sendMessage (ERROR "expected SUCCESS or FAILURE")
			return False

sendSuccess :: Bool -> Proto ()
sendSuccess True = sendMessage SUCCESS
sendSuccess False = sendMessage FAILURE

-- Reads key file from an offset. The Len should correspond to
-- the length of the ByteString, but to avoid buffering the content
-- in memory, is gotten using keyFileSize.
readKeyFileLen :: Key -> Offset -> Proto (Len, L.ByteString)
readKeyFileLen key (Offset offset) = do
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
	formatMessage (CONNECT service) = ["CONNECT", Proto.serialize service]
	formatMessage (CONNECTDONE exitcode) = ["CONNECTDONE", Proto.serialize exitcode]
	formatMessage (CHECKPRESENT key) = ["CHECKPRESENT", Proto.serialize key]
	formatMessage (LOCKCONTENT key) = ["LOCKCONTENT", Proto.serialize key]
	formatMessage UNLOCKCONTENT = ["UNLOCKCONTENT"]
	formatMessage (REMOVE key) = ["REMOVE", Proto.serialize key]
	formatMessage (GET offset key) = ["GET", Proto.serialize offset, Proto.serialize key]
	formatMessage (PUT key) = ["PUT", Proto.serialize key]
	formatMessage (PUT_FROM offset) = ["PUT-FROM", Proto.serialize offset]
	formatMessage ALREADY_HAVE = ["ALREADY-HAVE"]
	formatMessage SUCCESS = ["SUCCESS"]
	formatMessage FAILURE = ["FAILURE"]
	formatMessage (DATA len) = ["DATA", Proto.serialize len]
	formatMessage (ERROR err) = ["ERROR", Proto.serialize err]

instance Proto.Receivable Message where
	parseCommand "AUTH" = Proto.parse2 AUTH
	parseCommand "AUTH-SUCCESS" = Proto.parse1 AUTH_SUCCESS
	parseCommand "AUTH-FAILURE" = Proto.parse0 AUTH_FAILURE
	parseCommand "CONNECT" = Proto.parse1 CONNECT
	parseCommand "CONNECTDONE" = Proto.parse1 CONNECT
	parseCommand "CHECKPRESENT" = Proto.parse1 CHECKPRESENT
	parseCommand "LOCKCONTENT" = Proto.parse1 LOCKCONTENT
	parseCommand "UNLOCKCONTENT" = Proto.parse0 UNLOCKCONTENT
	parseCommand "REMOVE" = Proto.parse1 REMOVE
	parseCommand "GET" = Proto.parse2 GET
	parseCommand "PUT" = Proto.parse1 PUT
	parseCommand "PUT-FROM" = Proto.parse1 PUT_FROM
	parseCommand "ALREADY-HAVE" = Proto.parse0 ALREADY_HAVE
	parseCommand "SUCCESS" = Proto.parse0 SUCCESS
	parseCommand "FAILURE" = Proto.parse0 FAILURE
	parseCommand "DATA" = Proto.parse1 DATA
	parseCommand "ERROR" = Proto.parse1 ERROR
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

instance Proto.Serializable Service where
	serialize UploadPack = "git-upload-pack"
	serialize ReceivePack = "git-receive-pack"
	deserialize "git-upload-pack" = Just UploadPack
	deserialize "git-receive-pack" = Just ReceivePack
	deserialize _ = Nothing
