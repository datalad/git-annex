{- P2P protocol
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts, RankNTypes #-}

module Remote.Helper.P2P where

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
import System.IO
import qualified Data.ByteString.Lazy as L

newtype AuthToken = AuthToken String
	deriving (Show)

mkAuthToken :: String -> Maybe AuthToken
mkAuthToken = fmap AuthToken . headMaybe . lines

nullAuthToken :: AuthToken
nullAuthToken = AuthToken ""

newtype Offset = Offset Integer
	deriving (Show)

newtype Len = Len Integer
	deriving (Show)

-- | Service as used by the connect message is gitremote-helpers(1)
data Service = UploadPack | ReceivePack
	deriving (Show)

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
	parseCommand "CONNECTDONE" = Proto.parse1 CONNECTDONE
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

-- | Free monad for the protocol, combining net communication,
-- and local actions.
data ProtoF c = Net (NetF c) | Local (LocalF c)
	deriving (Functor)

type Proto = Free ProtoF

net :: Net a -> Proto a
net = hoistFree Net

local :: Local a -> Proto a
local = hoistFree Local

data NetF c
	= SendMessage Message c
	| ReceiveMessage (Message -> c)
	| SendBytes Len L.ByteString c
	| ReceiveBytes Len (L.ByteString -> c)
	| CheckAuthToken UUID AuthToken (Bool -> c)
	| RelayService Service c
	-- ^ Runs a service, relays its output to the peer, and data
	-- from the peer to it.
	| Relay RelayHandle RelayHandle (ExitCode -> c)
	-- ^ Reads from the first RelayHandle, and sends the data to a
	-- peer, while at the same time accepting input from the peer
	-- which is sent the the second RelayHandle. Continues until 
	-- the peer sends an ExitCode.
	deriving (Functor)

type Net = Free NetF

newtype RelayHandle = RelayHandle Handle

data LocalF c
	-- ^ Lazily reads bytes from peer. Stops once Len are read,
	-- or if connection is lost, and in either case returns the bytes
	-- that were read. This allows resuming interrupted transfers.
	= KeyFileSize Key (Len -> c)
	-- ^ Checks size of key file (dne = 0)
	| ReadKeyFile Key Offset (L.ByteString -> c)
	| WriteKeyFile Key Offset Len L.ByteString (Bool -> c)
	-- ^ Writes to key file starting at an offset. Returns True
	-- once the whole content of the key is stored in the key file.
	--
	-- Note: The ByteString may not contain the entire remaining content
	-- of the key. Only once the key file size == Len has the whole
	-- content been transferred.
	| SetPresent Key UUID c
	| CheckContentPresent Key (Bool -> c)
	-- ^ Checks if the whole content of the key is locally present.
	| RemoveKeyFile Key (Bool -> c)
	-- ^ If the key file is not present, still succeeds.
	-- May fail if not enough copies to safely drop, etc.
	| TryLockContent Key (Bool -> Proto ()) c
	-- ^ Try to lock the content of a key,  preventing it
	-- from being deleted, and run the provided protocol action.
	deriving (Functor)

type Local = Free LocalF

-- Generate sendMessage etc functions for all free monad constructors.
$(makeFree ''NetF)
$(makeFree ''LocalF)

-- | Running Proto actions purely, to see what they do.
runPure :: Show r => Proto r -> [Message] -> [(String, Maybe Message)]
runPure (Pure r) _ = [("result: " ++ show r, Nothing)]
runPure (Free (Net n)) ms = runNet n ms
runPure (Free (Local n)) ms = runLocal n ms

runNet :: Show r => NetF (Proto r) -> [Message] -> [(String, Maybe Message)]
runNet (SendMessage m next) ms = (">",  Just m):runPure next ms
runNet (ReceiveMessage _) [] = [("not enough Messages provided", Nothing)]
runNet (ReceiveMessage next) (m:ms) = ("<", Just m):runPure (next m) ms
runNet (SendBytes _ _ next) ms = ("> bytes", Nothing):runPure next ms
runNet (ReceiveBytes _ next) ms = ("< bytes", Nothing):runPure (next L.empty) ms
runNet (CheckAuthToken _ _ next) ms = runPure (next True) ms
runNet (Relay _ _ next) ms = runPure (next ExitSuccess) ms
runNet (RelayService _ next) ms = runPure next ms

runLocal :: Show r => LocalF (Proto r) -> [Message] -> [(String, Maybe Message)]
runLocal (KeyFileSize _ next) ms = runPure (next (Len 100)) ms
runLocal (ReadKeyFile _ _ next) ms = runPure (next L.empty) ms
runLocal (WriteKeyFile _ _ _ _ next) ms = runPure (next True) ms
runLocal (SetPresent _ _ next) ms = runPure next ms
runLocal (CheckContentPresent _ next) ms = runPure (next False) ms
runLocal (RemoveKeyFile _ next) ms = runPure (next True) ms
runLocal (TryLockContent _ p next) ms = runPure (p True >> next) ms

protoDump :: [(String, Maybe Message)] -> String
protoDump = unlines . map protoDump'

protoDump' :: (String, Maybe Message) -> String
protoDump' (s, Nothing) = s
protoDump' (s, Just m) = s ++ " " ++ unwords (Proto.formatMessage m)

auth :: UUID -> AuthToken -> Proto (Maybe UUID)
auth myuuid t = do
	net $ sendMessage (AUTH myuuid t)
	r <- net receiveMessage
	case r of
		AUTH_SUCCESS theiruuid -> return $ Just theiruuid
		AUTH_FAILURE -> return Nothing
		_ -> do
			net $ sendMessage (ERROR "auth failed")
			return Nothing

checkPresent :: Key -> Proto Bool
checkPresent key = do
	net $ sendMessage (CHECKPRESENT key)
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
		net $ sendMessage (LOCKCONTENT key)
		checkSuccess
	cleanup True = runproto $ net $ sendMessage UNLOCKCONTENT
	cleanup False = return ()

remove :: Key -> Proto Bool
remove key = do
	net $ sendMessage (REMOVE key)
	checkSuccess

get :: Key -> Proto Bool
get key = receiveContent key (`GET` key)

put :: Key -> Proto Bool
put key = do
	net $ sendMessage (PUT key)
	r <- net receiveMessage
	case r of
		PUT_FROM offset -> sendContent key offset
		ALREADY_HAVE -> return True
		_ -> do
			net $ sendMessage (ERROR "expected PUT_FROM")
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
-- When the client sends ERROR to the server, the server gives up,
-- since it's not clear what state the client is is, and so not possible to
-- recover.
serve :: UUID -> Proto ()
serve myuuid = go Nothing
  where
	go autheduuid = do
		r <- net receiveMessage
		case r of
			AUTH theiruuid authtoken -> do
				ok <- net $ checkAuthToken theiruuid authtoken
				if ok
					then do
						net $ sendMessage (AUTH_SUCCESS myuuid)
						go (Just theiruuid)
					else do
						net $ sendMessage AUTH_FAILURE
						go autheduuid
			ERROR _ -> return ()
			_ -> do
				case autheduuid of
					Just theiruuid -> authed theiruuid r
					Nothing -> net $ sendMessage (ERROR "must AUTH first")
				go autheduuid
	
	authed _theiruuid r = case r of
		LOCKCONTENT key -> local $ tryLockContent key $ \locked -> do
			sendSuccess locked
			when locked $ do
				r' <- net receiveMessage
				case r' of
					UNLOCKCONTENT -> return ()
					_ -> net $ sendMessage (ERROR "expected UNLOCKCONTENT")
		CHECKPRESENT key -> sendSuccess =<< local (checkContentPresent key)
		REMOVE key ->  sendSuccess =<< local (removeKeyFile key)
		PUT key -> do
			have <- local $ checkContentPresent key
			if have
				then net $ sendMessage ALREADY_HAVE
				else do
					ok <- receiveContent key PUT_FROM
					when ok $
						local $ setPresent key myuuid
		-- setPresent not called because the peer may have
		-- requested the data but not permanatly stored it.
		GET offset key -> void $ sendContent key offset
		CONNECT service -> net $ relayService service
		_ -> net $ sendMessage (ERROR "unexpected command")

sendContent :: Key -> Offset -> Proto Bool
sendContent key offset = do
	(len, content) <- readKeyFileLen key offset
	net $ sendMessage (DATA len)
	net $ sendBytes len content
	checkSuccess

receiveContent :: Key -> (Offset -> Message) -> Proto Bool
receiveContent key mkmsg = do
	Len n <- local $ keyFileSize key
	let offset = Offset n
	net $ sendMessage (mkmsg offset)
	r <- net receiveMessage
	case r of
		DATA len -> do
			ok <- local . writeKeyFile key offset len
				=<< net (receiveBytes len)
			sendSuccess ok
			return ok
		_ -> do
			net $ sendMessage (ERROR "expected DATA")
			return False

checkSuccess :: Proto Bool
checkSuccess = do
	ack <- net receiveMessage
	case ack of
		SUCCESS -> return True
		FAILURE -> return False
		_ -> do
			net $ sendMessage (ERROR "expected SUCCESS or FAILURE")
			return False

sendSuccess :: Bool -> Proto ()
sendSuccess True = net $ sendMessage SUCCESS
sendSuccess False = net $ sendMessage FAILURE

-- Reads key file from an offset. The Len should correspond to
-- the length of the ByteString, but to avoid buffering the content
-- in memory, is gotten using keyFileSize.
readKeyFileLen :: Key -> Offset -> Proto (Len, L.ByteString)
readKeyFileLen key (Offset offset) = do
	(Len totallen) <- local $ keyFileSize key
	let len = totallen - offset
	if len <= 0
		then return (Len 0, L.empty)
		else do
			content <- local $ readKeyFile key (Offset offset)
			return (Len len, content)

connect :: Service -> Handle -> Handle -> Proto ExitCode
connect service hin hout = do
	net $ sendMessage (CONNECT service)
	net $ relay (RelayHandle hin) (RelayHandle hout)

data RelayData
	= RelayToPeer L.ByteString
	| RelayFromPeer L.ByteString
	| RelayDone ExitCode
	deriving (Show)

relayFromPeer :: Net RelayData
relayFromPeer = do
	r <- receiveMessage
	case r of
		CONNECTDONE exitcode -> return $ RelayDone exitcode
		DATA len -> RelayFromPeer <$> receiveBytes len
		_ -> do
			sendMessage $ ERROR "expected DATA or CONNECTDONE"
			return $ RelayDone $ ExitFailure 1

relayToPeer :: RelayData -> Net ()
relayToPeer (RelayDone exitcode) = sendMessage (CONNECTDONE exitcode)
relayToPeer (RelayToPeer b) = do
	let len = Len $ fromIntegral $ L.length b
	sendMessage (DATA len)
	sendBytes len b
relayToPeer (RelayFromPeer _) = return ()
