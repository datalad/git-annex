{- P2P protocol
 -
 - See doc/design/p2p_protocol.mdwn
 -
 - Copyright 2016-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module P2P.Protocol where

import qualified Utility.SimpleProtocol as Proto
import Types (Annex)
import Types.Key
import Types.UUID
import Types.Remote (Verification(..), unVerified)
import Types.Backend (IncrementalVerifier(..))
import Utility.AuthToken
import Utility.Applicative
import Utility.PartialPrelude
import Utility.Metered
import Utility.FileSystemEncoding
import Git.FilePath
import Annex.ChangedRefs (ChangedRefs)

import Control.Monad
import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.Catch
import System.FilePath
import System.Exit (ExitCode(..))
import System.IO
import qualified Data.ByteString.Lazy as L
import Data.Char
import Control.Applicative
import Prelude

newtype Offset = Offset Integer
	deriving (Show)

newtype Len = Len Integer
	deriving (Show)

newtype ProtocolVersion = ProtocolVersion Integer
	deriving (Show, Eq, Ord)

defaultProtocolVersion :: ProtocolVersion
defaultProtocolVersion = ProtocolVersion 0

maxProtocolVersion :: ProtocolVersion
maxProtocolVersion = ProtocolVersion 1

newtype ProtoAssociatedFile = ProtoAssociatedFile AssociatedFile
	deriving (Show)

-- | Service as used by the connect message in gitremote-helpers(1)
data Service = UploadPack | ReceivePack
	deriving (Show)

data Validity = Valid | Invalid
	deriving (Show)

-- | Messages in the protocol. The peer that makes the connection
-- always initiates requests, and the other peer makes responses to them.
data Message
	= AUTH UUID AuthToken -- uuid of the peer that is authenticating
	| AUTH_SUCCESS UUID -- uuid of the remote peer
	| AUTH_FAILURE
	| VERSION ProtocolVersion
	| CONNECT Service
	| CONNECTDONE ExitCode
	| NOTIFYCHANGE
	| CHANGED ChangedRefs
	| CHECKPRESENT Key
	| LOCKCONTENT Key
	| UNLOCKCONTENT
	| REMOVE Key
	| GET Offset ProtoAssociatedFile Key
	| PUT ProtoAssociatedFile Key
	| PUT_FROM Offset
	| ALREADY_HAVE
	| SUCCESS
	| FAILURE
	| DATA Len -- followed by bytes of data
	| VALIDITY Validity
	| ERROR String
	deriving (Show)

instance Proto.Sendable Message where
	formatMessage (AUTH uuid authtoken) = ["AUTH", Proto.serialize uuid, Proto.serialize authtoken]
	formatMessage (AUTH_SUCCESS uuid) = ["AUTH-SUCCESS",  Proto.serialize uuid]
	formatMessage AUTH_FAILURE = ["AUTH-FAILURE"]
	formatMessage (VERSION v) = ["VERSION", Proto.serialize v]
	formatMessage (CONNECT service) = ["CONNECT", Proto.serialize service]
	formatMessage (CONNECTDONE exitcode) = ["CONNECTDONE", Proto.serialize exitcode]
	formatMessage NOTIFYCHANGE = ["NOTIFYCHANGE"]
	formatMessage (CHANGED refs) = ["CHANGED", Proto.serialize refs]
	formatMessage (CHECKPRESENT key) = ["CHECKPRESENT", Proto.serialize key]
	formatMessage (LOCKCONTENT key) = ["LOCKCONTENT", Proto.serialize key]
	formatMessage UNLOCKCONTENT = ["UNLOCKCONTENT"]
	formatMessage (REMOVE key) = ["REMOVE", Proto.serialize key]
	formatMessage (GET offset af key) = ["GET", Proto.serialize offset, Proto.serialize af, Proto.serialize key]
	formatMessage (PUT af key) = ["PUT", Proto.serialize af, Proto.serialize key]
	formatMessage (PUT_FROM offset) = ["PUT-FROM", Proto.serialize offset]
	formatMessage ALREADY_HAVE = ["ALREADY-HAVE"]
	formatMessage SUCCESS = ["SUCCESS"]
	formatMessage FAILURE = ["FAILURE"]
	formatMessage (VALIDITY Valid) = ["VALID"]
	formatMessage (VALIDITY Invalid) = ["INVALID"]
	formatMessage (DATA len) = ["DATA", Proto.serialize len]
	formatMessage (ERROR err) = ["ERROR", Proto.serialize err]

instance Proto.Receivable Message where
	parseCommand "AUTH" = Proto.parse2 AUTH
	parseCommand "AUTH-SUCCESS" = Proto.parse1 AUTH_SUCCESS
	parseCommand "AUTH-FAILURE" = Proto.parse0 AUTH_FAILURE
	parseCommand "VERSION" = Proto.parse1 VERSION
	parseCommand "CONNECT" = Proto.parse1 CONNECT
	parseCommand "CONNECTDONE" = Proto.parse1 CONNECTDONE
	parseCommand "NOTIFYCHANGE" = Proto.parse0 NOTIFYCHANGE
	parseCommand "CHANGED" = Proto.parse1 CHANGED
	parseCommand "CHECKPRESENT" = Proto.parse1 CHECKPRESENT
	parseCommand "LOCKCONTENT" = Proto.parse1 LOCKCONTENT
	parseCommand "UNLOCKCONTENT" = Proto.parse0 UNLOCKCONTENT
	parseCommand "REMOVE" = Proto.parse1 REMOVE
	parseCommand "GET" = Proto.parse3 GET
	parseCommand "PUT" = Proto.parse2 PUT
	parseCommand "PUT-FROM" = Proto.parse1 PUT_FROM
	parseCommand "ALREADY-HAVE" = Proto.parse0 ALREADY_HAVE
	parseCommand "SUCCESS" = Proto.parse0 SUCCESS
	parseCommand "FAILURE" = Proto.parse0 FAILURE
	parseCommand "DATA" = Proto.parse1 DATA
	parseCommand "ERROR" = Proto.parse1 ERROR
	parseCommand "VALID" = Proto.parse0 (VALIDITY Valid)
	parseCommand "INVALID" = Proto.parse0 (VALIDITY Invalid)
	parseCommand _ = Proto.parseFail

instance Proto.Serializable ProtocolVersion where
	serialize (ProtocolVersion n) = show n
	deserialize = ProtocolVersion <$$> readish

instance Proto.Serializable Offset where
	serialize (Offset n) = show n
	deserialize = Offset <$$> readish

instance Proto.Serializable Len where
	serialize (Len n) = show n
	deserialize = Len <$$> readish

instance Proto.Serializable Service where
	serialize UploadPack = "git-upload-pack"
	serialize ReceivePack = "git-receive-pack"
	deserialize "git-upload-pack" = Just UploadPack
	deserialize "git-receive-pack" = Just ReceivePack
	deserialize _ = Nothing

-- | Since ProtoAssociatedFile is not the last thing in a protocol line,
-- its serialization cannot contain any whitespace. This is handled
-- by replacing whitespace with '%' (and '%' with '%%')
--
-- When deserializing an AssociatedFile from a peer, it's sanitized,
-- to avoid any unusual characters that might cause problems when it's
-- displayed to the user.
--
-- These mungings are ok, because a ProtoAssociatedFile is only ever displayed
-- to the user and does not need to match a file on disk.
instance Proto.Serializable ProtoAssociatedFile where
	serialize (ProtoAssociatedFile (AssociatedFile Nothing)) = ""
	serialize (ProtoAssociatedFile (AssociatedFile (Just af))) = 
		decodeBS' $ toInternalGitPath $ encodeBS' $ concatMap esc $ fromRawFilePath af
	  where
		esc '%' = "%%"
		esc c 
			| isSpace c = "%"
			| otherwise = [c]
	
	deserialize s = case fromRawFilePath $ fromInternalGitPath $ toRawFilePath $ deesc [] s of
		[] -> Just $ ProtoAssociatedFile $ AssociatedFile Nothing
		f
			| isRelative f -> Just $ ProtoAssociatedFile $ 
				AssociatedFile $ Just $ toRawFilePath f
			| otherwise -> Nothing
	  where
	  	deesc b [] = reverse b
		deesc b ('%':'%':cs) = deesc ('%':b) cs
		deesc b ('%':cs) = deesc ('_':b) cs
		deesc b (c:cs)
			| isControl c = deesc ('_':b) cs
			| otherwise = deesc (c:b) cs

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
	| ReceiveMessage (Maybe Message -> c)
	| SendBytes Len L.ByteString MeterUpdate c
	-- ^ Sends exactly Len bytes of data. (Any more or less will
	-- confuse the receiver.)
	| ReceiveBytes Len MeterUpdate (L.ByteString -> c)
	-- ^ Lazily reads bytes from peer. Stops once Len are read,
	-- or if connection is lost, and in either case returns the bytes
	-- that were read. This allows resuming interrupted transfers.
	| CheckAuthToken UUID AuthToken (Bool -> c)
	| RelayService Service c
	-- ^ Runs a service, relays its output to the peer, and data
	-- from the peer to it.
	| Relay RelayHandle RelayHandle (ExitCode -> c)
	-- ^ Reads from the first RelayHandle, and sends the data to a
	-- peer, while at the same time accepting input from the peer
	-- which is sent the the second RelayHandle. Continues until 
	-- the peer sends an ExitCode.
	| SetProtocolVersion ProtocolVersion c
	--- ^ Called when a new protocol version has been negotiated.
	| GetProtocolVersion (ProtocolVersion -> c)
	deriving (Functor)

type Net = Free NetF

newtype RelayHandle = RelayHandle Handle

data LocalF c
	= TmpContentSize Key (Len -> c)
	-- ^ Gets size of the temp file where received content may have
	-- been stored. If not present, returns 0.
	| FileSize FilePath (Len -> c)
	-- ^ Gets size of the content of a file. If not present, returns 0.
	| ContentSize Key (Maybe Len -> c)
	-- ^ Gets size of the content of a key, when the full content is
	-- present.
	| ReadContent Key AssociatedFile Offset (L.ByteString -> Proto Validity -> Proto Bool) (Bool -> c)
	-- ^ Reads the content of a key and sends it to the callback.
	-- Must run the callback, or terminate the protocol connection.
	--
	-- May send any amount of data, including L.empty if the content is
	-- not available. The callback must deal with that.
	--
	-- And the content may change while it's being sent.
	-- The callback is passed a validity check that it can run after
	-- sending the content to detect when this happened.
	| StoreContent Key AssociatedFile Offset Len (Proto L.ByteString) (Proto (Maybe Validity)) (Bool -> c)
	-- ^ Stores content to the key's temp file starting at an offset.
	-- Once the whole content of the key has been stored, moves the
	-- temp file into place as the content of the key, and returns True.
	--
	-- Must consume the whole lazy ByteString, or if unable to do
	-- so, terminate the protocol connection.
	--
	-- If the validity check is provided and fails, the content was
	-- changed while it was being sent, so verificiation of the
	-- received content should be forced.
	--
	-- Note: The ByteString may not contain the entire remaining content
	-- of the key. Only once the temp file size == Len has the whole
	-- content been transferred.
	| StoreContentTo FilePath (Maybe IncrementalVerifier) Offset Len (Proto L.ByteString) (Proto (Maybe Validity)) ((Bool, Verification) -> c)
	-- ^ Like StoreContent, but stores the content to a temp file.
	| SetPresent Key UUID c
	| CheckContentPresent Key (Bool -> c)
	-- ^ Checks if the whole content of the key is locally present.
	| RemoveContent Key (Bool -> c)
	-- ^ If the content is not present, still succeeds.
	-- May fail if not enough copies to safely drop, etc.
	| TryLockContent Key (Bool -> Proto ()) c
	-- ^ Try to lock the content of a key,  preventing it
	-- from being deleted, while running the provided protocol
	-- action. If unable to lock the content, or the content is not
	-- present, runs the protocol action with False.
	| WaitRefChange (ChangedRefs -> c)
	-- ^ Waits for one or more git refs to change and returns them.a
	| UpdateMeterTotalSize Meter TotalSize c
	-- ^ Updates the total size of a Meter, for cases where the size is
	-- not known until the data is being received.
	| RunValidityCheck (Annex Validity) (Validity -> c)
	-- ^ Runs a deferred validity check.
	deriving (Functor)

type Local = Free LocalF

-- Generate sendMessage etc functions for all free monad constructors.
$(makeFree ''NetF)
$(makeFree ''LocalF)

auth :: UUID -> AuthToken -> Proto () -> Proto (Maybe UUID)
auth myuuid t a = do
	net $ sendMessage (AUTH myuuid t)
	postAuth a

postAuth :: Proto () -> Proto (Maybe UUID)
postAuth a = do
	r <- net receiveMessage
	case r of
		Just (AUTH_SUCCESS theiruuid) -> do
			a
			return $ Just theiruuid
		Just AUTH_FAILURE -> return Nothing
		_ -> do
			net $ sendMessage (ERROR "auth failed")
			return Nothing

negotiateProtocolVersion :: ProtocolVersion -> Proto ()
negotiateProtocolVersion preferredversion = do
	net $ sendMessage (VERSION preferredversion)
	r <- net receiveMessage
	case r of
		Just (VERSION v) -> net $ setProtocolVersion v
		-- Old server doesn't know about the VERSION command.
		Just (ERROR _) -> return ()
		_ -> net $ sendMessage (ERROR "expected VERSION")

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
	=> (forall r. r -> Proto r -> m r)
	-> Key
	-> (Bool -> m a)
	-> m a
lockContentWhile runproto key a = bracket setup cleanup a
  where
	setup = runproto False $ do
		net $ sendMessage (LOCKCONTENT key)
		checkSuccess
	cleanup True = runproto () $ net $ sendMessage UNLOCKCONTENT
	cleanup False = return ()

remove :: Key -> Proto Bool
remove key = do
	net $ sendMessage (REMOVE key)
	checkSuccess

get :: FilePath -> Key -> Maybe IncrementalVerifier -> AssociatedFile -> Meter -> MeterUpdate -> Proto (Bool, Verification)
get dest key iv af m p = 
	receiveContent (Just m) p sizer storer $ \offset ->
		GET offset (ProtoAssociatedFile af) key
  where
	sizer = fileSize dest
	storer = storeContentTo dest iv

put :: Key -> AssociatedFile -> MeterUpdate -> Proto Bool
put key af p = do
	net $ sendMessage (PUT (ProtoAssociatedFile af) key)
	r <- net receiveMessage
	case r of
		Just (PUT_FROM offset) -> sendContent key af offset p
		Just ALREADY_HAVE -> return True
		_ -> do
			net $ sendMessage (ERROR "expected PUT_FROM or ALREADY_HAVE")
			return False

data ServerHandler a
	= ServerGot a
	| ServerContinue
	| ServerUnexpected

-- Server loop, getting messages from the client and handling them
serverLoop :: (Message -> Proto (ServerHandler a)) -> Proto (Maybe a)
serverLoop a = do
	mcmd <- net receiveMessage
	case mcmd of
		-- When the client sends ERROR to the server, the server
		-- gives up, since it's not clear what state the client
		-- is in, and so not possible to recover.
		Just (ERROR _) -> return Nothing
		-- When the client sends an unparseable message, the server
		-- responds with an error message, and loops. This allows
		-- expanding the protocol with new messages.
		Nothing -> do
			net $ sendMessage (ERROR "unknown command")
			serverLoop a
		Just cmd -> do
			v <- a cmd
			case v of
				ServerGot r -> return (Just r)
				ServerContinue -> serverLoop a
				-- If the client sends an unexpected message,
				-- the server will respond with ERROR, and
				-- always continues processing messages.
				--
				-- Since the protocol is not versioned, this
				-- is necessary to handle protocol changes
				-- robustly, since the client can detect when
				-- it's talking to a server that does not
				-- support some new feature, and fall back.
				ServerUnexpected -> do
					net $ sendMessage (ERROR "unexpected command")
					serverLoop a

-- | Serve the protocol, with an unauthenticated peer. Once the peer
-- successfully authenticates, returns their UUID.
serveAuth :: UUID -> Proto (Maybe UUID)
serveAuth myuuid = serverLoop handler
  where
	handler (AUTH theiruuid authtoken) = do
		ok <- net $ checkAuthToken theiruuid authtoken
		if ok
			then do
				net $ sendMessage (AUTH_SUCCESS myuuid)
				return (ServerGot theiruuid)
			else do
				net $ sendMessage AUTH_FAILURE
				return ServerContinue
	handler _ = return ServerUnexpected

data ServerMode
	= ServeReadOnly
	-- ^ Allow reading, but not writing.
	| ServeAppendOnly
	-- ^ Allow reading, and storing new objects, but not deleting objects.
	| ServeReadWrite
	-- ^ Full read and write access.
	deriving (Eq, Ord)

-- | Serve the protocol, with a peer that has authenticated.
serveAuthed :: ServerMode -> UUID -> Proto ()
serveAuthed servermode myuuid = void $ serverLoop handler
  where
	readonlyerror = net $ sendMessage (ERROR "this repository is read-only; write access denied")
	appendonlyerror = net $ sendMessage (ERROR "this repository is append-only; removal denied")
	handler (VERSION theirversion) = do
		let v = min theirversion maxProtocolVersion
		net $ setProtocolVersion v
		net $ sendMessage (VERSION v)
		return ServerContinue
	handler (LOCKCONTENT key) = do
		local $ tryLockContent key $ \locked -> do
			sendSuccess locked
			when locked $ do
				r' <- net receiveMessage
				case r' of
					Just UNLOCKCONTENT -> return ()
					_ -> net $ sendMessage (ERROR "expected UNLOCKCONTENT")
		return ServerContinue
	handler (CHECKPRESENT key) = do
		sendSuccess =<< local (checkContentPresent key)
		return ServerContinue
	handler (REMOVE key) = case servermode of
		ServeReadWrite -> do
			sendSuccess =<< local (removeContent key)
			return ServerContinue
		ServeAppendOnly -> do
			appendonlyerror
			return ServerContinue
		ServeReadOnly -> do
			readonlyerror
			return ServerContinue
	handler (PUT (ProtoAssociatedFile af) key) = case servermode of
		ServeReadWrite -> handleput af key
		ServeAppendOnly -> handleput af key
		ServeReadOnly -> do
			readonlyerror
			return ServerContinue
	handler (GET offset (ProtoAssociatedFile af) key) = do
		void $ sendContent key af offset nullMeterUpdate
		-- setPresent not called because the peer may have
		-- requested the data but not permanently stored it.
		return ServerContinue
	handler (CONNECT service) = do
		let goahead = net $ relayService service
		case (servermode, service) of
			(ServeReadWrite, _) -> goahead
			(ServeAppendOnly, UploadPack) -> goahead
			-- git protocol could be used to overwrite
			-- refs or something, so don't allow
			(ServeAppendOnly, ReceivePack) -> readonlyerror
			(ServeReadOnly, UploadPack) -> goahead
			(ServeReadOnly, ReceivePack) -> readonlyerror
		-- After connecting to git, there may be unconsumed data
		-- from the git processes hanging around (even if they
		-- exited successfully), so stop serving this connection.
		return $ ServerGot ()
	handler NOTIFYCHANGE = do
		refs <- local waitRefChange
		net $ sendMessage (CHANGED refs)
		return ServerContinue
	handler _ = return ServerUnexpected

	handleput af key = do
		have <- local $ checkContentPresent key
		if have
			then net $ sendMessage ALREADY_HAVE
			else do
				let sizer = tmpContentSize key
				let storer = \o l b v -> unVerified $
					storeContent key af o l b v
				(ok, _v) <- receiveContent Nothing nullMeterUpdate sizer storer PUT_FROM
				when ok $
					local $ setPresent key myuuid
		return ServerContinue

sendContent :: Key -> AssociatedFile -> Offset -> MeterUpdate -> Proto Bool
sendContent key af offset@(Offset n) p = go =<< local (contentSize key)
  where
	go (Just (Len totallen)) = do
		let len = totallen - n
		if len <= 0
			then sender (Len 0) L.empty (return Valid)
			else local $ readContent key af offset $
				sender (Len len)
	-- Content not available to send. Indicate this by sending
	-- empty data and indlicate it's invalid.
 	go Nothing = sender (Len 0) L.empty (return Invalid)
	sender len content validitycheck = do
		let p' = offsetMeterUpdate p (toBytesProcessed n)
		net $ sendMessage (DATA len)
		net $ sendBytes len content p'
		ver <- net getProtocolVersion
		when (ver >= ProtocolVersion 1) $
			net . sendMessage . VALIDITY =<< validitycheck
		checkSuccess

receiveContent
	:: Maybe Meter
	-> MeterUpdate
	-> Local Len
	-> (Offset -> Len -> Proto L.ByteString -> Proto (Maybe Validity) -> Local (Bool, Verification))
	-> (Offset -> Message)
	-> Proto (Bool, Verification)
receiveContent mm p sizer storer mkmsg = do
	Len n <- local sizer
	let p' = offsetMeterUpdate p (toBytesProcessed n)
	let offset = Offset n
	net $ sendMessage (mkmsg offset)
	r <- net receiveMessage
	case r of
		Just (DATA len@(Len l)) -> do
			local $ case mm of
				Nothing -> return ()
				Just m -> updateMeterTotalSize m (TotalSize (n+l))
			ver <- net getProtocolVersion
			let validitycheck = if ver >= ProtocolVersion 1
				then net receiveMessage >>= \case
					Just (VALIDITY v) -> return (Just v)
					_ -> do
						net $ sendMessage (ERROR "expected VALID or INVALID")
						return Nothing
				else return Nothing
			(ok, v) <- local $ storer offset len
				(net (receiveBytes len p'))
				validitycheck
			sendSuccess ok
			return (ok, v)
		_ -> do
			net $ sendMessage (ERROR "expected DATA")
			return (False, UnVerified)

checkSuccess :: Proto Bool
checkSuccess = do
	ack <- net receiveMessage
	case ack of
		Just SUCCESS -> return True
		Just FAILURE -> return False
		_ -> do
			net $ sendMessage (ERROR "expected SUCCESS or FAILURE")
			return False

sendSuccess :: Bool -> Proto ()
sendSuccess True = net $ sendMessage SUCCESS
sendSuccess False = net $ sendMessage FAILURE

notifyChange :: Proto (Maybe ChangedRefs)
notifyChange = do
	net $ sendMessage NOTIFYCHANGE
	ack <- net receiveMessage
	case ack of
		Just (CHANGED rs) -> return (Just rs)
		_ -> do
			net $ sendMessage (ERROR "expected CHANGED")
			return Nothing

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
		Just (CONNECTDONE exitcode) -> return $ RelayDone exitcode
		Just (DATA len) -> RelayFromPeer <$> receiveBytes len nullMeterUpdate
		_ -> do
			sendMessage $ ERROR "expected DATA or CONNECTDONE"
			return $ RelayDone $ ExitFailure 1

relayToPeer :: RelayData -> Net ()
relayToPeer (RelayDone exitcode) = sendMessage (CONNECTDONE exitcode)
relayToPeer (RelayToPeer b) = do
	let len = Len $ fromIntegral $ L.length b
	sendMessage (DATA len)
	sendBytes len b nullMeterUpdate
relayToPeer (RelayFromPeer _) = return ()
