{- git-annex assistant out of band network messager types
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.NetMessager where

import Common.Annex
import Assistant.Pairing

import Control.Concurrent.STM
import Control.Concurrent.MSampleVar
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M

{- Messages that can be sent out of band by a network messager. -}
data NetMessage 
	-- indicate that pushes have been made to the repos with these uuids
	= NotifyPush [UUID]
	-- requests other clients to inform us of their presence
	| QueryPresence
	-- notification about a stage in the pairing process,
	-- involving a client, and a UUID.
	| PairingNotification PairStage ClientID UUID
	-- used for git push over the network messager
	| Pushing ClientID PushStage
	deriving (Show, Eq, Ord)

{- Something used to identify the client, or clients to send the message to. -}
type ClientID = Text

data PushStage
	-- indicates that we have data to push over the out of band network
	= CanPush
	-- request that a git push be sent over the out of band network
	| PushRequest
	-- indicates that a push is starting
	| StartingPush
	-- a chunk of output of git receive-pack
	| ReceivePackOutput SequenceNum ByteString
	-- a chuck of output of git send-pack
	| SendPackOutput SequenceNum ByteString
	-- sent when git receive-pack exits, with its exit code
	| ReceivePackDone ExitCode
	deriving (Show, Eq, Ord)

{- A sequence number. Incremented by one per packet in a sequence,
 - starting with 1 for the first packet. 0 means sequence numbers are
 - not being used. -}
type SequenceNum = Int

{- NetMessages that are important (and small), and should be stored to be
 - resent when new clients are seen. -}
isImportantNetMessage :: NetMessage -> Maybe ClientID
isImportantNetMessage (Pushing c CanPush) = Just c
isImportantNetMessage (Pushing c PushRequest) = Just c
isImportantNetMessage _ = Nothing

readdressNetMessage :: NetMessage -> ClientID -> NetMessage
readdressNetMessage (PairingNotification stage _ uuid) c = PairingNotification stage c uuid
readdressNetMessage (Pushing _ stage) c = Pushing c stage
readdressNetMessage m _ = m

{- Convert a NetMessage to something that can be logged. -}
logNetMessage :: NetMessage -> String
logNetMessage (Pushing c stage) = show $ Pushing (logClientID c) $
	case stage of
		ReceivePackOutput n _ -> ReceivePackOutput n elided
		SendPackOutput n _ -> SendPackOutput n elided
		s -> s
  where
  	elided = B8.pack "<elided>"
logNetMessage (PairingNotification stage c uuid) =
	show $ PairingNotification stage (logClientID c) uuid
logNetMessage m = show m

logClientID :: ClientID -> ClientID
logClientID c = T.concat [T.take 1 c, T.pack $ show $ T.length c]

{- Things that initiate either side of a push, but do not actually send data. -}
isPushInitiation :: PushStage -> Bool
isPushInitiation CanPush = True
isPushInitiation PushRequest = True
isPushInitiation StartingPush = True
isPushInitiation _ = False

data PushSide = SendPack | ReceivePack
	deriving (Eq, Ord)

pushDestinationSide :: PushStage -> PushSide
pushDestinationSide CanPush = ReceivePack
pushDestinationSide PushRequest = SendPack
pushDestinationSide StartingPush = ReceivePack
pushDestinationSide (ReceivePackOutput _ _) = SendPack
pushDestinationSide (SendPackOutput _ _) = ReceivePack
pushDestinationSide (ReceivePackDone _) = SendPack

type SideMap a = PushSide -> a

mkSideMap :: STM a -> IO (SideMap a)
mkSideMap gen = do
	(sp, rp) <- atomically $ (,) <$> gen <*> gen
	return $ lookupside sp rp
  where
	lookupside sp _ SendPack = sp
	lookupside _ rp ReceivePack = rp

getSide :: PushSide -> SideMap a -> a
getSide side m = m side

data NetMessager = NetMessager
	-- outgoing messages
	{ netMessages :: TChan NetMessage
	-- important messages for each client
	, importantNetMessages :: TMVar (M.Map ClientID (S.Set NetMessage))
	-- important messages that are believed to have been sent to a client
	, sentImportantNetMessages :: TMVar (M.Map ClientID (S.Set NetMessage))
	-- write to this to restart the net messager
	, netMessagerRestart :: MSampleVar ()
	-- only one side of a push can be running at a time
	, netMessagerPushRunning :: SideMap (TMVar (Maybe ClientID))
	-- incoming messages related to a running push
	, netMessagesPush :: SideMap (TChan NetMessage)
	-- incoming push messages, deferred to be processed later
	, netMessagesPushDeferred :: SideMap (TMVar (S.Set NetMessage))
	}

newNetMessager :: IO NetMessager
newNetMessager = NetMessager
	<$> atomically newTChan
	<*> atomically (newTMVar M.empty)
	<*> atomically (newTMVar M.empty)
	<*> newEmptySV
	<*> mkSideMap (newTMVar Nothing)
	<*> mkSideMap newTChan
	<*> mkSideMap (newTMVar S.empty)
