{- git-annex assistant out of band network messager types
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.NetMessager where

import Common.Annex
import Assistant.Pairing

import Data.Text (Text)
import Control.Concurrent.STM
import Control.Concurrent.MSampleVar
import Data.ByteString (ByteString)
import qualified Data.Set as S

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
	| ReceivePackOutput ByteString
	-- a chuck of output of git send-pack
	| SendPackOutput ByteString
	-- sent when git receive-pack exits, with its exit code
	| ReceivePackDone ExitCode
	deriving (Show, Eq, Ord)

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
pushDestinationSide (ReceivePackOutput _) = SendPack
pushDestinationSide (SendPackOutput _) = ReceivePack
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
	{ netMessages :: TChan (NetMessage)
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
	<*> newEmptySV
	<*> mkSideMap (newTMVar Nothing)
	<*> mkSideMap newTChan
	<*> mkSideMap (newTMVar S.empty)
  where
