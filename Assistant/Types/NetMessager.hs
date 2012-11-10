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
import Data.Set as S

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

data PushRunning = NoPushRunning | SendPushRunning ClientID | ReceivePushRunning ClientID
	deriving (Eq)

isPushInitiation :: PushStage -> Bool
isPushInitiation CanPush = True
isPushInitiation PushRequest = True
isPushInitiation StartingPush = True
isPushInitiation _ = False

data NetMessager = NetMessager
	-- outgoing messages
	{ netMessages :: TChan (NetMessage)
	-- only one push can be running at a time, and this tracks it
	, netMessagerPushRunning :: TMVar (PushRunning)
	-- incoming messages relating to the currently running push
	, netMessagesPush :: TChan (NetMessage)
	-- incoming push messages that have been deferred to be processed later
	, netMessagesDeferredPush :: TMVar (S.Set NetMessage)
	-- write to this to restart the net messager
	, netMessagerRestart :: MSampleVar ()
	}

newNetMessager :: IO NetMessager
newNetMessager = NetMessager
	<$> atomically newTChan
	<*> atomically (newTMVar NoPushRunning)
	<*> atomically newTChan
	<*> atomically (newTMVar S.empty)
	<*> newEmptySV
