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

{- Messages that can be sent out of band by a network messager. -}
data NetMessage 
	-- indicate that pushes have been made to the repos with these uuids
	= NotifyPush [UUID]
	-- requests other clients to inform us of their presence
	| QueryPresence
	-- notification about a stage in the pairing process,
	-- involving a client, and a UUID.
	| PairingNotification PairStage ClientID UUID
	-- request that a git push be sent over the out of band network
	| PushRequest ClientID
	-- indicates that a push is starting
	| StartingPush ClientID
	-- a chunk of output of git receive-pack
	| ReceivePackOutput ClientID ByteString
	-- a chuck of output of git send-pack
	| SendPackOutput ClientID ByteString
	-- sent when git receive-pack exits, with its exit code
	| ReceivePackDone ClientID ExitCode
	deriving (Show)

{- Something used to identify a specific client to send the message to. -}
type ClientID = Text

data NetMessager = NetMessager
	{ netMessages :: TChan (NetMessage)
	, netMessagerRestart :: MSampleVar ()
	}

newNetMessager :: IO NetMessager
newNetMessager = NetMessager
	<$> atomically newTChan
	<*> newEmptySV
