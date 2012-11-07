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
	-- involving a client identified by the Text, and a UUID.
	| PairingNotification PairStage Text UUID
	-- request that a git push be sent over the out of band network
	| PushRequest
	-- indicates that a PushRequest has been seen and a push is starting
	| StartingPush
	-- a chunk of output of git receive-pack
	| ReceivePackOutput ByteString
	-- a chuck of output of git send-pack
	| SendPackOutput ByteString
	-- sent when git receive-pack exits, with its exit code
	| ReceivePackDone ExitCode
	deriving (Show)

data NetMessagerControl = NetMessagerControl
	{ netMessages :: TChan (NetMessage)
	, netMessagerRestart :: MSampleVar ()
	}

newNetMessagerControl :: IO NetMessagerControl
newNetMessagerControl = NetMessagerControl
	<$> atomically newTChan
	<*> newEmptySV
