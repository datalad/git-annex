{- git-annex assistant out of band network messager types
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.NetMessager where

import Common.Annex

import Control.Concurrent.STM
import Control.Concurrent.MSampleVar

{- Messages that can be sent out of band by a network messager. -}
data NetMessage = NotifyPush [UUID]

{- Controls for the XMPP client. 
 -
 - It can be fed XMPP messages to send.
 -
 - It can also be sent a signal when it should restart for some reason. -}
data NetMessagerControl = NetMessagerControl
	{ netMessages :: TChan (NetMessage)
	, netMessagerRestart :: MSampleVar ()
	}

newNetMessagerControl :: IO NetMessagerControl
newNetMessagerControl = NetMessagerControl
	<$> atomically newTChan
	<*> newEmptySV
