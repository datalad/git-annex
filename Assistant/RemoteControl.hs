{- git-annex assistant RemoteDaemon control
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.RemoteControl (
	sendRemoteControl,
	RemoteDaemon.Consumed(..)
) where

import Assistant.Common
import qualified RemoteDaemon.Types as RemoteDaemon

import Control.Concurrent

sendRemoteControl :: RemoteDaemon.Consumed -> Assistant ()
sendRemoteControl msg = do
	clicker <- getAssistant remoteControl
	liftIO $ writeChan clicker msg
