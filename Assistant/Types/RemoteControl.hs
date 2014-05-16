{- git-annex assistant RemoteDaemon control
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.RemoteControl where

import qualified RemoteDaemon.Types as RemoteDaemon
import Control.Concurrent

type RemoteControl = Chan RemoteDaemon.Consumed

newRemoteControl :: IO RemoteControl
newRemoteControl = newChan
