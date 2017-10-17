{- git-annex assistant pending transfer queue
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.TransferQueue where

import Annex.Common
import Types.Transfer

import Control.Concurrent.STM
import Utility.TList

data TransferQueue = TransferQueue
	{ queuesize :: TVar Int
	, queuelist :: TList (Transfer, TransferInfo)
	, deferreddownloads :: TList (Key, AssociatedFile)
	}

data Schedule = Next | Later
	deriving (Eq)

newTransferQueue :: IO TransferQueue
newTransferQueue = atomically $ TransferQueue
	<$> newTVar 0
	<*> newTList
	<*> newTList
