{- A pool of "git-annex transferkeys" processes available for use
 -
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.TransferrerPool where

import Common

import Control.Concurrent.STM hiding (check)

type TransferrerPool = TVar [TransferrerPoolItem]

type CheckTransferrer = IO Bool
type MkCheckTransferrer = IO (IO Bool)

{- Each item in the pool may have a transferrer running, and has an
 - IO action that can be used to check if it's still ok to use the
 - transferrer. -}
data TransferrerPoolItem = TransferrerPoolItem (Maybe Transferrer) CheckTransferrer

data Transferrer = Transferrer
	{ transferrerRead :: Handle
	, transferrerWrite :: Handle
	, transferrerHandle :: ProcessHandle
	}

newTransferrerPool :: IO TransferrerPool
newTransferrerPool = newTVarIO []

popTransferrerPool :: TransferrerPool -> STM (Maybe TransferrerPoolItem, Int)
popTransferrerPool p = do
	l <- readTVar p
	case l of
		[] -> return (Nothing, 0)
		(i:is) -> do
			writeTVar p is
			return $ (Just i, length is)

pushTransferrerPool :: TransferrerPool -> TransferrerPoolItem -> STM ()
pushTransferrerPool p i = do
	l <- readTVar p
	let l' = i:l
	writeTVar p l'

{- Note that making a CheckTransferrer may allocate resources,
 - such as a NotificationHandle, so it's important that the returned
 - TransferrerPoolItem is pushed into the pool, and not left to be
 - garbage collected. -}
mkTransferrerPoolItem :: MkCheckTransferrer -> Transferrer -> IO TransferrerPoolItem
mkTransferrerPoolItem mkcheck t = do
	check <- mkcheck
	return $ TransferrerPoolItem (Just t) check
