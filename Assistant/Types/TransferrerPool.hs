{- A pool of "git-annex transferkeys" processes
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.TransferrerPool where

import Common.Annex

import Control.Concurrent.STM

type TransferrerPool = TChan Transferrer

data Transferrer = Transferrer
	{ transferrerRead :: Handle
	, transferrerWrite :: Handle
	, transferrerHandle :: ProcessHandle
	}

newTransferrerPool :: IO TransferrerPool
newTransferrerPool = newTChanIO
