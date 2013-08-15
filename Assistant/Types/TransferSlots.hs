{- git-annex assistant transfer slots
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE DeriveDataTypeable #-}

module Assistant.Types.TransferSlots where

import qualified Control.Exception as E
import qualified Control.Concurrent.MSemN as MSemN
import Data.Typeable

type TransferSlots = MSemN.MSemN Int

{- A special exception that can be thrown to pause or resume a transfer, while
 - keeping its slot in use. -}
data TransferException = PauseTransfer | ResumeTransfer
	deriving (Show, Eq, Typeable)

instance E.Exception TransferException

{- Number of concurrent transfers allowed to be run from the assistant.
 -
 - Transfers launched by other means, including by remote assistants,
 - do not currently take up slots.
 -}
numSlots :: Int
numSlots = 1

newTransferSlots :: IO TransferSlots
newTransferSlots = MSemN.new numSlots
