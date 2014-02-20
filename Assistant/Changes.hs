{- git-annex assistant change tracking
 -
 - Copyright 2012-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Changes where

import Assistant.Common
import Assistant.Types.Changes
import Utility.TList

import Data.Time.Clock
import Control.Concurrent.STM

{- Handlers call this when they made a change that needs to get committed. -}
madeChange :: FilePath -> ChangeInfo -> Assistant (Maybe Change)
madeChange f t = Just <$> (Change <$> liftIO getCurrentTime <*> pure f <*> pure t)

noChange :: Assistant (Maybe Change)
noChange = return Nothing

{- Indicates an add needs to be done, but has not started yet. -}
pendingAddChange :: FilePath -> Assistant (Maybe Change)
pendingAddChange f = Just <$> (PendingAddChange <$> liftIO getCurrentTime <*> pure f)

{- Gets all unhandled changes.
 - Blocks until at least one change is made. -}
getChanges :: Assistant [Change]
getChanges = (atomically . getTList) <<~ changePool

{- Gets all unhandled changes, without blocking. -}
getAnyChanges :: Assistant [Change]
getAnyChanges = (atomically . takeTList) <<~ changePool

{- Puts unhandled changes back into the pool.
 - Note: Original order is not preserved. -}
refillChanges :: [Change] -> Assistant ()
refillChanges cs = (atomically . flip appendTList cs) <<~ changePool

{- Records a change to the pool. -}
recordChange :: Change -> Assistant ()
recordChange c = (atomically . flip snocTList c) <<~ changePool

recordChanges :: [Change] -> Assistant ()
recordChanges = refillChanges
