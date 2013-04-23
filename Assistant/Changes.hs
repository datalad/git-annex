{- git-annex assistant change tracking
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Changes where

import Assistant.Common
import Assistant.Types.Changes
import Utility.TSet

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
getChanges = fmap concat $ (atomically . getTSet) <<~ changeChan

{- Gets all unhandled changes, without blocking. -}
getAnyChanges :: Assistant [Change]
getAnyChanges = fmap concat $ (atomically . readTSet) <<~ changeChan

{- Puts unhandled changes back into the channel.
 - Note: Original order is not preserved. -}
refillChanges :: [Change] -> Assistant ()
refillChanges cs = (atomically . flip putTSet1 cs) <<~ changeChan

{- Records a change in the channel. -}
recordChange :: Change -> Assistant ()
recordChange c = (atomically . flip putTSet1 [c]) <<~ changeChan
