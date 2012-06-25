{- git-annex assistant push tracking
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Pushes where

import Common.Annex
import Utility.TSet

import Data.Time.Clock

type FailedPushChan = TSet FailedPush

data FailedPush = FailedPush
        { failedRemote :: Remote
        , failedTimeStamp :: UTCTime
        }

newFailedPushChan :: IO FailedPushChan
newFailedPushChan = newTSet

{- Gets all failed pushes. Blocks until there is at least one failed push. -}
getFailedPushes :: FailedPushChan -> IO [FailedPush]
getFailedPushes = getTSet

{- Puts failed pushes back into the channel.
 - Note: Original order is not preserved. -}
refillFailedPushes :: FailedPushChan -> [FailedPush] -> IO ()
refillFailedPushes = putTSet

{- Records a failed push in the channel. -}
recordFailedPush :: FailedPushChan -> FailedPush -> IO ()
recordFailedPush = putTSet1
