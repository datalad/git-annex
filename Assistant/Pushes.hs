{- git-annex assistant push tracking
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Pushes where

import Common.Annex
import Control.Concurrent.SampleVar

import Data.Time.Clock
import qualified Data.Map as M

{- Track the most recent push failure for each remote. -}
type PushMap = M.Map Remote UTCTime
type FailedPushes = SampleVar PushMap

newFailedPushChan :: IO FailedPushChan
newFailedPushChan = newEmptySampleVar

{- Gets all failed pushes. Blocks until set. -}
getFailedPushes :: FailedPushChan -> IO PushMap
getFailedPushes = readSampleVar

{- Sets all failed pushes to passed PushMap -}
setFailedPushes :: FailedPushChan -> PushMap -> IO ()
setFailedPushes = writeSampleVar

{- Indicates a failure to push to a single remote. -}
failedPush :: FailedPushChan -> Remote -> IO ()
failedPush c r = 

{- Indicates that a remote was pushed to successfully. -}
successfulPush :: FailedPushChan -> Remote -> IO ()
successfulPush c r = 
