{- git-annex assistant push tracking
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.Pushes where

import Annex.Common

import Control.Concurrent.STM
import Data.Time.Clock
import qualified Data.Map as M

{- Track the most recent push failure for each remote. -}
type PushMap = M.Map Remote UTCTime
type FailedPushMap = TMVar PushMap

{- The TMVar starts empty, and is left empty when there are no
 - failed pushes. This way we can block until there are some failed pushes.
 -}
newFailedPushMap :: IO FailedPushMap
newFailedPushMap = atomically newEmptyTMVar
