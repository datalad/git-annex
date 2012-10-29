{- git-annex assistant push tracking
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.Pushes where

import Common.Annex
import Utility.TSet

import Control.Concurrent.STM
import Control.Concurrent.MSampleVar
import Data.Time.Clock
import qualified Data.Map as M

{- Track the most recent push failure for each remote. -}
type PushMap = M.Map Remote UTCTime
type FailedPushMap = TMVar PushMap

{- The TSet is recent, successful pushes that other remotes should be
 - notified about.
 -
 - The MSampleVar is written to when the PushNotifier thread should be
 - restarted for some reason.
 -}
data PushNotifier = PushNotifier
	{ pushNotifierSuccesses :: TSet UUID
	, pushNotifierWaiter :: MSampleVar ()
	}

{- The TMVar starts empty, and is left empty when there are no
 - failed pushes. This way we can block until there are some failed pushes.
 -}
newFailedPushMap :: IO FailedPushMap
newFailedPushMap = atomically newEmptyTMVar

newPushNotifier :: IO PushNotifier
newPushNotifier = PushNotifier
	<$> newTSet
	<*> newEmptySV
