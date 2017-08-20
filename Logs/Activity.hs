{- git-annex activity log
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Activity (
	Log,
	Activity(..),
	recordActivity,
	lastActivities,
) where

import Annex.Common
import qualified Annex.Branch
import Logs
import Logs.UUIDBased

data Activity = Fsck
	deriving (Eq, Read, Show, Enum, Bounded)

recordActivity :: Activity -> UUID -> Annex ()
recordActivity act uuid = do
	c <- liftIO currentVectorClock
	Annex.Branch.change activityLog $
		showLog show . changeLog c uuid act . parseLog readish

lastActivities :: Maybe Activity -> Annex (Log Activity)
lastActivities wantact = parseLog onlywanted <$> Annex.Branch.get activityLog
  where
	onlywanted s = case readish s of
		Just a | wanted a -> Just a
		_ -> Nothing
	wanted a = maybe True (a ==) wantact
