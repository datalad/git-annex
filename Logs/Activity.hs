{- git-annex activity log
 -
 - Copyright 2015-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Activity (
	Log,
	Activity(..),
	allActivities,
	recordActivity,
	lastActivities,
) where

import Annex.Common
import qualified Annex.Branch
import Logs
import Logs.UUIDBased

import qualified Data.ByteString as S
import qualified Data.Attoparsec.ByteString as A
import Data.ByteString.Builder

data Activity 
	= Fsck
	-- Allow for unknown activities to be added later.
	| UnknownActivity S.ByteString
	deriving (Eq, Read, Show)

allActivities :: [Activity]
allActivities = [Fsck]

-- Record an activity. This takes the place of previously recorded activity
-- for the UUID.
recordActivity :: Activity -> UUID -> Annex ()
recordActivity act uuid = do
	c <- currentVectorClock
	Annex.Branch.change (Annex.Branch.RegardingUUID [uuid]) activityLog $
		buildLogOld buildActivity
			. changeLog c uuid act
			. parseLogOld parseActivity

-- Most recent activity for each UUID.
lastActivities :: Maybe Activity -> Annex (Log Activity)
lastActivities wantact = parseLogOld (onlywanted =<< parseActivity)
	<$> Annex.Branch.get activityLog
  where
	onlywanted a 
		| wanted a = pure a
		| otherwise = fail "unwanted activity"
	wanted a = maybe True (a ==) wantact

buildActivity :: Activity -> Builder
buildActivity (UnknownActivity b) = byteString b
buildActivity a = byteString $ encodeBS $ show a

parseActivity :: A.Parser Activity
parseActivity = go <$> A.takeByteString
  where
	go b = fromMaybe (UnknownActivity b) (readish $ decodeBS b)
