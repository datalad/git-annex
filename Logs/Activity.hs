{- git-annex activity log
 -
 - Copyright 2015-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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

import qualified Data.ByteString as S
import qualified Data.Attoparsec.ByteString as A
import Data.ByteString.Builder

data Activity 
	= Fsck
	deriving (Eq, Read, Show, Enum, Bounded)

recordActivity :: Activity -> UUID -> Annex ()
recordActivity act uuid = do
	c <- currentVectorClock
	Annex.Branch.change activityLog $
		buildLogOld buildActivity
			. changeLog c uuid (Right act)
			. parseLogOld parseActivity

lastActivities :: Maybe Activity -> Annex (Log Activity)
lastActivities wantact = parseLogOld (onlywanted =<< parseActivity)
	<$> Annex.Branch.get activityLog
  where
	onlywanted (Right a) | wanted a = pure a
	onlywanted _ = fail "unwanted activity"
	wanted a = maybe True (a ==) wantact

buildActivity :: Either S.ByteString Activity -> Builder
buildActivity (Right a) = byteString $ encodeBS $ show a
buildActivity (Left b) = byteString b

-- Allow for unknown activities to be added later by preserving them.
parseActivity :: A.Parser (Either S.ByteString Activity)
parseActivity = go <$> A.takeByteString
  where
	go b = maybe (Left b) Right $ readish $ decodeBS b
