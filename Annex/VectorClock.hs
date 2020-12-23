{- git-annex vector clocks
 -
 - We don't have a way yet to keep true distributed vector clocks.
 - The next best thing is a timestamp.
 -
 - Copyright 2017-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.VectorClock (
	module Annex.VectorClock,
	module Types.VectorClock,
) where

import Types.VectorClock
import Annex.Common
import qualified Annex
import Utility.TimeStamp

import Data.ByteString.Builder
import qualified Data.Attoparsec.ByteString.Lazy as A

currentVectorClock :: Annex VectorClock
currentVectorClock = liftIO =<< Annex.getState Annex.getvectorclock

-- Runs the action and uses the same vector clock throughout.
--
-- When the action modifies several files in the git-annex branch,
-- this can cause less space to be used, since the same vector clock
-- value is used, which can compress better.
--
-- However, this should not be used when running a long-duration action,
-- because the vector clock is based on the start of the action, and not on 
-- the later points where it writes changes. For example, if this were
-- used across downloads of several files, the location log information
-- would have an earlier vector clock than necessary, which might cause it
-- to be disregarded in favor of other information that was collected
-- at an earlier point in time than when the transfers completted and the
-- log was written.
reuseVectorClockWhile :: Annex a -> Annex a
reuseVectorClockWhile = bracket setup cleanup . const
  where
	setup = do
		origget <- Annex.getState Annex.getvectorclock
		vc <- liftIO origget
		use (pure vc)
		return origget

	cleanup origget = use origget

	use vc = Annex.changeState $ \s ->
		s { Annex.getvectorclock = vc }

formatVectorClock :: VectorClock -> String
formatVectorClock Unknown = "0"
formatVectorClock (VectorClock t) = show t

buildVectorClock :: VectorClock -> Builder
buildVectorClock = string7 . formatVectorClock

parseVectorClock :: String -> Maybe VectorClock
parseVectorClock t = VectorClock <$> parsePOSIXTime t

vectorClockParser :: A.Parser VectorClock
vectorClockParser = VectorClock <$> parserPOSIXTime
