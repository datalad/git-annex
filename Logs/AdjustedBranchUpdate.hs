{- git-annex log file that indicates when the adjusted branch needs to be
 - updated due to changes in content availability.
 -
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Logs.AdjustedBranchUpdate (
	recordAdjustedBranchUpdateNeeded,
	recordAdjustedBranchUpdateFinished,
	isAdjustedBranchUpdateNeeded,
) where

import Annex.Common
import Logs.File
import Utility.TimeStamp

import qualified Data.ByteString.Lazy as L
import Data.Time.Clock.POSIX

-- | Updates the log to indicate that an update is needed.
recordAdjustedBranchUpdateNeeded :: Annex ()
recordAdjustedBranchUpdateNeeded = do
	now <- liftIO getPOSIXTime
	logf <- fromRepo gitAnnexAdjustedBranchUpdateLog
	lckf <- fromRepo gitAnnexAdjustedBranchUpdateLock
	-- Replace any other log entries, because an update is needed now,
	-- so an entry that says an update finished must be in the past.
	-- And, if there were clock skew, an entry that says an update is
	-- needed in the future would be wrong information.
	modifyLogFile logf lckf (const [formatAdjustLog True now])

-- | Called after an update has finished. The time is when the update
-- started. If recordAdjustedBranchUpdateNeeded was called during the
-- update, the log is left indicating that an update is still needed.
recordAdjustedBranchUpdateFinished :: POSIXTime -> Annex ()
recordAdjustedBranchUpdateFinished starttime = do
	now <- liftIO getPOSIXTime
	logf <- fromRepo gitAnnexAdjustedBranchUpdateLog
	lckf <- fromRepo gitAnnexAdjustedBranchUpdateLock
	modifyLogFile logf lckf (go now)
  where
	go now logged
		| null $ filter (isnewer now) $ mapMaybe parseAdjustLog logged =
			[formatAdjustLog False starttime]
		| otherwise = logged
	
	-- If the logged time is in the future, there was clock skew,
	-- so disregard that log entry.
	isnewer now (_, loggedtime) = 
		loggedtime >= starttime && loggedtime <= now

isAdjustedBranchUpdateNeeded :: Annex Bool
isAdjustedBranchUpdateNeeded = do
	logf <- fromRepo gitAnnexAdjustedBranchUpdateLog
	lckf <- fromRepo gitAnnexAdjustedBranchUpdateLock
	calcLogFile logf lckf Nothing go >>= return . \case
		Just b -> b
		-- No log, so assume an update is needed.
		-- This handles upgrades from before this log was written.
		Nothing -> True
  where
 	go l p = case parseAdjustLog l of
		Nothing -> p
		Just (b, _t) -> case p of
			Nothing -> Just b
			Just b' -> Just (b' || b)

formatAdjustLog :: Bool -> POSIXTime -> L.ByteString
formatAdjustLog b t = encodeBL (show t) <> " " <> if b then "1" else "0"

parseAdjustLog :: L.ByteString -> Maybe (Bool, POSIXTime)
parseAdjustLog l = 
	let (ts, bs) = separate (== ' ') (decodeBL l)
	in do
		b <- case bs of
			"1" -> Just True
			"0" -> Just False
			_ -> Nothing
		t <- parsePOSIXTime (encodeBS ts)
		return (b, t)
