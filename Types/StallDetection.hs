{- types for stall detection and banwdith rates
 -
 - Copyright 2020-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.StallDetection where

import Utility.DataUnits
import Utility.HumanTime
import Utility.Misc
import Git.Config

data StallDetection
	= StallDetection BwRate
	-- ^ Unless the given number of bytes have been sent over the given
	-- amount of time, there's a stall.
	| ProbeStallDetection
	-- ^ Used when unsure how frequently transfer progress is updated,
	-- or how fast data can be sent.
	| StallDetectionDisabled
	deriving (Show)

data BwRate = BwRate ByteSize Duration
	deriving (Show)

-- Parse eg, "0KiB/60s"
--
-- Also, it can be set to "true" (or other git config equivalents)
-- to enable ProbeStallDetection. 
-- And "false" (and other git config equivalents) explicitly
-- disable stall detection.
parseStallDetection :: String -> Either String StallDetection
parseStallDetection s = case isTrueFalse s of
	Nothing -> do
		v <- parseBwRate s
		Right (StallDetection v)
	Just True -> Right ProbeStallDetection
	Just False -> Right StallDetectionDisabled

readStallDetection :: String -> Maybe StallDetection
readStallDetection = either (const Nothing) Just . parseStallDetection

parseBwRate :: String -> Either String BwRate
parseBwRate s = do
	let (bs, ds) = separate (== '/') s
	b <- maybe 
		(Left $ "Unable to parse bandwidth amount " ++ bs)
		Right
		(readSize dataUnits bs)
	d <- parseDuration ds
	Right (BwRate b d)

readBwRatePerSecond :: String -> Maybe BwRate
readBwRatePerSecond s = do
	sz <- readSize dataUnits s
	return (BwRate sz (Duration 1))
