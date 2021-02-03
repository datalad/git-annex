{- types for stall detection
 -
 - Copyright 2020-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.StallDetection where

import Utility.DataUnits
import Utility.HumanTime
import Utility.Misc
import Git.Config

data StallDetection
	= StallDetection ByteSize Duration
	-- ^ Unless the given number of bytes have been sent over the given
	-- amount of time, there's a stall.
	| ProbeStallDetection
	-- ^ Used when unsure how frequently transfer progress is updated,
	-- or how fast data can be sent.
	| StallDetectionDisabled
	deriving (Show)

-- Parse eg, "0KiB/60s"
--
-- Also, it can be set to "true" (or other git config equivilants)
-- to enable ProbeStallDetection. 
-- And "false" (and other git config equivilants) explicitly
-- disable stall detection.
parseStallDetection :: String -> Either String (Maybe StallDetection)
parseStallDetection s = case isTrueFalse s of
	Nothing -> do
		let (bs, ds) = separate (== '/') s
		b <- maybe 
			(Left $ "Unable to parse stall detection amount " ++ bs)
			Right
			(readSize dataUnits bs)
		d <- parseDuration ds
		return (Just (StallDetection b d))
	Just True -> Right (Just ProbeStallDetection)
	Just False -> Right (Just StallDetectionDisabled)
