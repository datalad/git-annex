{- types for stall detection
 -
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.StallDetection where

import Utility.DataUnits
import Utility.HumanTime
import Utility.Misc

-- Unless the given number of bytes have been sent over the given
-- amount of time, there's a stall.
data StallDetection = StallDetection ByteSize Duration
	deriving (Show)

-- Parse eg, "0KiB/60s"
parseStallDetection :: String -> Either String StallDetection
parseStallDetection s = 
	let (bs, ds) = separate (== '/') s
	in do
		b <- maybe 
			(Left $ "Unable to parse stall detection amount " ++ bs)
			Right
			(readSize dataUnits bs)
		d <- parseDuration ds
		return (StallDetection b d)
