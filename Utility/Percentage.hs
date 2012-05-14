{- percentages
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Percentage (
	Percentage,
	percentage,
	showPercentage
) where

import Data.Ratio

newtype Percentage = Percentage (Ratio Integer)

instance Show Percentage where
	show = showPercentage 0

{- Normally the big number comes first. But 110% is allowed if desired. :) -}
percentage :: Integer -> Integer -> Percentage
percentage 0 _ = Percentage 0
percentage full have = Percentage $ have * 100 % full

{- Pretty-print a Percentage, with a specified level of precision. -}
showPercentage :: Int -> Percentage -> String
showPercentage precision (Percentage p)
	| precision == 0 || remainder == 0 = go $ show int
	| otherwise = go $ show int ++ "." ++ strip0s (show remainder)
	where
		go v = v ++ "%"
		int :: Integer
		(int, frac) = properFraction (fromRational p)
		remainder = floor (frac * multiplier) :: Integer
		strip0s = reverse . dropWhile (== '0') . reverse
		multiplier :: Float
		multiplier = 10 ** (fromIntegral precision)
