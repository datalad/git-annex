{- numbers for humans
 -
 - Copyright 2012-2013 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.HumanNumber where

{- Displays a fractional value as a string with a limited number
 - of decimal digits. -}
showImprecise :: RealFrac a => Int -> a -> String
showImprecise precision n
	| precision == 0 || remainder == 0 = show (round n :: Integer)
	| otherwise = show int ++ "." ++ striptrailing0s (pad0s $ show remainder)
  where
	int :: Integer
	(int, frac) = properFraction n
	remainder = round (frac * 10 ^ precision) :: Integer
	pad0s s = replicate (precision - length s) '0' ++ s
	striptrailing0s = reverse . dropWhile (== '0') . reverse
