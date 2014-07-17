{- percentages
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - License: BSD-2-clause
 -}

module Utility.Percentage (
	Percentage,
	percentage,
	showPercentage
) where

import Data.Ratio

import Utility.HumanNumber

newtype Percentage = Percentage (Ratio Integer)

instance Show Percentage where
	show = showPercentage 0

{- Normally the big number comes first. But 110% is allowed if desired. :) -}
percentage :: Integer -> Integer -> Percentage
percentage 0 _ = Percentage 0
percentage full have = Percentage $ have * 100 % full

{- Pretty-print a Percentage, with a specified level of precision. -}
showPercentage :: Int -> Percentage -> String
showPercentage precision (Percentage p) = v ++ "%"
  where
	v = showImprecise precision n
	n = fromRational p :: Double
