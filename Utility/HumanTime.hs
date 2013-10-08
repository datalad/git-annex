{- Time for humans.
 -
 - Copyright 2012-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.HumanTime (
	Duration(..),
	durationToPOSIXTime,
	parseDuration,
	fromDuration,
) where

import Utility.PartialPrelude
import Utility.Applicative

import Data.Time.Clock.POSIX (POSIXTime)
import Data.Char
import qualified Data.Map as M

newtype Duration = Duration { durationSeconds :: Integer }
  deriving (Eq, Ord, Read, Show)

durationToPOSIXTime :: Duration -> POSIXTime
durationToPOSIXTime = fromIntegral . durationSeconds

{- Parses a human-input time duration, of the form "5h", "1m", "5h1m", etc -}
parseDuration :: String -> Maybe Duration
parseDuration = Duration <$$> go 0
  where
  	go n [] = return n
  	go n s = do
		num <- readish s :: Maybe Integer
		let (c:rest) = dropWhile isDigit s
		u <- M.lookup c unitmap
		go (n + num * u) rest

fromDuration :: Duration -> String
fromDuration = concat . map showunit . go [] units . durationSeconds
  where
	showunit (u, n)
		| n > 0 = show n ++ [u]
		| otherwise = ""
	go c [] _ = reverse c
	go c ((u, n):us) d =
		let (q,r) = d `quotRem` n
		in go ((u, q):c) us r

units :: [(Char, Integer)]
units = 
	[ ('y', ysecs)
	, ('d', dsecs)
	, ('h', hsecs)
	, ('m', msecs)
	, ('s', 1)
	]

unitmap :: M.Map Char Integer
unitmap = M.fromList units

ysecs :: Integer
ysecs = dsecs * 365

dsecs :: Integer
dsecs = hsecs * 24

hsecs :: Integer
hsecs = msecs * 60

msecs :: Integer
msecs = 60
