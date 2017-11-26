{- Time for humans.
 -
 - Copyright 2012-2013 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.HumanTime (
	Duration(..),
	durationSince,
	durationToPOSIXTime,
	durationToDays,
	daysToDuration,
	parseDuration,
	fromDuration,
	prop_duration_roundtrips
) where

import Utility.PartialPrelude
import Utility.QuickCheck

import qualified Data.Map as M
import Data.Time.Clock
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Char
import Control.Applicative
import Prelude

newtype Duration = Duration { durationSeconds :: Integer }
	deriving (Eq, Ord, Read, Show)

durationSince :: UTCTime -> IO Duration
durationSince pasttime = do
	now <- getCurrentTime
	return $ Duration $ round $ diffUTCTime now pasttime

durationToPOSIXTime :: Duration -> POSIXTime
durationToPOSIXTime = fromIntegral . durationSeconds

durationToDays :: Duration -> Integer
durationToDays d = durationSeconds d `div` dsecs

daysToDuration :: Integer -> Duration
daysToDuration i = Duration $ i * dsecs

{- Parses a human-input time duration, of the form "5h", "1m", "5h1m", etc -}
parseDuration :: Monad m => String -> m Duration
parseDuration = maybe parsefail (return . Duration) . go 0
  where
	go n [] = return n
	go n s = do
		num <- readish s :: Maybe Integer
		case dropWhile isDigit s of
			(c:rest) -> do
				u <- M.lookup c unitmap
				go (n + num * u) rest
			_ -> return $ n + num
	parsefail = fail "duration parse error; expected eg \"5m\" or \"1h5m\""

fromDuration :: Duration -> String
fromDuration Duration { durationSeconds = d }
	| d == 0 = "0s"
	| otherwise = concatMap showunit $ go [] units d
  where
	showunit (u, n)
		| n > 0 = show n ++ [u]
		| otherwise = ""
	go c [] _ = reverse c
	go c ((u, n):us) v =
		let (q,r) = v `quotRem` n
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

-- Durations cannot be negative.
instance Arbitrary Duration where
	arbitrary = Duration <$> nonNegative arbitrary

prop_duration_roundtrips :: Duration -> Bool
prop_duration_roundtrips d = parseDuration (fromDuration d) == Just d
