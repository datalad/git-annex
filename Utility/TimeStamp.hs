{- timestamp parsing and formatting
 -
 - Copyright 2015-2019 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.TimeStamp where

import Utility.PartialPrelude
import Utility.Misc

import Data.Time.Clock.POSIX
import Data.Time
import Data.Ratio
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8 (char, decimal, signed, isDigit_w8)
#if ! MIN_VERSION_time(1,5,0)
import System.Locale
#endif

{- Parses how POSIXTime shows itself: "1431286201.113452s"
 - (The "s" is included for historical reasons and is optional.)
 - Also handles the format with no decimal seconds. -}
parserPOSIXTime :: A.Parser POSIXTime
parserPOSIXTime = mkPOSIXTime
	<$> signed decimal
	<*> (declen <|> pure (0, 0))
	<* optional (char 's')
  where
	declen :: A.Parser (Integer, Int)
	declen = do
		_ <- char '.'
		b <- A.takeWhile isDigit_w8
		let len = B.length b
		d <- either fail pure $
			A.parseOnly (decimal <* A.endOfInput) b
		return (d, len)

{- Parses how POSIXTime shows itself: "1431286201.113452s"
 - Also handles the format with no fractional seconds. -}
parsePOSIXTime :: String -> Maybe POSIXTime
parsePOSIXTime = uncurry parsePOSIXTime' . separate (== '.')

{- Parses the integral and decimal part of a POSIXTime -}
parsePOSIXTime' :: String -> String -> Maybe POSIXTime
parsePOSIXTime' sn sd = do
	n <- readi sn
	let sd' = takeWhile (/= 's') sd
	if null sd'
		then return (fromIntegral n)
		else do
			d <- readi sd'
			return $ mkPOSIXTime n (d, length sd')
  where
	readi :: String -> Maybe Integer
	readi = readish

{- This implementation allows for higher precision in a POSIXTime than
 - supported by the system's Double, and avoids the complications of
 - floating point. -}
mkPOSIXTime :: Integer -> (Integer, Int) -> POSIXTime
mkPOSIXTime n (d, dlen)
	| n < 0 = fromIntegral n - fromRational r
	| otherwise = fromIntegral n + fromRational r
  where
	r = d % (10 ^ dlen)

formatPOSIXTime :: String -> POSIXTime -> String
formatPOSIXTime fmt t = formatTime defaultTimeLocale fmt (posixSecondsToUTCTime t)
