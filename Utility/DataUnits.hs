{- data size display and parsing
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -
 -
 - And now a rant: 
 -
 - In the beginning, we had powers of two, and they were good.
 -
 - Disk drive manufacturers noticed that some powers of two were
 - sorta close to some powers of ten, and that rounding down to the nearest
 - power of ten allowed them to advertise their drives were bigger. This
 - was sorta annoying.
 -
 - Then drives got big. Really, really big. This was good.
 -
 - Except that the small rounding error perpretrated by the drive
 - manufacturers suffered the fate of a small error, and became a large
 - error. This was bad.
 -
 - So, a committee was formed. And it arrived at a committee-like decision,
 - which satisfied noone, confused everyone, and made the world an uglier
 - place. As with all committees, this was meh.
 -
 - And the drive manufacturers happily continued selling drives that are
 - increasingly smaller than you'd expect, if you don't count on your
 - fingers. But that are increasingly too big for anyone to much notice.
 - This caused me to need git-annex.
 -
 - Thus, I use units here that I loathe. Because if I didn't, people would
 - be confused that their drives seem the wrong size, and other people would
 - complain at me for not being standards compliant. And we call this
 - progress?
 -}

module Utility.DataUnits (
	dataUnits,
	storageUnits,
	memoryUnits,
	bandwidthUnits,
	oldSchoolUnits,
	Unit(..),
	ByteSize,

	roughSize,
	compareSizes,
	readSize
) where

import Data.List
import Data.Char

import Utility.HumanNumber

type ByteSize = Integer
type Name = String
type Abbrev = String
data Unit = Unit ByteSize Abbrev Name
	deriving (Ord, Show, Eq)

dataUnits :: [Unit]
dataUnits = storageUnits ++ memoryUnits

{- Storage units are (stupidly) powers of ten. -}
storageUnits :: [Unit]
storageUnits =
	[ Unit (p 8) "YB" "yottabyte"
	, Unit (p 7) "ZB" "zettabyte"
	, Unit (p 6) "EB" "exabyte"
	, Unit (p 5) "PB" "petabyte"
	, Unit (p 4) "TB" "terabyte"
	, Unit (p 3) "GB" "gigabyte"
	, Unit (p 2) "MB" "megabyte"
	, Unit (p 1) "kB" "kilobyte" -- weird capitalization thanks to committe
	, Unit (p 0) "B" "byte"
	]
  where
	p :: Integer -> Integer
	p n = 1000^n

{- Memory units are (stupidly named) powers of 2. -}
memoryUnits :: [Unit]
memoryUnits =
	[ Unit (p 8) "YiB" "yobibyte"
	, Unit (p 7) "ZiB" "zebibyte"
	, Unit (p 6) "EiB" "exbibyte"
	, Unit (p 5) "PiB" "pebibyte"
	, Unit (p 4) "TiB" "tebibyte"
	, Unit (p 3) "GiB" "gibibyte"
	, Unit (p 2) "MiB" "mebibyte"
	, Unit (p 1) "KiB" "kibibyte"
	, Unit (p 0) "B" "byte"
	]
  where
	p :: Integer -> Integer
	p n = 2^(n*10)

{- Bandwidth units are only measured in bits if you're some crazy telco. -}
bandwidthUnits :: [Unit]
bandwidthUnits = error "stop trying to rip people off"

{- Do you yearn for the days when men were men and megabytes were megabytes? -}
oldSchoolUnits :: [Unit]
oldSchoolUnits = zipWith (curry mingle) storageUnits memoryUnits
  where
	mingle (Unit _ a n, Unit s' _ _) = Unit s' a n

{- approximate display of a particular number of bytes -}
roughSize :: [Unit] -> Bool -> ByteSize -> String
roughSize units short i
	| i < 0 = '-' : findUnit units' (negate i)
	| otherwise = findUnit units' i
  where
	units' = sortBy (flip compare) units -- largest first

	findUnit (u@(Unit s _ _):us) i'
		| i' >= s = showUnit i' u
		| otherwise = findUnit us i'
	findUnit [] i' = showUnit i' (last units') -- bytes

	showUnit x (Unit size abbrev name) = s ++ " " ++ unit
	  where
		v = (fromInteger x :: Double) / fromInteger size
		s = showImprecise 2 v
		unit
			| short = abbrev
			| s == "1" = name
			| otherwise = name ++ "s"

{- displays comparison of two sizes -}
compareSizes :: [Unit] -> Bool -> ByteSize -> ByteSize -> String
compareSizes units abbrev old new
	| old > new = roughSize units abbrev (old - new) ++ " smaller"
	| old < new = roughSize units abbrev (new - old) ++ " larger"
	| otherwise = "same"

{- Parses strings like "10 kilobytes" or "0.5tb". -}
readSize :: [Unit] -> String -> Maybe ByteSize
readSize units input
	| null parsednum || null parsedunit = Nothing
	| otherwise = Just $ round $ number * fromIntegral multiplier
  where
	(number, rest) = head parsednum
	multiplier = head parsedunit
	unitname = takeWhile isAlpha $ dropWhile isSpace rest

	parsednum = reads input :: [(Double, String)]
	parsedunit = lookupUnit units unitname

	lookupUnit _ [] = [1] -- no unit given, assume bytes
	lookupUnit [] _ = []
	lookupUnit (Unit s a n:us) v
		| a ~~ v || n ~~ v = [s]
		| plural n ~~ v || a ~~ byteabbrev v = [s]
		| otherwise = lookupUnit us v
		
	a ~~ b = map toLower a == map toLower b
		
	plural n = n ++ "s"
	byteabbrev a = a ++ "b"
