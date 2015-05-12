{- scheduled activities
 - 
 - Copyright 2013-2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Scheduled (
	Schedule(..),
	Recurrance(..),
	ScheduledTime(..),
	NextTime(..),
	WeekDay,
	MonthDay,
	YearDay,
	nextTime,
	calcNextTime,
	startTime,
	fromSchedule,
	fromScheduledTime,
	toScheduledTime,
	fromRecurrance,
	toRecurrance,
	toSchedule,
	parseSchedule,
	prop_schedule_roundtrips,
	prop_past_sane,
) where

import Utility.Data
import Utility.QuickCheck
import Utility.PartialPrelude
import Utility.Misc

import Data.List
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.OrdinalDate
import Data.Tuple.Utils
import Data.Char
import Control.Applicative
import Prelude

{- Some sort of scheduled event. -}
data Schedule = Schedule Recurrance ScheduledTime
	deriving (Eq, Read, Show, Ord)

data Recurrance
	= Daily
	| Weekly (Maybe WeekDay)
	| Monthly (Maybe MonthDay)
	| Yearly (Maybe YearDay)
	| Divisible Int Recurrance
	-- ^ Days, Weeks, or Months of the year evenly divisible by a number.
	-- (Divisible Year is years evenly divisible by a number.)
	deriving (Eq, Read, Show, Ord)

type WeekDay = Int
type MonthDay = Int
type YearDay = Int

data ScheduledTime
	= AnyTime
	| SpecificTime Hour Minute
	deriving (Eq, Read, Show, Ord)

type Hour = Int
type Minute = Int

-- | Next time a Schedule should take effect. The NextTimeWindow is used
-- when a Schedule is allowed to start at some point within the window.
data NextTime
	= NextTimeExactly LocalTime
	| NextTimeWindow LocalTime LocalTime
	deriving (Eq, Read, Show)

startTime :: NextTime -> LocalTime
startTime (NextTimeExactly t) = t
startTime (NextTimeWindow t _) = t

nextTime :: Schedule -> Maybe LocalTime -> IO (Maybe NextTime)
nextTime schedule lasttime = do
	now <- getCurrentTime
	tz <- getTimeZone now
	return $ calcNextTime schedule lasttime $ utcToLocalTime tz now

-- | Calculate the next time that fits a Schedule, based on the
-- last time it occurred, and the current time.
calcNextTime :: Schedule -> Maybe LocalTime -> LocalTime -> Maybe NextTime
calcNextTime schedule@(Schedule recurrance scheduledtime) lasttime currenttime
	| scheduledtime == AnyTime = do
		next <- findfromtoday True
		return $ case next of
			NextTimeWindow _ _ -> next
			NextTimeExactly t -> window (localDay t) (localDay t)
	| otherwise = NextTimeExactly . startTime <$> findfromtoday False
  where
	findfromtoday anytime = findfrom recurrance afterday today
	  where
		today = localDay currenttime
		afterday = sameaslastrun || toolatetoday
		toolatetoday = not anytime && localTimeOfDay currenttime >= nexttime
		sameaslastrun = lastrun == Just today
	lastrun = localDay <$> lasttime
	nexttime = case scheduledtime of
		AnyTime -> TimeOfDay 0 0 0
		SpecificTime h m -> TimeOfDay h m 0
	exactly d = NextTimeExactly $ LocalTime d nexttime
	window startd endd = NextTimeWindow
		(LocalTime startd nexttime)
		(LocalTime endd (TimeOfDay 23 59 0))
	findfrom r afterday candidate
		| ynum candidate > (ynum (localDay currenttime)) + 100 =
			-- avoid possible infinite recusion
			error $ "bug: calcNextTime did not find a time within 100 years to run " ++
			show (schedule, lasttime, currenttime)
		| otherwise = findfromChecked r afterday candidate
	findfromChecked r afterday candidate = case r of
		Daily
			| afterday -> Just $ exactly $ addDays 1 candidate
			| otherwise -> Just $ exactly candidate
		Weekly Nothing
			| afterday -> skip 1
			| otherwise -> case (wday <$> lastrun, wday candidate) of
				(Nothing, _) -> Just $ window candidate (addDays 6 candidate)
				(Just old, curr)
					| old == curr -> Just $ window candidate (addDays 6 candidate)
					| otherwise -> skip 1
		Monthly Nothing
			| afterday -> skip 1
			| maybe True (candidate `oneMonthPast`) lastrun ->
				Just $ window candidate (endOfMonth candidate)
			| otherwise -> skip 1
		Yearly Nothing
			| afterday -> skip 1
			| maybe True (candidate `oneYearPast`) lastrun ->
				Just $ window candidate (endOfYear candidate)
			| otherwise -> skip 1
		Weekly (Just w)
			| w < 0 || w > maxwday -> Nothing
			| w == wday candidate -> if afterday
				then Just $ exactly $ addDays 7 candidate
				else Just $ exactly candidate
			| otherwise -> Just $ exactly $
				addDays (fromIntegral $ (w - wday candidate) `mod` 7) candidate
		Monthly (Just m)
			| m < 0 || m > maxmday -> Nothing
			-- TODO can be done more efficiently than recursing
			| m == mday candidate -> if afterday
				then skip 1
				else Just $ exactly candidate
			| otherwise -> skip 1
		Yearly (Just y)
			| y < 0 || y > maxyday -> Nothing
			| y == yday candidate -> if afterday
				then skip 365
				else Just $ exactly candidate
			| otherwise -> skip 1
		Divisible n r'@Daily -> handlediv n r' yday (Just maxyday)
		Divisible n r'@(Weekly _) -> handlediv n r' wnum (Just maxwnum)
		Divisible n r'@(Monthly _) -> handlediv n r' mnum (Just maxmnum)
		Divisible n r'@(Yearly _) -> handlediv n r' ynum Nothing
		Divisible _ r'@(Divisible _ _) -> findfrom r' afterday candidate
	  where
		skip n = findfrom r False (addDays n candidate)
		handlediv n r' getval mmax
			| n > 0 && maybe True (n <=) mmax =
				findfromwhere r' (divisible n . getval) afterday candidate
			| otherwise = Nothing
	findfromwhere r p afterday candidate
		| maybe True (p . getday) next = next
		| otherwise = maybe Nothing (findfromwhere r p True . getday) next
	  where
		next = findfrom r afterday candidate
		getday = localDay . startTime
	divisible n v = v `rem` n == 0

-- Check if the new Day occurs one month or more past the old Day.
oneMonthPast :: Day -> Day -> Bool
new `oneMonthPast` old = fromGregorian y (m+1) d <= new
  where
	(y,m,d) = toGregorian old

-- Check if the new Day occurs one year or more past the old Day.
oneYearPast :: Day -> Day -> Bool
new `oneYearPast` old = fromGregorian (y+1) m d <= new
  where
	(y,m,d) = toGregorian old

endOfMonth :: Day -> Day
endOfMonth day =
	let (y,m,_d) = toGregorian day
	in fromGregorian y m (gregorianMonthLength y m)

endOfYear :: Day -> Day
endOfYear day =
	let (y,_m,_d) = toGregorian day
	in endOfMonth (fromGregorian y maxmnum 1)

-- extracting various quantities from a Day
wday :: Day -> Int
wday = thd3 . toWeekDate
wnum :: Day -> Int
wnum = snd3 . toWeekDate
mday :: Day -> Int
mday = thd3 . toGregorian
mnum :: Day -> Int
mnum = snd3 . toGregorian
yday :: Day -> Int
yday = snd . toOrdinalDate
ynum :: Day -> Int
ynum = fromIntegral . fst . toOrdinalDate

-- Calendar max values.
maxyday :: Int
maxyday = 366 -- with leap days
maxwnum :: Int
maxwnum = 53 -- some years have more than 52
maxmday :: Int
maxmday = 31
maxmnum :: Int
maxmnum = 12
maxwday :: Int
maxwday = 7

fromRecurrance :: Recurrance -> String
fromRecurrance (Divisible n r) =
	fromRecurrance' (++ "s divisible by " ++ show n) r
fromRecurrance r = fromRecurrance' ("every " ++) r

fromRecurrance' :: (String -> String) -> Recurrance -> String
fromRecurrance' a Daily = a "day"
fromRecurrance' a (Weekly n) = onday n (a "week")
fromRecurrance' a (Monthly n) = onday n (a "month")
fromRecurrance' a (Yearly n) = onday n (a "year")
fromRecurrance' a (Divisible _n r) = fromRecurrance' a r -- not used

onday :: Maybe Int -> String -> String
onday (Just n) s = "on day " ++ show n ++ " of " ++ s
onday Nothing s = s

toRecurrance :: String -> Maybe Recurrance
toRecurrance s = case words s of
	("every":"day":[]) -> Just Daily
	("on":"day":sd:"of":"every":something:[]) -> withday sd something
	("every":something:[]) -> noday something
	("days":"divisible":"by":sn:[]) -> 
		Divisible <$> getdivisor sn <*> pure Daily
	("on":"day":sd:"of":something:"divisible":"by":sn:[]) -> 
		Divisible
			<$> getdivisor sn
			<*> withday sd something
	("every":something:"divisible":"by":sn:[]) -> 
		Divisible
			<$> getdivisor sn
			<*> noday something
	(something:"divisible":"by":sn:[]) -> 
		Divisible
			<$> getdivisor sn
			<*> noday something
	_ -> Nothing
  where
	constructor "week" = Just Weekly
	constructor "month" = Just Monthly
	constructor "year" = Just Yearly
	constructor u
		| "s" `isSuffixOf` u = constructor $ reverse $ drop 1 $ reverse u
		| otherwise = Nothing
	withday sd u = do
		c <- constructor u
		d <- readish sd
		Just $ c (Just d)
	noday u = do
		c <- constructor u
		Just $ c Nothing
	getdivisor sn = do
		n <- readish sn
		if n > 0
			then Just n
			else Nothing

fromScheduledTime :: ScheduledTime -> String
fromScheduledTime AnyTime = "any time"
fromScheduledTime (SpecificTime h m) = 
	show h' ++ (if m > 0 then ":" ++ pad 2 (show m) else "") ++ " " ++ ampm
  where
	pad n s = take (n - length s) (repeat '0') ++ s
	(h', ampm)
		| h == 0 = (12, "AM")
		| h < 12 = (h, "AM")
		| h == 12 = (h, "PM")
		| otherwise = (h - 12, "PM")

toScheduledTime :: String -> Maybe ScheduledTime
toScheduledTime "any time" = Just AnyTime
toScheduledTime v = case words v of
	(s:ampm:[])
		| map toUpper ampm == "AM" ->
			go s h0
		| map toUpper ampm == "PM" ->
			go s (\h -> (h0 h) + 12)
		| otherwise -> Nothing
	(s:[]) -> go s id
	_ -> Nothing
  where
	h0 h
		| h == 12 = 0
		| otherwise = h
	go :: String -> (Int -> Int) -> Maybe ScheduledTime
	go s adjust =
		let (h, m) = separate (== ':') s
		in SpecificTime
			<$> (adjust <$> readish h)
			<*> if null m then Just 0 else readish m

fromSchedule :: Schedule -> String
fromSchedule (Schedule recurrance scheduledtime) = unwords
	[ fromRecurrance recurrance
	, "at"
	, fromScheduledTime scheduledtime
	]

toSchedule :: String -> Maybe Schedule
toSchedule = eitherToMaybe . parseSchedule

parseSchedule :: String -> Either String Schedule
parseSchedule s = do
	r <- maybe (Left $ "bad recurrance: " ++ recurrance) Right
		(toRecurrance recurrance)
	t <- maybe (Left $ "bad time of day: " ++ scheduledtime) Right
		(toScheduledTime scheduledtime)
	Right $ Schedule r t
  where
	(rws, tws) = separate (== "at") (words s)
	recurrance = unwords rws
	scheduledtime = unwords tws

instance Arbitrary Schedule where
	arbitrary = Schedule <$> arbitrary <*> arbitrary

instance Arbitrary ScheduledTime where
	arbitrary = oneof
		[ pure AnyTime
		, SpecificTime 
			<$> choose (0, 23)
			<*> choose (1, 59)
		]

instance Arbitrary Recurrance where
	arbitrary = oneof
		[ pure Daily
		, Weekly <$> arbday
		, Monthly <$> arbday
		, Yearly <$> arbday
		, Divisible
			<$> positive arbitrary
			<*> oneof -- no nested Divisibles
				[ pure Daily
				, Weekly <$> arbday
				, Monthly <$> arbday
				, Yearly <$> arbday
				]
		]
	  where
		arbday = oneof
			[ Just <$> nonNegative arbitrary
			, pure Nothing
			]

prop_schedule_roundtrips :: Schedule -> Bool
prop_schedule_roundtrips s = toSchedule (fromSchedule s) == Just s

prop_past_sane :: Bool
prop_past_sane = and
	[ all (checksout oneMonthPast) (mplus1 ++ yplus1)
	, all (not . (checksout oneMonthPast)) (map swap (mplus1 ++ yplus1))
	, all (checksout oneYearPast) yplus1
	, all (not . (checksout oneYearPast)) (map swap yplus1)
	]
  where
	mplus1 =   -- new date               old date, 1+ months before it
		[ (fromGregorian 2014 01 15, fromGregorian 2013 12 15)
		, (fromGregorian 2014 01 15, fromGregorian 2013 02 15)
		, (fromGregorian 2014 02 15, fromGregorian 2013 01 15)
		, (fromGregorian 2014 03 01, fromGregorian 2013 01 15)
		, (fromGregorian 2014 03 01, fromGregorian 2013 12 15)
		, (fromGregorian 2015 01 01, fromGregorian 2010 01 01)
		]
	yplus1 =   -- new date               old date, 1+ years before it
		[ (fromGregorian 2014 01 15, fromGregorian 2012 01 16)
		, (fromGregorian 2014 01 15, fromGregorian 2013 01 14)
		, (fromGregorian 2022 12 31, fromGregorian 2000 01 01)
		]
	checksout cmp (new, old) = new `cmp` old
	swap (a,b) = (b,a)
