{- scheduled activities
 - 
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Scheduled (
	Schedule(..),
	Recurrance(..),
	ScheduledTime(..),
	NextTime(..),
	nextTime,
	fromSchedule,
	toSchedule,
	parseSchedule,
	prop_schedule_roundtrips
) where

import Common
import Utility.QuickCheck

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.OrdinalDate
import Data.Tuple.Utils

{- Some sort of scheduled event. -}
data Schedule = Schedule Recurrance ScheduledTime
  deriving (Eq, Read, Show, Ord)

data Recurrance
	= Daily
	| Weekly WeekDay
	| Monthly MonthDay
	| Yearly YearDay
	-- Days, Weeks, or Months of the year evenly divisible by a number.
	-- (Divisible Year is years evenly divisible by a number.)
	| Divisible Int Recurrance
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

{- Next time a Schedule should take effect. The NextTimeWindow is used
 - when a Schedule is allowed to start at some point within the window. -}
data NextTime
	= NextTimeExactly LocalTime
	| NextTimeWindow LocalTime LocalTime
  deriving (Eq, Read, Show)

nextTime :: Schedule -> Maybe LocalTime -> IO (Maybe NextTime)
nextTime schedule lasttime = do
	now <- getCurrentTime
	tz <- getTimeZone now
	return $ calcNextTime schedule lasttime $ utcToLocalTime tz now

{- Calculate the next time that fits a Schedule, based on the
 - last time it occurred, and the current time. -}
calcNextTime :: Schedule -> Maybe LocalTime -> LocalTime -> Maybe NextTime
calcNextTime (Schedule recurrance scheduledtime) lasttime currenttime
	| scheduledtime == AnyTime = do
		start <- findfromtoday
		return $ NextTimeWindow
			start
			(LocalTime (localDay start) (TimeOfDay 23 59 0))
	| otherwise = NextTimeExactly <$> findfromtoday
  where
  	findfromtoday = 
		LocalTime <$> nextday <*> pure nexttime
	  where
	  	nextday = findnextday recurrance afterday today
	  	today = localDay currenttime
		afterday = sameaslastday || toolatetoday
		toolatetoday = localTimeOfDay currenttime >= nexttime
		sameaslastday = (localDay <$> lasttime) == Just today
	nexttime = case scheduledtime of
		AnyTime -> TimeOfDay 0 0 0
		SpecificTime h m -> TimeOfDay h m 0
	findnextday r afterday day = case r of
		Daily
			| afterday -> Just $ addDays 1 day
			| otherwise -> Just day
		Weekly w
			| w < 0 || w > maxwday -> Nothing
			| w == wday day -> if afterday
				then Just $ addDays 7 day
				else Just day
			| otherwise -> Just $ 
				addDays (fromIntegral $ (w - wday day) `mod` 7) day
		Monthly m
			| m < 0 || m > maxmday -> Nothing
			-- TODO can be done more efficiently than recursing
			| m == mday day -> if afterday
				then findnextday r False (addDays 1 day)
				else Just day
			| otherwise -> findnextday r False (addDays 1 day)
		Yearly y
			| y < 0 || y > maxyday -> Nothing
			| y == yday day -> if afterday
				then findnextday r False (addDays 365 day)
				else Just day
			| otherwise -> findnextday r False (addDays 1 day)
		Divisible n r'@Daily -> handlediv n r' yday (Just maxyday)
		Divisible n r'@(Weekly _) -> handlediv n r' wnum (Just maxwnum)
		Divisible n r'@(Monthly _) -> handlediv n r' mnum (Just maxmnum)
		Divisible n r'@(Yearly _) -> handlediv n r' year Nothing
		Divisible _ r'@(Divisible _ _) -> findnextday r' afterday day
	  where
	  	handlediv n r' getval mmax
			| n > 0 && maybe True (n <=) mmax =
				findnextdaywhere r' (divisible n . getval) afterday day
			| otherwise = Nothing
	findnextdaywhere r p afterday day
		| maybe True p d = d
		| otherwise = maybe d (findnextdaywhere r p True) d
	  where
		d = findnextday r afterday day
	divisible n v = v `rem` n == 0

	-- extracting various quantities from a Day
	wday = thd3 . toWeekDate
	wnum = snd3 . toWeekDate
	mday = thd3 . toGregorian
	mnum = snd3 . toGregorian
	yday = snd . toOrdinalDate
	year = fromIntegral . fst . toOrdinalDate

	maxyday = 366 -- with leap days
	maxwnum = 53 -- some years have more than 52
	maxmday = 31
	maxmnum = 12
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

onday :: Int -> String -> String
onday n s = "on day " ++ show n ++ " of " ++ s

toRecurrance :: String -> Maybe Recurrance
toRecurrance s = case words s of
	("every":"day":[]) -> Just Daily
	("on":"day":sd:"of":"every":something:[]) -> parse something sd
	("days":"divisible":"by":sn:[]) -> 
		Divisible <$> getdivisor sn <*> pure Daily
	("on":"day":sd:"of":something:"divisible":"by":sn:[]) -> 
		Divisible
			<$> getdivisor sn
			<*> parse something sd
	_ -> Nothing
  where
	parse "week" sd = withday Weekly sd
	parse "month" sd = withday Monthly sd
	parse "year" sd = withday Yearly sd
	parse v sd
		| "s" `isSuffixOf` v = parse (reverse $ drop 1 $ reverse v) sd
		| otherwise = Nothing
	withday c sd = c <$> readish sd
	getdivisor sn = do
		n <- readish sn
		if n > 0
			then Just n
			else Nothing

fromScheduledTime :: ScheduledTime -> String
fromScheduledTime AnyTime = "any time"
fromScheduledTime (SpecificTime h m) = show h ++ ":" ++ pad 2 (show m)
  where
  	pad n s = take (n - length s) (repeat '0') ++ s

toScheduledTime :: String -> Maybe ScheduledTime
toScheduledTime "any time" = Just AnyTime
toScheduledTime s =
	let (h, m) = separate (== ':') s
	in SpecificTime <$> readish h <*> readish m

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
			<$> nonNegative arbitrary 
			<*> nonNegative arbitrary
		]

instance Arbitrary Recurrance where
	arbitrary = oneof
		[ pure Daily
		, Weekly <$> nonNegative arbitrary
		, Monthly <$> nonNegative arbitrary
		, Yearly <$> nonNegative arbitrary
		, Divisible
			<$> positive arbitrary
			<*> oneof -- no nested Divisibles
				[ pure Daily
				, Weekly <$> nonNegative arbitrary
				, Monthly <$> nonNegative arbitrary
				, Yearly <$> nonNegative arbitrary
				]
		]

prop_schedule_roundtrips :: Schedule -> Bool
prop_schedule_roundtrips s = toSchedule (fromSchedule s) == Just s
