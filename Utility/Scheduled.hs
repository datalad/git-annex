{- scheduled activities
 - 
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Scheduled (
	Schedule(..),
	Recurrance(..),
	TimeOfDay(..),
	Duration(..),
	fromSchedule,
	toSchedule,
	parseSchedule,
	prop_schedule_roundtrips
) where

import Common
import Utility.QuickCheck

{- Some sort of scheduled event. -}
data Schedule = Schedule Recurrance TimeOfDay Duration
  deriving (Eq, Show, Ord)

data Recurrance
	= Daily
	| Weekly WeekDay
	| Monthly MonthDay
	| Yearly YearDay
	| Divisable Int Recurrance
  deriving (Eq, Show, Ord)

type WeekDay = Int
type MonthDay = Int
type YearDay = Int

data TimeOfDay
	= AnyTime
	| SpecificTime Hour Minute
  deriving (Eq, Show, Ord)

type Hour = Int
type Minute = Int

data Duration = MinutesDuration Int
  deriving (Eq, Show, Ord)

fromRecurrance :: Recurrance -> String
fromRecurrance (Divisable n r) =
	fromRecurrance' (++ "s divisible by " ++ show n) r
fromRecurrance r = fromRecurrance' ("every " ++) r

fromRecurrance' :: (String -> String) -> Recurrance -> String
fromRecurrance' a Daily = a "day"
fromRecurrance' a (Weekly n) = onday n (a "week")
fromRecurrance' a (Monthly n) = onday n (a "month")
fromRecurrance' a (Yearly n) = onday n (a "year")
fromRecurrance' a (Divisable _n r) = fromRecurrance' a r -- not used

onday :: Int -> String -> String
onday n s = "on day " ++ show n ++ " of " ++ s

toRecurrance :: String -> Maybe Recurrance
toRecurrance s = case words s of
	("every":"day":[]) -> Just Daily
	("on":"day":sd:"of":"every":something:[]) -> parse something sd
	("days":"divisible":"by":sn:[]) -> 
		Divisable <$> getdivisor sn <*> pure Daily
	("on":"day":sd:"of":something:"divisible":"by":sn:[]) -> 
		Divisable
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

fromTimeOfDay :: TimeOfDay -> String
fromTimeOfDay AnyTime = "any time"
fromTimeOfDay (SpecificTime h m) = show h ++ ":" ++ show m

toTimeOfDay :: String -> Maybe TimeOfDay
toTimeOfDay "any time" = Just AnyTime
toTimeOfDay s =
	let (h, m) = separate (== ':') s
	in SpecificTime <$> readish h <*> readish m

fromDuration :: Duration -> String
fromDuration (MinutesDuration n) = show n ++ " minutes"

toDuration :: String -> Maybe Duration
toDuration s = case words s of
	(n:"minutes":[]) -> MinutesDuration <$> readish n
	(n:"minute":[]) -> MinutesDuration <$> readish n
	_ -> Nothing

fromSchedule :: Schedule -> String
fromSchedule (Schedule recurrance timeofday duration) = unwords
	[ fromRecurrance recurrance
	, "at"
	, fromTimeOfDay timeofday
	, "for"
	, fromDuration duration
	]

toSchedule :: String -> Maybe Schedule
toSchedule = eitherToMaybe . parseSchedule

parseSchedule :: String -> Either String Schedule
parseSchedule s = do
	r <- maybe (Left $ "bad recurrance: " ++ recurrance) Right
		(toRecurrance recurrance)
	t <- maybe (Left $ "bad time of day: " ++ timeofday) Right
		(toTimeOfDay timeofday)
	d <- maybe (Left $ "bad duration: " ++ duration) Right
		(toDuration duration)
	Right $ Schedule r t d
  where
  	ws = words s
	(rws, ws') = separate (== "at") ws
	(tws, dws) = separate (== "for") ws'
	recurrance = unwords rws
	timeofday = unwords tws
	duration = unwords dws

instance Arbitrary Schedule where
	arbitrary = Schedule <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Duration where
	arbitrary = MinutesDuration <$> nonNegative arbitrary

instance Arbitrary TimeOfDay where
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
		, Divisable
			<$> positive arbitrary
			<*> oneof -- no nested Divisibles
				[ pure Daily
				, Weekly <$> nonNegative arbitrary
				, Monthly <$> nonNegative arbitrary
				, Yearly <$> nonNegative arbitrary
				]
		]

prop_schedule_roundtrips :: Schedule -> Bool
prop_schedule_roundtrips s = case toSchedule $ fromSchedule s of
	Just s' | s == s' -> True
	_ -> False
