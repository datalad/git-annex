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
	-- Divisible 3 Daily is every day of the year evenly divisible by 3
	| Divisable Int Recurrance
  deriving (Eq, Show, Ord)

type WeekDay = Int
type MonthDay = Int
type YearDay = Int

data TimeOfDay
	= AnyTime
	| Hour Int
  deriving (Eq, Show, Ord)

data Duration = MinutesDuration Int
  deriving (Eq, Show, Ord)

fromRecurrance :: Recurrance -> String
fromRecurrance (Divisable n r) =
	fromRecurrance' (\u -> "on " ++ u ++ "s divisible by " ++ show n) r
fromRecurrance r = fromRecurrance' ("every " ++) r

fromRecurrance' :: (String -> String) -> Recurrance -> String
fromRecurrance' a Daily = a "day"
fromRecurrance' a (Weekly n) = onday n (a "week")
fromRecurrance' a (Monthly n) = onday n (a "month")
fromRecurrance' a (Yearly n) = onday n (a "year")
fromRecurrance' a (Divisable _n r) = fromRecurrance' a r -- not used

onday :: Int -> String -> String
onday n s = s ++ " on day " ++ show n

toRecurrance :: String -> Maybe Recurrance
toRecurrance s = case words s of
	("every":something:l) -> parse something l
	("on":something:"divisible":"by":sn:l) -> do
		r <- parse something l
		n <- readish sn
		if n > 0
			then Just $ Divisable n r
			else Nothing
	_ -> Nothing
  where
	parse "day" [] = Just Daily
	parse "week" l = withday Weekly l
	parse "month" l = withday Monthly l
	parse "year" l = withday Yearly l
	parse v l
		| "s" `isSuffixOf` v = parse (reverse $ drop 1 $ reverse v) l
		| otherwise = Nothing
	withday a ("on":"day":n:[]) = a <$> readish n
	withday _ _ = Nothing

fromTimeOfDay :: TimeOfDay -> String
fromTimeOfDay AnyTime = "any time"
fromTimeOfDay (Hour n) = "hour " ++ show n

toTimeOfDay :: String -> Maybe TimeOfDay
toTimeOfDay s = case words s of
	("any":"time":[]) -> Just AnyTime
	("hour":n:[]) -> Hour <$> readish n
	_ -> Nothing

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
		, Hour <$> nonNegative arbitrary
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
