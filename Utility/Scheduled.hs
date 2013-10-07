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
	fromSchedule,
	toSchedule
) where

import Common

{- Some sort of scheduled event. -}
data Schedule = Schedule Recurrance TimeOfDay Duration
  deriving (Show)

data Recurrance
	= Daily
	| Weekly WeekDay
	| Monthly MonthDay
	| Yearly YearDay
	-- Divisible 3 Daily is every day of the year evenly divisible by 3
	| Divisable Int Recurrance
  deriving (Show)

type WeekDay = Int
type MonthDay = Int
type YearDay = Int

data TimeOfDay
	= AnyTime
	| Hour Int
  deriving (Show)

data Duration = MinutesDuration Int
  deriving (Show)

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
onday n s = s ++ " on day " ++ show n

toRecurrance :: String -> Maybe Recurrance
toRecurrance s = case words s of
	("every":something:l) -> parse something l
	(something:"divisible":"by":sn:l) -> do
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
toSchedule s = Schedule
	<$> toRecurrance (unwords recurrance)
	<*> toTimeOfDay (unwords timeofday)
	<*> toDuration (unwords duration)
  where
  	ws = words s
	(recurrance, ws') = separate (== "at") ws
	(timeofday, duration) = separate (== "for") ws'

