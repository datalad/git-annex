{- git-annex presence log, pure operations
 -
 - Copyright 2010-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Presence.Pure where

import Data.Time.Clock.POSIX
import Data.Time
import System.Locale
import qualified Data.Map as M

import Common.Annex
import Utility.QuickCheck

data LogLine = LogLine {
	date :: POSIXTime,
	status :: LogStatus,
	info :: String
} deriving (Eq, Show)

data LogStatus = InfoPresent | InfoMissing
	deriving (Eq, Show, Bounded, Enum)

{- Parses a log file. Unparseable lines are ignored. -}
parseLog :: String -> [LogLine]
parseLog = mapMaybe parseline . lines
  where
	parseline l = LogLine
		<$> (utcTimeToPOSIXSeconds <$> parseTime defaultTimeLocale "%s%Qs" d)
		<*> parsestatus s
		<*> pure rest
	  where
		(d, pastd) = separate (== ' ') l
		(s, rest) = separate (== ' ') pastd
	parsestatus "1" = Just InfoPresent
	parsestatus "0" = Just InfoMissing
	parsestatus _ = Nothing

{- Generates a log file. -}
showLog :: [LogLine] -> String
showLog = unlines . map genline
  where
	genline (LogLine d s i) = unwords [show d, genstatus s, i]
	genstatus InfoPresent = "1"
	genstatus InfoMissing = "0"

{- Given a log, returns only the info that is are still in effect. -}
getLog :: String -> [String]
getLog = map info . filterPresent . parseLog

{- Returns the info from LogLines that are in effect. -}
filterPresent :: [LogLine] -> [LogLine]
filterPresent = filter (\l -> InfoPresent == status l) . compactLog

{- Compacts a set of logs, returning a subset that contains the current
 - status. -}
compactLog :: [LogLine] -> [LogLine]
compactLog = M.elems . foldr mapLog M.empty

type LogMap = M.Map String LogLine

{- Inserts a log into a map of logs, if the log has better (ie, newer)
 - information than the other logs in the map -}
mapLog :: LogLine -> LogMap -> LogMap
mapLog l m
	| better = M.insert i l m
	| otherwise = m
  where
	better = maybe True newer $ M.lookup i m
	newer l' = date l' <= date l
	i = info l

instance Arbitrary LogLine where
	arbitrary = LogLine
		<$> arbitrary
		<*> elements [minBound..maxBound]
		<*> arbitrary `suchThat` ('\n' `notElem`)

prop_parse_show_log :: [LogLine] -> Bool
prop_parse_show_log l = parseLog (showLog l) == l

