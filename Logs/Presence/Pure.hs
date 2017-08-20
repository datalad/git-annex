{- git-annex presence log, pure operations
 -
 - Copyright 2010-2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Presence.Pure where

import Annex.Common
import Annex.VectorClock
import Logs.Line
import Utility.QuickCheck

import qualified Data.Map as M

data LogLine = LogLine
	{ date :: VectorClock
	, status :: LogStatus
	, info :: String
	} deriving (Eq)

instance Show LogLine where
	show l = "LogLine " ++ formatVectorClock (date l) ++ show (status l) ++ " " ++ show (info l)

data LogStatus = InfoPresent | InfoMissing | InfoDead
	deriving (Eq, Show, Bounded, Enum)

{- Parses a log file. Unparseable lines are ignored. -}
parseLog :: String -> [LogLine]
parseLog = mapMaybe parseline . splitLines
  where
	parseline l = LogLine
		<$> parseVectorClock c
		<*> parseStatus s
		<*> pure rest
	  where
		(c, pastc) = separate (== ' ') l
		(s, rest) = separate (== ' ') pastc

parseStatus :: String -> Maybe LogStatus
parseStatus "1" = Just InfoPresent
parseStatus "0" = Just InfoMissing
parseStatus "X" = Just InfoDead
parseStatus _ = Nothing

{- Generates a log file. -}
showLog :: [LogLine] -> String
showLog = unlines . map genline
  where
	genline (LogLine c s i) = unwords [formatVectorClock c, genstatus s, i]
	genstatus InfoPresent = "1"
	genstatus InfoMissing = "0"
	genstatus InfoDead = "X"

{- Given a log, returns only the info that is are still in effect. -}
getLog :: String -> [String]
getLog = map info . filterPresent . parseLog

{- Returns the info from LogLines that are in effect. -}
filterPresent :: [LogLine] -> [LogLine]
filterPresent = filter (\l -> InfoPresent == status l) . compactLog

{- Compacts a set of logs, returning a subset that contains the current
 - status. -}
compactLog :: [LogLine] -> [LogLine]
compactLog = mapLog . logMap

type LogMap = M.Map String LogLine

mapLog :: LogMap -> [LogLine]
mapLog = M.elems

logMap :: [LogLine] -> LogMap
logMap = foldr insertNewerLogLine M.empty

insertBetter :: (LogLine -> Bool) -> LogLine -> LogMap -> Maybe LogMap
insertBetter betterthan l m
	| better = Just (M.insert i l m)
	| otherwise = Nothing
  where
	better = maybe True betterthan (M.lookup i m)
	i = info l

{- Inserts a log into a map of logs, if the log has newer
 - information than the other logs in the map for the same info. -}
insertNewerLogLine :: LogLine -> LogMap -> LogMap
insertNewerLogLine l m = fromMaybe m $ insertBetter newer l m
  where
	newer l' = date l' <= date l

{- Inserts the log unless there's already one in the map with
 - the same status for its info, in which case there's no need to
 - change anything, to avoid log churn. -}
insertNewStatus :: LogLine -> LogMap -> Maybe LogMap
insertNewStatus l m  = insertBetter diffstatus l m
  where
	diffstatus l' = status l' /= status l

instance Arbitrary LogLine where
	arbitrary = LogLine
		<$> arbitrary
		<*> elements [minBound..maxBound]
		<*> arbitrary `suchThat`
			(\c -> '\n' `notElem` c && '\r' `notElem` c)

prop_parse_show_log :: [LogLine] -> Bool
prop_parse_show_log l = parseLog (showLog l) == l

