{- git-annex presence log
 -
 - This is used to store presence information in the git-annex branch in
 - a way that can be union merged.
 -
 - A line of the log will look like: "date N INFO"
 - Where N=1 when the INFO is present, and 0 otherwise.
 - 
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module PresenceLog (
	LogStatus(..),
	addLog,
	readLog,
	parseLog,
	logNow,
	compactLog,
	currentLog,
	LogLine
) where

import Data.Time.Clock.POSIX
import Data.Time
import System.Locale
import qualified Data.Map as M

import Annex.Common
import qualified Annex.Branch

data LogLine = LogLine {
	date :: POSIXTime,
	status :: LogStatus,
	info :: String
} deriving (Eq)

data LogStatus = InfoPresent | InfoMissing | Undefined
	deriving (Eq)

instance Show LogStatus where
	show InfoPresent = "1"
	show InfoMissing = "0"
	show Undefined = "undefined"

instance Read LogStatus where
	readsPrec _ "1" = [(InfoPresent, "")]
	readsPrec _ "0" = [(InfoMissing, "")]
	readsPrec _ _   = [(Undefined, "")]

instance Show LogLine where
	show (LogLine d s i) = unwords [show d, show s, i]

instance Read LogLine where
	-- This parser is robust in that even unparsable log lines are
	-- read without an exception being thrown.
	-- Such lines have a status of Undefined.
	readsPrec _ string = 
		if length w >= 3
			then maybe bad good pdate
			else bad
		where
			w = words string
			s = read $ w !! 1
			i = w !! 2
			pdate :: Maybe UTCTime
			pdate = parseTime defaultTimeLocale "%s%Qs" $ head w

			good v = ret $ LogLine (utcTimeToPOSIXSeconds v) s i
			bad = ret $ LogLine 0 Undefined ""
			ret v = [(v, "")]

addLog :: FilePath -> LogLine -> Annex ()
addLog file line = Annex.Branch.change file $ \s -> 
	showLog $ compactLog (line : parseLog s)

{- Reads a log file.
 - Note that the LogLines returned may be in any order. -}
readLog :: FilePath -> Annex [LogLine]
readLog file = parseLog <$> Annex.Branch.get file

parseLog :: String -> [LogLine]
parseLog = filter parsable . map read . lines
	where
		-- some lines may be unparseable, avoid them
		parsable l = status l /= Undefined

{- Generates a log file. -}
showLog :: [LogLine] -> String
showLog = unlines . map show

{- Generates a new LogLine with the current date. -}
logNow :: LogStatus -> String -> Annex LogLine
logNow s i = do
	now <- liftIO getPOSIXTime
	return $ LogLine now s i

{- Reads a log and returns only the info that is still in effect. -}
currentLog :: FilePath -> Annex [String]
currentLog file = map info . filterPresent <$> readLog file

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
mapLog l m = 
	if better
		then M.insert i l m
		else m
	where
		better = maybe True newer $ M.lookup i m
		newer l' = date l' <= date l
		i = info l
