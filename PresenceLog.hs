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
	writeLog,
	logNow,
	compactLog,
	currentLog
) where

import Data.Time.Clock.POSIX
import Data.Time
import System.Locale
import qualified Data.Map as Map
import Control.Monad.State (liftIO)

import qualified Branch
import Types

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
addLog file line = do
	ls <- readLog file
	writeLog file (compactLog $ line:ls)

{- Reads a log file.
 - Note that the LogLines returned may be in any order. -}
readLog :: FilePath -> Annex [LogLine]
readLog file = return . parseLog =<< Branch.get file

parseLog :: String -> [LogLine]
parseLog s = filter parsable $ map read $ lines s
	where
		-- some lines may be unparseable, avoid them
		parsable l = status l /= Undefined

{- Stores a set of lines in a log file -}
writeLog :: FilePath -> [LogLine] -> Annex ()
writeLog file ls = Branch.change file (unlines $ map show ls)

{- Generates a new LogLine with the current date. -}
logNow :: LogStatus -> String -> Annex LogLine
logNow s i = do
	now <- liftIO getPOSIXTime
	return $ LogLine now s i

{- Reads a log and returns only the info that is still in effect. -}
currentLog :: FilePath -> Annex [String]
currentLog file = do
	ls <- readLog file
	return $ map info $ filterPresent ls

{- Returns the info from LogLines that are in effect. -}
filterPresent :: [LogLine] -> [LogLine]
filterPresent ls = filter (\l -> InfoPresent == status l) $ compactLog ls

type LogMap = Map.Map String LogLine

{- Compacts a set of logs, returning a subset that contains the current
 - status. -}
compactLog :: [LogLine] -> [LogLine]
compactLog = compactLog' Map.empty
compactLog' :: LogMap -> [LogLine] -> [LogLine]
compactLog' m [] = Map.elems m
compactLog' m (l:ls) = compactLog' (mapLog m l) ls

{- Inserts a log into a map of logs, if the log has better (ie, newer)
 - information than the other logs in the map -}
mapLog :: LogMap -> LogLine -> LogMap
mapLog m l = 
	if better
		then Map.insert i l m
		else m
	where
		better = maybe True (\l' -> date l' <= date l) $ Map.lookup i m
		i = info l
