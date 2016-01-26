{- git-annex presence log
 -
 - This is used to store presence information in the git-annex branch in
 - a way that can be union merged.
 -
 - A line of the log will look like: "date N INFO"
 - Where N=1 when the INFO is present, 0 otherwise.
 - 
 - Copyright 2010-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Presence (
	module X,
	addLog,
	maybeAddLog,
	readLog,
	logNow,
	currentLog,
	currentLogInfo,
	historicalLogInfo,
) where

import Data.Time.Clock.POSIX

import Logs.Presence.Pure as X
import Annex.Common
import qualified Annex.Branch
import Git.Types (RefDate)

{- Adds a LogLine to the log, removing any LogLines that are obsoleted by
 - adding it. -}
addLog :: FilePath -> LogLine -> Annex ()
addLog file line = Annex.Branch.change file $ \s ->
	showLog $ compactLog (line : parseLog s)

{- When a LogLine already exists with the same status and info, but an
 - older timestamp, that LogLine is preserved, rather than updating the log
 - with a newer timestamp.
 -}
maybeAddLog :: FilePath -> LogLine -> Annex ()
maybeAddLog file line = Annex.Branch.maybeChange file $ \s -> do
	m <- insertNewStatus line $ logMap $ parseLog s
	return $ showLog $ mapLog m

{- Reads a log file.
 - Note that the LogLines returned may be in any order. -}
readLog :: FilePath -> Annex [LogLine]
readLog = parseLog <$$> Annex.Branch.get

{- Generates a new LogLine with the current date. -}
logNow :: LogStatus -> String -> Annex LogLine
logNow s i = do
	now <- liftIO getPOSIXTime
	return $ LogLine now s i

{- Reads a log and returns only the info that is still in effect. -}
currentLogInfo :: FilePath -> Annex [String]
currentLogInfo file = map info <$> currentLog file

currentLog :: FilePath -> Annex [LogLine]
currentLog file = filterPresent <$> readLog file

{- Reads a historical version of a log and returns the info that was in
 - effect at that time. 
 -
 - The date is formatted as shown in gitrevisions man page.
 -}
historicalLogInfo :: RefDate -> FilePath -> Annex [String]
historicalLogInfo refdate file = map info . filterPresent . parseLog
	<$> Annex.Branch.getHistorical refdate file
