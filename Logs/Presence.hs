{- git-annex presence log
 -
 - This is used to store presence information in the git-annex branch in
 - a way that can be union merged.
 -
 - A line of the log will look like: "date N INFO"
 - Where N=1 when the INFO is present, 0 otherwise.
 - 
 - Copyright 2010-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Presence (
	module X,
	addLog,
	addLog',
	maybeAddLog,
	readLog,
	currentLog,
	currentLogInfo,
	historicalLogInfo,
) where

import Logs.Presence.Pure as X
import Annex.Common
import Annex.VectorClock
import qualified Annex.Branch
import Git.Types (RefDate)

{- Adds to the log, removing any LogLines that are obsoleted. -}
addLog :: Annex.Branch.RegardingUUID -> RawFilePath -> LogStatus -> LogInfo -> Annex ()
addLog ru file logstatus loginfo = 
	addLog' ru file logstatus loginfo =<< currentVectorClock

addLog' :: Annex.Branch.RegardingUUID -> RawFilePath -> LogStatus -> LogInfo -> CandidateVectorClock -> Annex ()
addLog' ru file logstatus loginfo c = 
	Annex.Branch.changeOrAppend ru file $ \b ->
		let old = parseLog b
		    line = genLine logstatus loginfo c old
		in if isNewInfo line old
			then Annex.Branch.Append $ buildLog [line]
			else Annex.Branch.Change $ buildLog $
				compactLog (line : old)

{- When a LogLine already exists with the same status and info, but an
 - older timestamp, that LogLine is preserved, rather than updating the log
 - with a newer timestamp.
 -}
maybeAddLog :: Annex.Branch.RegardingUUID -> RawFilePath -> LogStatus -> LogInfo -> Annex ()
maybeAddLog ru file logstatus loginfo = do
	c <- currentVectorClock
	Annex.Branch.maybeChange ru file $ \b ->
		let old = parseLog b
		    line = genLine logstatus loginfo c old
		in do
			m <- insertNewStatus line $ logMap old
			return $ buildLog $ mapLog m

genLine :: LogStatus -> LogInfo -> CandidateVectorClock -> [LogLine] -> LogLine
genLine logstatus loginfo c old = LogLine c' logstatus loginfo
  where
	oldcs = map date (filter (\l -> info l == loginfo) old)
	c' = advanceVectorClock c oldcs

{- Reads a log file.
 - Note that the LogLines returned may be in any order. -}
readLog :: RawFilePath -> Annex [LogLine]
readLog = parseLog <$$> Annex.Branch.get

{- Reads a log and returns only the info that is still in effect. -}
currentLogInfo :: RawFilePath -> Annex [LogInfo]
currentLogInfo file = map info <$> currentLog file

currentLog :: RawFilePath -> Annex [LogLine]
currentLog file = filterPresent <$> readLog file

{- Reads a historical version of a log and returns the info that was in
 - effect at that time. 
 -
 - The date is formatted as shown in gitrevisions man page.
 -}
historicalLogInfo :: RefDate -> RawFilePath -> Annex [LogInfo]
historicalLogInfo refdate file = map info . filterPresent . parseLog
	<$> Annex.Branch.getHistorical refdate file
