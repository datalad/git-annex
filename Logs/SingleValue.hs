{- git-annex single-value log
 -
 - This is used to store a value in a way that can be union merged.
 -
 - A line of the log will look like: "timestamp value"
 -
 - The line with the newest timestamp wins.
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.SingleValue (
	module Logs.SingleValue.Pure,
	readLog,
	getLog,
	setLog,
) where

import Annex.Common
import qualified Annex.Branch
import Logs.SingleValue.Pure
import Annex.VectorClock

import qualified Data.Set as S

readLog :: (Ord v, SingleValueSerializable v) => FilePath -> Annex (Log v)
readLog = parseLog . decodeBL <$$> Annex.Branch.get

getLog :: (Ord v, SingleValueSerializable v) => FilePath -> Annex (Maybe v)
getLog = newestValue <$$> readLog

setLog :: (SingleValueSerializable v) => FilePath -> v -> Annex ()
setLog f v = do
	c <- liftIO currentVectorClock
	let ent = LogEntry c v
	Annex.Branch.change f $ \_old -> encodeBL (showLog (S.singleton ent))
