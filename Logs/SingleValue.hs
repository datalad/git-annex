{-# LANGUAGE ScopedTypeVariables #-}

{- git-annex single-value log
 -
 - This is used to store a value in a way that can be union merged.
 -
 - A line of the log will look like: "timestamp value"
 -
 - The line with the newest timestamp wins.
 -
 - Copyright 2014-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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

readLog :: (Ord v, SingleValueSerializable v) => OsPath -> Annex (Log v)
readLog = parseLog <$$> Annex.Branch.get

getLog :: (Ord v, SingleValueSerializable v) => OsPath -> Annex (Maybe v)
getLog = newestValue <$$> readLog

setLog :: (Ord v, SingleValueSerializable v) => Annex.Branch.RegardingUUID -> OsPath -> v -> Annex ()
setLog ru f v = do
	c <- currentVectorClock
	Annex.Branch.change ru f $ \old ->
		let oldcs = map changed ((parseLog' old) `asTypeOf` [ent])
		    ent = LogEntry (advanceVectorClock c oldcs) v
		in buildLog (S.singleton ent)
