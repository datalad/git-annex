{- git-annex metadata log, pure operations
 -
 - Copyright 2014-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Logs.MetaData.Pure (
	Log,
	LogEntry(..),
	parseLog,
	buildLog,
	logToCurrentMetaData,
	simplifyLog,
	filterRemoteMetaData,
	filterOutEmpty,
) where

import Types.MetaData
import Logs.SingleValue.Pure
import Types.UUID

import qualified Data.Set as S
import qualified Data.Map.Strict as M

instance SingleValueSerializable MetaData where
	serialize = Types.MetaData.serialize
	deserialize = Types.MetaData.deserialize

logToCurrentMetaData :: [LogEntry MetaData] -> MetaData
logToCurrentMetaData = currentMetaData . combineMetaData . map value

{- Simplify a log, removing historical values that are no longer
 - needed. 
 -
 - This is not as simple as just making a single log line with the newest
 - state of all metadata. Consider this case:
 -
 - We have:
 -
 - 100 foo +x bar +y
 - 200 foo -x
 -
 - An unmerged remote has:
 -
 - 150 bar -y baz +w
 -
 - If what we have were simplified to "200 foo -x bar +y" then when the line
 - from the remote became available, it would be older than the simplified
 - line, and its change to bar would not take effect. That is wrong.
 -
 - Instead, simplify it to:
 -
 - 100 bar +y
 - 200 foo -x
 -
 - (Note that this ends up with the same number of lines as the
 - unsimplified version, so there's really no point in updating
 - the log to this version. Doing so would only add data to git,
 - with little benefit.)
 -
 - Now merging with the remote yields:
 -
 - 100 bar +y
 - 150 bar -y baz +w
 - 200 foo -x
 -
 - Simplifying again:
 -
 - 150 bar +z baz +w
 - 200 foo -x
 -}
simplifyLog :: Log MetaData -> Log MetaData
simplifyLog s = case sl of
	(newest:rest) -> 
		let sl' = go [newest] (value newest) rest
		in if length sl' < length sl
			then S.fromList sl'
			else s
	_ -> s
  where
	sl = S.toDescList s

	go c _ [] = c
	go c newer (l:ls)
		| unique == emptyMetaData = go c newer ls
		| otherwise = go (l { value = unique } : c)
			(unionMetaData unique newer) ls
	  where
		older = value l
		unique = older `differenceMetaData` newer

{- Filters per-remote metadata on the basis of UUID.
 -
 - Note that the LogEntry's clock is left the same, so this should not be
 - used except for in a transition.
 -}
filterRemoteMetaData :: (UUID -> Bool) -> Log MetaData -> Log MetaData
filterRemoteMetaData p = S.map go
  where
	go l@(LogEntry { value = MetaData m }) = 
		l { value = MetaData $ M.filterWithKey fil m }
	fil f _v = case splitRemoteMetaDataField f of
		Just (u, _) -> p u
		Nothing -> True

{- Filters out log lines that are empty. -}
filterOutEmpty :: Log MetaData -> Log MetaData
filterOutEmpty = S.filter $ \l -> value l /= emptyMetaData
