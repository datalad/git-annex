{- git-annex log file names
 -
 - Copyright 2013-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Logs where

import Annex.Common
import Annex.DirHashes

import qualified Data.ByteString as S
import qualified System.FilePath.ByteString as P

{- There are several varieties of log file formats. -}
data LogVariety
	= OldUUIDBasedLog
	| NewUUIDBasedLog
	| ChunkLog Key
	| LocationLog Key
	| UrlLog Key
	| RemoteMetaDataLog
	| OtherLog
	deriving (Show)

{- Converts a path from the git-annex branch into one of the varieties
 - of logs used by git-annex, if it's a known path. -}
getLogVariety :: GitConfig -> RawFilePath -> Maybe LogVariety
getLogVariety config f
	| f `elem` topLevelOldUUIDBasedLogs = Just OldUUIDBasedLog
	| f `elem` topLevelNewUUIDBasedLogs = Just NewUUIDBasedLog
	| isRemoteStateLog f = Just NewUUIDBasedLog
	| isRemoteContentIdentifierLog f = Just NewUUIDBasedLog
	| isRemoteMetaDataLog f = Just RemoteMetaDataLog
	| isMetaDataLog f
		|| f `elem` otherTopLevelLogs
		|| isEquivilantKeyLog f = Just OtherLog
	| otherwise = (LocationLog <$> locationLogFileKey config f)
		<|> (ChunkLog <$> extLogFileKey chunkLogExt f)
		<|> (UrlLog  <$> urlLogFileKey f)

{- Typical number of log files that may be read while processing a single
 - key. This is used to size a cache.
 -
 - The location log is generally read, and the metadata log is read when
 - matching a preferred content expression that matches on metadata,
 - or when using metadata options.
 -
 - When using a remote, the url log, chunk log, remote state log, remote
 - metadata log, and remote content identifier log might each be used,
 - but probably at most 3 out of the 6. However, caching too much slows
 - down all operations because the cache is a linear list, so the cache
 - is not currently sized to include these.
 -
 - The result is that when seeking for files to operate on,
 - the location log will stay in the cache if the metadata log is also
 - read.
 -}
logFilesToCache :: Int
logFilesToCache = 2

{- All the log files that might contain information about a key. -}
keyLogFiles :: GitConfig -> Key -> [RawFilePath]
keyLogFiles config k = 
	[ locationLogFile config k
	, urlLogFile config k
	, remoteStateLogFile config k
	, metaDataLogFile config k
	, remoteMetaDataLogFile config k
	, remoteContentIdentifierLogFile config k
	, chunkLogFile config k
	, equivilantKeysLogFile config k
	] ++ oldurlLogs config k

{- All uuid-based logs stored in the top of the git-annex branch. -}
topLevelUUIDBasedLogs :: [RawFilePath]
topLevelUUIDBasedLogs = topLevelNewUUIDBasedLogs ++ topLevelOldUUIDBasedLogs

{- All the old-format uuid-based logs stored in the top of the git-annex branch. -}
topLevelOldUUIDBasedLogs :: [RawFilePath]
topLevelOldUUIDBasedLogs =
	[ uuidLog
	, remoteLog
	, trustLog
	, groupLog 
	, preferredContentLog
	, requiredContentLog
	, scheduleLog
	, activityLog
	, differenceLog
	, multicastLog
	]

{- All the new-format uuid-based logs stored in the top of the git-annex branch. -}
topLevelNewUUIDBasedLogs :: [RawFilePath]
topLevelNewUUIDBasedLogs =
	[ exportLog
	, proxyLog
	, clusterLog
	, maxSizeLog
	]

{- Other top-level logs. -}
otherTopLevelLogs :: [RawFilePath]
otherTopLevelLogs =
	[ numcopiesLog
	, mincopiesLog
	, configLog
	, groupPreferredContentLog
	]

uuidLog :: RawFilePath
uuidLog = "uuid.log"

numcopiesLog :: RawFilePath
numcopiesLog = "numcopies.log"

mincopiesLog :: RawFilePath
mincopiesLog = "mincopies.log"

configLog :: RawFilePath
configLog = "config.log"

remoteLog :: RawFilePath
remoteLog = "remote.log"

trustLog :: RawFilePath
trustLog = "trust.log"

groupLog :: RawFilePath
groupLog = "group.log"

preferredContentLog :: RawFilePath
preferredContentLog = "preferred-content.log"

requiredContentLog :: RawFilePath
requiredContentLog = "required-content.log"

groupPreferredContentLog :: RawFilePath
groupPreferredContentLog = "group-preferred-content.log"

scheduleLog :: RawFilePath
scheduleLog = "schedule.log"

activityLog :: RawFilePath
activityLog = "activity.log"

differenceLog :: RawFilePath
differenceLog = "difference.log"

multicastLog :: RawFilePath
multicastLog = "multicast.log"

exportLog :: RawFilePath
exportLog = "export.log"

proxyLog :: RawFilePath
proxyLog = "proxy.log"

clusterLog :: RawFilePath
clusterLog = "cluster.log"

maxSizeLog :: RawFilePath
maxSizeLog = "maxsize.log"

{- This is not a log file, it's where exported treeishes get grafted into
 - the git-annex branch. -}
exportTreeGraftPoint :: RawFilePath
exportTreeGraftPoint = "export.tree"

{- This is not a log file, it's where migration treeishes get grafted into
 - the git-annex branch. -}
migrationTreeGraftPoint :: RawFilePath
migrationTreeGraftPoint = "migrate.tree"

{- The pathname of the location log file for a given key. -}
locationLogFile :: GitConfig -> Key -> RawFilePath
locationLogFile config key =
	branchHashDir config key P.</> keyFile key <> locationLogExt

locationLogExt :: S.ByteString
locationLogExt = ".log"

{- The filename of the url log for a given key. -}
urlLogFile :: GitConfig -> Key -> RawFilePath
urlLogFile config key = 
	branchHashDir config key P.</> keyFile key <> urlLogExt

{- Old versions stored the urls elsewhere. -}
oldurlLogs :: GitConfig -> Key -> [RawFilePath]
oldurlLogs config key =
	[ "remote/web" P.</> hdir P.</> serializeKey' key <> ".log"
	, "remote/web" P.</> hdir P.</> keyFile key <> ".log"
	]
  where
	hdir = branchHashDir config key

urlLogExt :: S.ByteString
urlLogExt = ".log.web"

{- Does not work on oldurllogs. -}
isUrlLog :: RawFilePath -> Bool
isUrlLog file = urlLogExt `S.isSuffixOf` file

{- The filename of the remote state log for a given key. -}
remoteStateLogFile :: GitConfig -> Key -> RawFilePath
remoteStateLogFile config key = 
	(branchHashDir config key P.</> keyFile key)
		<> remoteStateLogExt

remoteStateLogExt :: S.ByteString
remoteStateLogExt = ".log.rmt"

isRemoteStateLog :: RawFilePath -> Bool
isRemoteStateLog path = remoteStateLogExt `S.isSuffixOf` path

{- The filename of the chunk log for a given key. -}
chunkLogFile :: GitConfig -> Key -> RawFilePath
chunkLogFile config key = 
	(branchHashDir config key P.</> keyFile key)
		<> chunkLogExt

chunkLogExt :: S.ByteString
chunkLogExt = ".log.cnk"

{- The filename of the equivalent keys log for a given key. -}
equivilantKeysLogFile :: GitConfig -> Key -> RawFilePath
equivilantKeysLogFile config key = 
	(branchHashDir config key P.</> keyFile key)
		<> equivilantKeyLogExt

equivilantKeyLogExt :: S.ByteString
equivilantKeyLogExt = ".log.ek"

isEquivilantKeyLog :: RawFilePath -> Bool
isEquivilantKeyLog path = equivilantKeyLogExt `S.isSuffixOf` path

{- The filename of the metadata log for a given key. -}
metaDataLogFile :: GitConfig -> Key -> RawFilePath
metaDataLogFile config key =
	(branchHashDir config key P.</> keyFile key)
		<> metaDataLogExt

metaDataLogExt :: S.ByteString
metaDataLogExt = ".log.met"

isMetaDataLog :: RawFilePath -> Bool
isMetaDataLog path = metaDataLogExt `S.isSuffixOf` path

{- The filename of the remote metadata log for a given key. -}
remoteMetaDataLogFile :: GitConfig -> Key -> RawFilePath
remoteMetaDataLogFile config key = 
	(branchHashDir config key P.</> keyFile key)
		<> remoteMetaDataLogExt

remoteMetaDataLogExt :: S.ByteString
remoteMetaDataLogExt = ".log.rmet"

isRemoteMetaDataLog :: RawFilePath -> Bool
isRemoteMetaDataLog path = remoteMetaDataLogExt `S.isSuffixOf` path

{- The filename of the remote content identifier log for a given key. -}
remoteContentIdentifierLogFile :: GitConfig -> Key -> RawFilePath
remoteContentIdentifierLogFile config key =
	(branchHashDir config key P.</> keyFile key)
		<> remoteContentIdentifierExt

remoteContentIdentifierExt :: S.ByteString
remoteContentIdentifierExt = ".log.cid"

isRemoteContentIdentifierLog :: RawFilePath -> Bool
isRemoteContentIdentifierLog path = remoteContentIdentifierExt `S.isSuffixOf` path

{- From an extension and a log filename, get the key that it's a log for. -}
extLogFileKey :: S.ByteString -> RawFilePath -> Maybe Key
extLogFileKey expectedext path
	| ext == expectedext = fileKey base
	| otherwise = Nothing
  where
	file = P.takeFileName path
	(base, ext) = S.splitAt (S.length file - extlen) file
	extlen = S.length expectedext

{- Converts a url log file into a key.
 - (Does not work on oldurlLogs.) -}
urlLogFileKey :: RawFilePath -> Maybe Key
urlLogFileKey = extLogFileKey urlLogExt

{- Converts a pathname into a key if it's a location log. -}
locationLogFileKey :: GitConfig -> RawFilePath -> Maybe Key
locationLogFileKey config path
	| length (splitDirectories (fromRawFilePath path)) /= locationLogFileDepth config = Nothing
	| otherwise = extLogFileKey ".log" path

{- Depth of location log files within the git-annex branch.
 -
 - Normally they are xx/yy/key.log so depth 3. 
 - The same extension is also used for other logs that
 - are not location logs. -}
locationLogFileDepth :: GitConfig -> Int
locationLogFileDepth config = hashlevels + 1
  where
        HashLevels hashlevels = branchHashLevels config
