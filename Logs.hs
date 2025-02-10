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
import qualified Utility.OsString as OS

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
getLogVariety :: GitConfig -> OsPath -> Maybe LogVariety
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
keyLogFiles :: GitConfig -> Key -> [OsPath]
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
topLevelUUIDBasedLogs :: [OsPath]
topLevelUUIDBasedLogs = topLevelNewUUIDBasedLogs ++ topLevelOldUUIDBasedLogs

{- All the old-format uuid-based logs stored in the top of the git-annex branch. -}
topLevelOldUUIDBasedLogs :: [OsPath]
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
topLevelNewUUIDBasedLogs :: [OsPath]
topLevelNewUUIDBasedLogs =
	[ exportLog
	, proxyLog
	, clusterLog
	, maxSizeLog
	]

{- Other top-level logs. -}
otherTopLevelLogs :: [OsPath]
otherTopLevelLogs =
	[ numcopiesLog
	, mincopiesLog
	, configLog
	, groupPreferredContentLog
	]

uuidLog :: OsPath
uuidLog = literalOsPath "uuid.log"

numcopiesLog :: OsPath
numcopiesLog = literalOsPath "numcopies.log"

mincopiesLog :: OsPath
mincopiesLog = literalOsPath "mincopies.log"

configLog :: OsPath
configLog = literalOsPath "config.log"

remoteLog :: OsPath
remoteLog = literalOsPath "remote.log"

trustLog :: OsPath
trustLog = literalOsPath "trust.log"

groupLog :: OsPath
groupLog = literalOsPath "group.log"

preferredContentLog :: OsPath
preferredContentLog = literalOsPath "preferred-content.log"

requiredContentLog :: OsPath
requiredContentLog = literalOsPath "required-content.log"

groupPreferredContentLog :: OsPath
groupPreferredContentLog = literalOsPath "group-preferred-content.log"

scheduleLog :: OsPath
scheduleLog = literalOsPath "schedule.log"

activityLog :: OsPath
activityLog = literalOsPath "activity.log"

differenceLog :: OsPath
differenceLog = literalOsPath "difference.log"

multicastLog :: OsPath
multicastLog = literalOsPath "multicast.log"

exportLog :: OsPath
exportLog = literalOsPath "export.log"

proxyLog :: OsPath
proxyLog = literalOsPath "proxy.log"

clusterLog :: OsPath
clusterLog = literalOsPath "cluster.log"

maxSizeLog :: OsPath
maxSizeLog = literalOsPath "maxsize.log"

{- This is not a log file, it's where exported treeishes get grafted into
 - the git-annex branch. -}
exportTreeGraftPoint :: OsPath
exportTreeGraftPoint = literalOsPath "export.tree"

{- This is not a log file, it's where migration treeishes get grafted into
 - the git-annex branch. -}
migrationTreeGraftPoint :: OsPath
migrationTreeGraftPoint = literalOsPath "migrate.tree"

{- The pathname of the location log file for a given key. -}
locationLogFile :: GitConfig -> Key -> OsPath
locationLogFile config key =
	branchHashDir config key </> keyFile key <> locationLogExt

locationLogExt :: OsPath
locationLogExt = literalOsPath ".log"

{- The filename of the url log for a given key. -}
urlLogFile :: GitConfig -> Key -> OsPath
urlLogFile config key = 
	branchHashDir config key </> keyFile key <> urlLogExt

{- Old versions stored the urls elsewhere. -}
oldurlLogs :: GitConfig -> Key -> [OsPath]
oldurlLogs config key =
	[ literalOsPath "remote/web" </> hdir </> toOsPath (serializeKey'' key) <> literalOsPath ".log"
	, literalOsPath "remote/web" </> hdir </> keyFile key <> literalOsPath ".log"
	]
  where
	hdir = branchHashDir config key

urlLogExt :: OsPath
urlLogExt = literalOsPath ".log.web"

{- Does not work on oldurllogs. -}
isUrlLog :: OsPath -> Bool
isUrlLog file = urlLogExt `OS.isSuffixOf` file

{- The filename of the remote state log for a given key. -}
remoteStateLogFile :: GitConfig -> Key -> OsPath
remoteStateLogFile config key = 
	(branchHashDir config key </> keyFile key)
		<> remoteStateLogExt

remoteStateLogExt :: OsPath
remoteStateLogExt = literalOsPath ".log.rmt"

isRemoteStateLog :: OsPath -> Bool
isRemoteStateLog path = remoteStateLogExt `OS.isSuffixOf` path

{- The filename of the chunk log for a given key. -}
chunkLogFile :: GitConfig -> Key -> OsPath
chunkLogFile config key = 
	(branchHashDir config key </> keyFile key)
		<> chunkLogExt

chunkLogExt :: OsPath
chunkLogExt = literalOsPath ".log.cnk"

{- The filename of the equivalent keys log for a given key. -}
equivilantKeysLogFile :: GitConfig -> Key -> OsPath
equivilantKeysLogFile config key = 
	(branchHashDir config key </> keyFile key)
		<> equivilantKeyLogExt

equivilantKeyLogExt :: OsPath
equivilantKeyLogExt = literalOsPath ".log.ek"

isEquivilantKeyLog :: OsPath -> Bool
isEquivilantKeyLog path = equivilantKeyLogExt `OS.isSuffixOf` path

{- The filename of the metadata log for a given key. -}
metaDataLogFile :: GitConfig -> Key -> OsPath
metaDataLogFile config key =
	(branchHashDir config key </> keyFile key)
		<> metaDataLogExt

metaDataLogExt :: OsPath
metaDataLogExt = literalOsPath ".log.met"

isMetaDataLog :: OsPath -> Bool
isMetaDataLog path = metaDataLogExt `OS.isSuffixOf` path

{- The filename of the remote metadata log for a given key. -}
remoteMetaDataLogFile :: GitConfig -> Key -> OsPath
remoteMetaDataLogFile config key = 
	(branchHashDir config key </> keyFile key)
		<> remoteMetaDataLogExt

remoteMetaDataLogExt :: OsPath
remoteMetaDataLogExt = literalOsPath ".log.rmet"

isRemoteMetaDataLog :: OsPath -> Bool
isRemoteMetaDataLog path = remoteMetaDataLogExt `OS.isSuffixOf` path

{- The filename of the remote content identifier log for a given key. -}
remoteContentIdentifierLogFile :: GitConfig -> Key -> OsPath
remoteContentIdentifierLogFile config key =
	(branchHashDir config key </> keyFile key)
		<> remoteContentIdentifierExt

remoteContentIdentifierExt :: OsPath
remoteContentIdentifierExt = literalOsPath ".log.cid"

isRemoteContentIdentifierLog :: OsPath -> Bool
isRemoteContentIdentifierLog path = remoteContentIdentifierExt `OS.isSuffixOf` path

{- From an extension and a log filename, get the key that it's a log for. -}
extLogFileKey :: OsPath -> OsPath -> Maybe Key
extLogFileKey expectedext path
	| ext == expectedext = fileKey base
	| otherwise = Nothing
  where
	file = takeFileName path
	(base, ext) = OS.splitAt (OS.length file - extlen) file
	extlen = OS.length expectedext

{- Converts a url log file into a key.
 - (Does not work on oldurlLogs.) -}
urlLogFileKey :: OsPath -> Maybe Key
urlLogFileKey = extLogFileKey urlLogExt

{- Converts a pathname into a key if it's a location log. -}
locationLogFileKey :: GitConfig -> OsPath -> Maybe Key
locationLogFileKey config path
	| length (splitDirectories path) /= locationLogFileDepth config = Nothing
	| otherwise = extLogFileKey (literalOsPath ".log") path

{- Depth of location log files within the git-annex branch.
 -
 - Normally they are xx/yy/key.log so depth 3. 
 - The same extension is also used for other logs that
 - are not location logs. -}
locationLogFileDepth :: GitConfig -> Int
locationLogFileDepth config = hashlevels + 1
  where
        HashLevels hashlevels = branchHashLevels config
