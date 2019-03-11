{- git-annex log file names
 -
 - Copyright 2013-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs where

import Annex.Common
import Annex.DirHashes

{- There are several varieties of log file formats. -}
data LogVariety
	= OldUUIDBasedLog
	| NewUUIDBasedLog
	| ChunkLog Key
	| PresenceLog Key
	| RemoteMetaDataLog
	| OtherLog
	deriving (Show)

{- Converts a path from the git-annex branch into one of the varieties
 - of logs used by git-annex, if it's a known path. -}
getLogVariety :: FilePath -> Maybe LogVariety
getLogVariety f
	| f `elem` topLevelOldUUIDBasedLogs = Just OldUUIDBasedLog
	| f `elem` topLevelNewUUIDBasedLogs = Just NewUUIDBasedLog
	| isRemoteStateLog f = Just NewUUIDBasedLog
	| isRemoteContentIdentifierLog f = Just NewUUIDBasedLog
	| isChunkLog f = ChunkLog <$> extLogFileKey chunkLogExt f
	| isRemoteMetaDataLog f = Just RemoteMetaDataLog
	| isMetaDataLog f || f `elem` otherLogs = Just OtherLog
	| otherwise = PresenceLog <$> firstJust (presenceLogs f)

{- All the old-format uuid-based logs stored in the top of the git-annex branch. -}
topLevelOldUUIDBasedLogs :: [FilePath]
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
topLevelNewUUIDBasedLogs :: [FilePath]
topLevelNewUUIDBasedLogs =
	[ exportLog
	]


{- All the ways to get a key from a presence log file -}
presenceLogs :: FilePath -> [Maybe Key]
presenceLogs f =
	[ urlLogFileKey f
	, locationLogFileKey f
	]

{- Top-level logs that are neither UUID based nor presence logs. -}
otherLogs :: [FilePath]
otherLogs =
	[ numcopiesLog
	, groupPreferredContentLog
	]

uuidLog :: FilePath
uuidLog = "uuid.log"

numcopiesLog :: FilePath
numcopiesLog = "numcopies.log"

configLog :: FilePath
configLog = "config.log"

remoteLog :: FilePath
remoteLog = "remote.log"

trustLog :: FilePath
trustLog = "trust.log"

groupLog :: FilePath
groupLog = "group.log"

preferredContentLog :: FilePath
preferredContentLog = "preferred-content.log"

requiredContentLog :: FilePath
requiredContentLog = "required-content.log"

groupPreferredContentLog :: FilePath
groupPreferredContentLog = "group-preferred-content.log"

scheduleLog :: FilePath
scheduleLog = "schedule.log"

activityLog :: FilePath
activityLog = "activity.log"

differenceLog :: FilePath
differenceLog = "difference.log"

multicastLog :: FilePath
multicastLog = "multicast.log"

exportLog :: FilePath
exportLog = "export.log"

{- The pathname of the location log file for a given key. -}
locationLogFile :: GitConfig -> Key -> String
locationLogFile config key = branchHashDir config key </> keyFile key ++ ".log"

{- The filename of the url log for a given key. -}
urlLogFile :: GitConfig -> Key -> FilePath
urlLogFile config key = branchHashDir config key </> keyFile key ++ urlLogExt

{- Old versions stored the urls elsewhere. -}
oldurlLogs :: GitConfig -> Key -> [FilePath]
oldurlLogs config key =
	[ "remote/web" </> hdir </> serializeKey key ++ ".log"
	, "remote/web" </> hdir </> keyFile key ++ ".log"
	]
  where
	hdir = branchHashDir config key

urlLogExt :: String
urlLogExt = ".log.web"

{- Does not work on oldurllogs. -}
isUrlLog :: FilePath -> Bool
isUrlLog file = urlLogExt `isSuffixOf` file

{- The filename of the remote state log for a given key. -}
remoteStateLogFile :: GitConfig -> Key -> FilePath
remoteStateLogFile config key = branchHashDir config key 
	</> keyFile key ++ remoteStateLogExt

remoteStateLogExt :: String
remoteStateLogExt = ".log.rmt"

isRemoteStateLog :: FilePath -> Bool
isRemoteStateLog path = remoteStateLogExt `isSuffixOf` path

{- The filename of the chunk log for a given key. -}
chunkLogFile :: GitConfig -> Key -> FilePath
chunkLogFile config key = branchHashDir config key </> keyFile key ++ chunkLogExt

chunkLogExt :: String
chunkLogExt = ".log.cnk"

isChunkLog :: FilePath -> Bool
isChunkLog path = chunkLogExt `isSuffixOf` path

{- The filename of the metadata log for a given key. -}
metaDataLogFile :: GitConfig -> Key -> FilePath
metaDataLogFile config key = branchHashDir config key </> keyFile key ++ metaDataLogExt

metaDataLogExt :: String
metaDataLogExt = ".log.met"

isMetaDataLog :: FilePath -> Bool
isMetaDataLog path = metaDataLogExt `isSuffixOf` path

{- The filename of the remote metadata log for a given key. -}
remoteMetaDataLogFile :: GitConfig -> Key -> FilePath
remoteMetaDataLogFile config key = branchHashDir config key </> keyFile key ++ remoteMetaDataLogExt

remoteMetaDataLogExt :: String
remoteMetaDataLogExt = ".log.rmet"

isRemoteMetaDataLog :: FilePath -> Bool
isRemoteMetaDataLog path = remoteMetaDataLogExt `isSuffixOf` path

{- The filename of the remote content identifier log for a given key. -}
remoteContentIdentifierLogFile :: GitConfig -> Key -> FilePath
remoteContentIdentifierLogFile config key = branchHashDir config key </> keyFile key ++ remoteContentIdentifierExt

remoteContentIdentifierExt :: String
remoteContentIdentifierExt = ".log.cid"

isRemoteContentIdentifierLog :: FilePath -> Bool
isRemoteContentIdentifierLog path = remoteContentIdentifierExt `isSuffixOf` path

{- From an extension and a log filename, get the key that it's a log for. -}
extLogFileKey :: String -> FilePath -> Maybe Key
extLogFileKey expectedext path
	| ext == expectedext = fileKey base
	| otherwise = Nothing
  where
	file = takeFileName path
	(base, ext) = splitAt (length file - extlen) file
	extlen = length expectedext

{- Converts a url log file into a key.
 - (Does not work on oldurlLogs.) -}
urlLogFileKey :: FilePath -> Maybe Key
urlLogFileKey = extLogFileKey urlLogExt

{- Converts a pathname into a key if it's a location log. -}
locationLogFileKey :: FilePath -> Maybe Key
locationLogFileKey path
	-- Want only xx/yy/foo.log, not .log files in other places.
	| length (splitDirectories path) /= 3 = Nothing
	| otherwise = extLogFileKey ".log" path
