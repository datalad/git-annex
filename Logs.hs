{- git-annex log file names
 -
 - Copyright 2013-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Logs where

import Annex.Common
import Annex.DirHashes

import qualified Data.ByteString as S

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
getLogVariety :: RawFilePath -> Maybe LogVariety
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
	]


{- All the ways to get a key from a presence log file -}
presenceLogs :: RawFilePath -> [Maybe Key]
presenceLogs f =
	[ urlLogFileKey f
	, locationLogFileKey f
	]

{- Top-level logs that are neither UUID based nor presence logs. -}
otherLogs :: [RawFilePath]
otherLogs =
	[ numcopiesLog
	, groupPreferredContentLog
	]

uuidLog :: RawFilePath
uuidLog = "uuid.log"

numcopiesLog :: RawFilePath
numcopiesLog = "numcopies.log"

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

{- The pathname of the location log file for a given key. -}
locationLogFile :: GitConfig -> Key -> RawFilePath
locationLogFile config key = toRawFilePath $
	branchHashDir config key </> keyFile key ++ ".log"

{- The filename of the url log for a given key. -}
urlLogFile :: GitConfig -> Key -> RawFilePath
urlLogFile config key = toRawFilePath $
	branchHashDir config key </> keyFile key ++ decodeBS' urlLogExt

{- Old versions stored the urls elsewhere. -}
oldurlLogs :: GitConfig -> Key -> [RawFilePath]
oldurlLogs config key = map toRawFilePath
	[ "remote/web" </> hdir </> serializeKey key ++ ".log"
	, "remote/web" </> hdir </> keyFile key ++ ".log"
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
	toRawFilePath (branchHashDir config key </> keyFile key)
		<> remoteStateLogExt

remoteStateLogExt :: S.ByteString
remoteStateLogExt = ".log.rmt"

isRemoteStateLog :: RawFilePath -> Bool
isRemoteStateLog path = remoteStateLogExt `S.isSuffixOf` path

{- The filename of the chunk log for a given key. -}
chunkLogFile :: GitConfig -> Key -> RawFilePath
chunkLogFile config key = 
	toRawFilePath (branchHashDir config key </> keyFile key)
		<> chunkLogExt

chunkLogExt :: S.ByteString
chunkLogExt = ".log.cnk"

isChunkLog :: RawFilePath -> Bool
isChunkLog path = chunkLogExt `S.isSuffixOf` path

{- The filename of the metadata log for a given key. -}
metaDataLogFile :: GitConfig -> Key -> RawFilePath
metaDataLogFile config key =
	toRawFilePath (branchHashDir config key </> keyFile key)
		<> metaDataLogExt

metaDataLogExt :: S.ByteString
metaDataLogExt = ".log.met"

isMetaDataLog :: RawFilePath -> Bool
isMetaDataLog path = metaDataLogExt `S.isSuffixOf` path

{- The filename of the remote metadata log for a given key. -}
remoteMetaDataLogFile :: GitConfig -> Key -> RawFilePath
remoteMetaDataLogFile config key = 
	toRawFilePath (branchHashDir config key </> keyFile key)
		<> remoteMetaDataLogExt

remoteMetaDataLogExt :: S.ByteString
remoteMetaDataLogExt = ".log.rmet"

isRemoteMetaDataLog :: RawFilePath -> Bool
isRemoteMetaDataLog path = remoteMetaDataLogExt `S.isSuffixOf` path

{- The filename of the remote content identifier log for a given key. -}
remoteContentIdentifierLogFile :: GitConfig -> Key -> RawFilePath
remoteContentIdentifierLogFile config key =
	toRawFilePath (branchHashDir config key </> keyFile key)
		<> remoteContentIdentifierExt

remoteContentIdentifierExt :: S.ByteString
remoteContentIdentifierExt = ".log.cid"

isRemoteContentIdentifierLog :: RawFilePath -> Bool
isRemoteContentIdentifierLog path = remoteContentIdentifierExt `S.isSuffixOf` path

{- From an extension and a log filename, get the key that it's a log for. -}
extLogFileKey :: S.ByteString -> RawFilePath -> Maybe Key
extLogFileKey expectedext path
	| encodeBS' ext == expectedext = fileKey base
	| otherwise = Nothing
  where
	file = takeFileName (fromRawFilePath path)
	(base, ext) = splitAt (length file - extlen) file
	extlen = S.length expectedext

{- Converts a url log file into a key.
 - (Does not work on oldurlLogs.) -}
urlLogFileKey :: RawFilePath -> Maybe Key
urlLogFileKey = extLogFileKey urlLogExt

{- Converts a pathname into a key if it's a location log. -}
locationLogFileKey :: RawFilePath -> Maybe Key
locationLogFileKey path
	-- Want only xx/yy/foo.log, not .log files in other places.
	| length (splitDirectories (fromRawFilePath path)) /= 3 = Nothing
	| otherwise = extLogFileKey ".log" path
