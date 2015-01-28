{- git-annex log file names
 -
 - Copyright 2013-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs where

import Common.Annex
import Types.Key

{- There are several varieties of log file formats. -}
data LogVariety
	= UUIDBasedLog
	| NewUUIDBasedLog
	| ChunkLog Key
	| PresenceLog Key
	| OtherLog
	deriving (Show)

{- Converts a path from the git-annex branch into one of the varieties
 - of logs used by git-annex, if it's a known path. -}
getLogVariety :: FilePath -> Maybe LogVariety
getLogVariety f
	| f `elem` topLevelUUIDBasedLogs = Just UUIDBasedLog
	| isRemoteStateLog f = Just NewUUIDBasedLog
	| isChunkLog f = ChunkLog <$> chunkLogFileKey f
	| isMetaDataLog f || f `elem` otherLogs = Just OtherLog
	| otherwise = PresenceLog <$> firstJust (presenceLogs f)

{- All the uuid-based logs stored in the top of the git-annex branch. -}
topLevelUUIDBasedLogs :: [FilePath]
topLevelUUIDBasedLogs =
	[ uuidLog
	, remoteLog
	, trustLog
	, groupLog 
	, preferredContentLog
	, requiredContentLog
	, scheduleLog
	, differenceLog
	]

{- All the ways to get a key from a presence log file -}
presenceLogs :: FilePath -> [Maybe Key]
presenceLogs f =
	[ urlLogFileKey f
	, locationLogFileKey f
	]

{- Logs that are neither UUID based nor presence logs. -}
otherLogs :: [FilePath]
otherLogs =
	[ numcopiesLog
	, groupPreferredContentLog
	]

uuidLog :: FilePath
uuidLog = "uuid.log"

numcopiesLog :: FilePath
numcopiesLog = "numcopies.log"

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

differenceLog :: FilePath
differenceLog = "difference.log"

{- The pathname of the location log file for a given key. -}
locationLogFile :: Key -> String
locationLogFile key = hashDirLower def key ++ keyFile key ++ ".log"

{- Converts a pathname into a key if it's a location log. -}
locationLogFileKey :: FilePath -> Maybe Key
locationLogFileKey path
	| ["remote", "web"] `isPrefixOf` splitDirectories dir = Nothing
	| ext == ".log" = fileKey base
	| otherwise = Nothing
  where
	(dir, file) = splitFileName path
	(base, ext) = splitAt (length file - 4) file

{- The filename of the url log for a given key. -}
urlLogFile :: Key -> FilePath
urlLogFile key = hashDirLower def key </> keyFile key ++ urlLogExt

{- Old versions stored the urls elsewhere. -}
oldurlLogs :: Key -> [FilePath]
oldurlLogs key =
	[ "remote/web" </> hashDirLower def key </> key2file key ++ ".log"
	, "remote/web" </> hashDirLower def key </> keyFile key ++ ".log"
	]

urlLogExt :: String
urlLogExt = ".log.web"

{- Converts a url log file into a key.
 - (Does not work on oldurlLogs.) -}
urlLogFileKey :: FilePath -> Maybe Key
urlLogFileKey path
	| ext == urlLogExt = fileKey base
	| otherwise = Nothing
  where
	file = takeFileName path
	(base, ext) = splitAt (length file - extlen) file
	extlen = length urlLogExt

{- Does not work on oldurllogs. -}
isUrlLog :: FilePath -> Bool
isUrlLog file = urlLogExt `isSuffixOf` file

{- The filename of the remote state log for a given key. -}
remoteStateLogFile :: Key -> FilePath
remoteStateLogFile key = hashDirLower def key </> keyFile key ++ remoteStateLogExt

remoteStateLogExt :: String
remoteStateLogExt = ".log.rmt"

isRemoteStateLog :: FilePath -> Bool
isRemoteStateLog path = remoteStateLogExt `isSuffixOf` path

{- The filename of the chunk log for a given key. -}
chunkLogFile :: Key -> FilePath
chunkLogFile key = hashDirLower def key </> keyFile key ++ chunkLogExt

chunkLogFileKey :: FilePath -> Maybe Key
chunkLogFileKey path
	| ext == chunkLogExt = fileKey base
	| otherwise = Nothing
  where
	file = takeFileName path
	(base, ext) = splitAt (length file - extlen) file
	extlen = length chunkLogExt

chunkLogExt :: String
chunkLogExt = ".log.cnk"

isChunkLog :: FilePath -> Bool
isChunkLog path = chunkLogExt `isSuffixOf` path

{- The filename of the metadata log for a given key. -}
metaDataLogFile :: Key -> FilePath
metaDataLogFile key = hashDirLower def key </> keyFile key ++ metaDataLogExt

metaDataLogExt :: String
metaDataLogExt = ".log.met"

isMetaDataLog :: FilePath -> Bool
isMetaDataLog path = metaDataLogExt `isSuffixOf` path

prop_logs_sane :: Key -> Bool
prop_logs_sane dummykey = and
	[ isNothing (getLogVariety "unknown")
	, expect gotUUIDBasedLog (getLogVariety uuidLog)
	, expect gotPresenceLog (getLogVariety $ locationLogFile dummykey)
	, expect gotPresenceLog (getLogVariety $ urlLogFile dummykey)
	, expect gotNewUUIDBasedLog (getLogVariety $ remoteStateLogFile dummykey)
	, expect gotChunkLog (getLogVariety $ chunkLogFile dummykey)
	, expect gotOtherLog (getLogVariety $ metaDataLogFile dummykey)
	, expect gotOtherLog (getLogVariety numcopiesLog)
	]
  where
	expect = maybe False
	gotUUIDBasedLog UUIDBasedLog = True
	gotUUIDBasedLog _ = False
	gotNewUUIDBasedLog NewUUIDBasedLog = True
	gotNewUUIDBasedLog _ = False
	gotChunkLog (ChunkLog k) = k == dummykey
	gotChunkLog _ = False
	gotPresenceLog (PresenceLog k) = k == dummykey
	gotPresenceLog _ = False
	gotOtherLog OtherLog = True
	gotOtherLog _ = False
