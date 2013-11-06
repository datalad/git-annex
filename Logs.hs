{- git-annex log file names
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs where

import Common.Annex
import Types.Key

data LogVariety = UUIDBasedLog | PresenceLog Key
	deriving (Show)

{- Converts a path from the git-annex branch into one of the varieties
 - of logs used by git-annex, if it's a known path. -}
getLogVariety :: FilePath -> Maybe LogVariety
getLogVariety f
	| f `elem` uuidBasedLogs = Just UUIDBasedLog
	| otherwise = PresenceLog <$> firstJust (presenceLogs f)

{- All the uuid-based logs stored in the git-annex branch. -}
uuidBasedLogs :: [FilePath]
uuidBasedLogs =
	[ uuidLog
	, remoteLog
	, trustLog
	, groupLog 
	, preferredContentLog
	, scheduleLog
	]

{- All the ways to get a key from a presence log file -}
presenceLogs :: FilePath -> [Maybe Key]
presenceLogs f =
	[ urlLogFileKey f
	, locationLogFileKey f
	]

uuidLog :: FilePath
uuidLog = "uuid.log"

remoteLog :: FilePath
remoteLog = "remote.log"

trustLog :: FilePath
trustLog = "trust.log"

groupLog :: FilePath
groupLog = "group.log"

preferredContentLog :: FilePath
preferredContentLog = "preferred-content.log"

scheduleLog :: FilePath
scheduleLog = "schedule.log"

{- The pathname of the location log file for a given key. -}
locationLogFile :: Key -> String
locationLogFile key = hashDirLower key ++ keyFile key ++ ".log"

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
urlLogFile key = hashDirLower key </> keyFile key ++ urlLogExt

{- Old versions stored the urls elsewhere. -}
oldurlLogs :: Key -> [FilePath]
oldurlLogs key =
	[ "remote/web" </> hashDirLower key </> key2file key ++ ".log"
	, "remote/web" </> hashDirLower key </> keyFile key ++ ".log"
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

prop_logs_sane :: Key -> Bool
prop_logs_sane dummykey = all id
	[ isNothing (getLogVariety "unknown")
	, expect isUUIDBasedLog (getLogVariety uuidLog)
	, expect isPresenceLog (getLogVariety $ locationLogFile dummykey)
	, expect isPresenceLog (getLogVariety $ urlLogFile dummykey)
	]
  where
  	expect = maybe False
  	isUUIDBasedLog UUIDBasedLog = True
	isUUIDBasedLog _ = False
	isPresenceLog (PresenceLog k) = k == dummykey
	isPresenceLog _ = False
