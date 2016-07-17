{-# LANGUAGE BangPatterns #-}

{- git-annex location log
 -
 - git-annex keeps track of which repositories have the contents of annexed
 - files.
 -
 - Repositories record their UUID and the date when they --get or --drop
 - a value.
 - 
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Location (
	LogStatus(..),
	logStatus,
	logChange,
	loggedLocations,
	loggedLocationsHistorical,
	loggedLocationsRef,
	isKnownKey,
	checkDead,
	setDead,
	loggedKeys,
	loggedKeysFor,
) where

import Annex.Common
import qualified Annex.Branch
import Logs
import Logs.Presence
import Annex.UUID
import Annex.CatFile
import Git.Types (RefDate, Ref)
import qualified Annex

import Data.Time.Clock
import qualified Data.ByteString.Lazy.Char8 as L

{- Log a change in the presence of a key's value in current repository. -}
logStatus :: Key -> LogStatus -> Annex ()
logStatus key s = do
	u <- getUUID
	logChange key u s

{- Log a change in the presence of a key's value in a repository. -}
logChange :: Key -> UUID -> LogStatus -> Annex ()
logChange = logChange' logNow

logChange' :: (LogStatus -> String -> Annex LogLine) -> Key -> UUID -> LogStatus -> Annex ()
logChange' mklog key (UUID u) s = do
	config <- Annex.getGitConfig
	maybeAddLog (locationLogFile config key) =<< mklog s u
logChange' _ _ NoUUID _ = noop

{- Returns a list of repository UUIDs that, according to the log, have
 - the value of a key. -}
loggedLocations :: Key -> Annex [UUID]
loggedLocations = getLoggedLocations currentLogInfo

{- Gets the location log on a particular date. -}
loggedLocationsHistorical :: RefDate -> Key -> Annex [UUID]
loggedLocationsHistorical = getLoggedLocations . historicalLogInfo

{- Gets the locations contained in a git ref. -}
loggedLocationsRef :: Ref -> Annex [UUID]
loggedLocationsRef ref = map toUUID . getLog . L.unpack <$> catObject ref

getLoggedLocations :: (FilePath -> Annex [String]) -> Key -> Annex [UUID]
getLoggedLocations getter key = do
	config <- Annex.getGitConfig
	map toUUID <$> getter (locationLogFile config key)

{- Is there a location log for the key? True even for keys with no
 - remaining locations. -}
isKnownKey :: Key -> Annex Bool
isKnownKey key = do
	config <- Annex.getGitConfig
	not . null <$> readLog (locationLogFile config key)

{- For a key to be dead, all locations that have location status for the key
 - must have InfoDead set. -}
checkDead :: Key -> Annex Bool
checkDead key = do
	config <- Annex.getGitConfig
	ls <- compactLog <$> readLog (locationLogFile config key)
	return $ all (\l -> status l == InfoDead) ls

{- Updates the log to say that a key is dead. 
 - 
 - Changes all logged lines for the key, in any location, that are
 - currently InfoMissing, to be InfoDead.
 -}
setDead :: Key -> Annex ()
setDead key = do
	config <- Annex.getGitConfig
	let logfile = locationLogFile config key
	ls <- compactLog <$> readLog logfile
	mapM_ (go logfile) (filter (\l -> status l == InfoMissing) ls)
  where
	go logfile l = addLog logfile $ setDead' l

{- Note that the timestamp in the log is updated minimally, so that this
 - can be overruled by other location log changes. -}
setDead' :: LogLine -> LogLine
setDead' l = l
	{ status = InfoDead
	, date = date l + realToFrac (picosecondsToDiffTime 1)
	}

{- Finds all keys that have location log information.
 - (There may be duplicate keys in the list.) -}
loggedKeys :: Annex [Key]
loggedKeys = mapMaybe locationLogFileKey <$> Annex.Branch.files

{- Finds all keys that have location log information indicating
 - they are present for the specified repository. -}
loggedKeysFor :: UUID -> Annex [Key]
loggedKeysFor u = filterM isthere =<< loggedKeys
  where
	{- This should run strictly to avoid the filterM
	 - building many thunks containing keyLocations data. -}
	isthere k = do
		us <- loggedLocations k
		let !there = u `elem` us
		return there
