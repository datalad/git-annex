{-# LANGUAGE BangPatterns #-}

{- git-annex location log
 -
 - git-annex keeps track of which repositories have the contents of annexed
 - files.
 -
 - Repositories record their UUID and the date when they --get or --drop
 - a value.
 - 
 - Copyright 2010-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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
	Unchecked,
	finishCheck,
	loggedKeys,
	loggedKeysFor,
	loggedKeysFor',
) where

import Annex.Common
import qualified Annex.Branch
import Logs
import Logs.Presence
import Annex.UUID
import Annex.CatFile
import Annex.VectorClock
import Git.Types (RefDate, Ref)
import qualified Annex

import Data.Time.Clock

{- Log a change in the presence of a key's value in current repository. -}
logStatus :: Key -> LogStatus -> Annex ()
logStatus key s = do
	u <- getUUID
	logChange key u s

{- Log a change in the presence of a key's value in a repository. -}
logChange :: Key -> UUID -> LogStatus -> Annex ()
logChange = logChange' logNow

logChange' :: (LogStatus -> LogInfo -> Annex LogLine) -> Key -> UUID -> LogStatus -> Annex ()
logChange' mklog key u@(UUID _) s = do
	config <- Annex.getGitConfig
	maybeAddLog (locationLogFile config key) =<< mklog s (LogInfo (fromUUID u))
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
loggedLocationsRef ref = map (toUUID . fromLogInfo) . getLog <$> catObject ref

getLoggedLocations :: (RawFilePath -> Annex [LogInfo]) -> Key -> Annex [UUID]
getLoggedLocations getter key = do
	config <- Annex.getGitConfig
	map (toUUID . fromLogInfo) <$> getter (locationLogFile config key)

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
	return $! all (\l -> status l == InfoDead) ls

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
	, date = case date l of
		VectorClock c -> VectorClock $
			c + realToFrac (picosecondsToDiffTime 1)
		Unknown -> Unknown
	}

data Unchecked a = Unchecked (Annex (Maybe a))

finishCheck :: Unchecked a -> Annex (Maybe a)
finishCheck (Unchecked a) = a

{- Finds all keys that have location log information.
 - (There may be duplicate keys in the list.)
 -
 - Keys that have been marked as dead are not included.
 -}
loggedKeys :: Annex ([Unchecked Key], IO Bool)
loggedKeys = loggedKeys' (not <$$> checkDead)

loggedKeys' :: (Key -> Annex Bool) -> Annex ([Unchecked Key], IO Bool)
loggedKeys' check = do
	config <- Annex.getGitConfig
	(bfs, cleanup) <- Annex.Branch.files
	let l = mapMaybe (defercheck <$$> locationLogFileKey config) bfs
	return (l, cleanup)
  where
	defercheck k = Unchecked $ ifM (check k)
		( return (Just k)
		, return Nothing
		)

{- Finds all keys that have location log information indicating
 - they are present in the specified repository.
 -
 - This does not stream well; use loggedKeysFor' for lazy streaming.
 -}
loggedKeysFor :: UUID -> Annex [Key]
loggedKeysFor u = do
	(l, cleanup) <- loggedKeysFor' u
	l' <- catMaybes <$> mapM finishCheck l
	liftIO $ void cleanup
	return l'

loggedKeysFor' :: UUID -> Annex ([Unchecked Key], IO Bool)
loggedKeysFor' u = loggedKeys' isthere
  where
	isthere k = do
		us <- loggedLocations k
		let !there = u `elem` us
		return there
