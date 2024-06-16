{-# LANGUAGE BangPatterns #-}

{- git-annex location log
 -
 - git-annex keeps track of which repositories have the contents of annexed
 - files.
 -
 - Repositories record their UUID and the date when they --get or --drop
 - a value.
 - 
 - Copyright 2010-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Logs.Location (
	LogStatus(..),
	logStatus,
	logStatusAfter,
	logChange,
	loggedLocations,
	loggedPreviousLocations,
	loggedLocationsHistorical,
	loggedLocationsRef,
	parseLoggedLocations,
	isKnownKey,
	checkDead,
	setDead,
	Unchecked,
	finishCheck,
	loggedKeys,
	loggedKeysFor,
	loggedKeysFor',
	overLocationLogs,
	overLocationLogs',
) where

import Annex.Common
import qualified Annex.Branch
import Logs
import Logs.Presence
import Types.Cluster
import Annex.UUID
import Annex.CatFile
import Annex.VectorClock
import Git.Types (RefDate, Ref)
import qualified Annex

import Data.Time.Clock
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Set as S

{- Log a change in the presence of a key's value in current repository. -}
logStatus :: Key -> LogStatus -> Annex ()
logStatus key s = do
	u <- getUUID
	logChange key u s

{- Run an action that gets the content of a key, and update the log
 - when it succeeds. -}
logStatusAfter :: Key -> Annex Bool -> Annex Bool
logStatusAfter key a = ifM a 
	( do
		logStatus key InfoPresent
		return True
	, return False
	)

{- Log a change in the presence of a key's value in a repository.
 -
 - Cluster UUIDs are not logged. Instead, when a node of a cluster is
 - logged to contain a key, loading the log will include the cluster's
 - UUID.
 -}
logChange :: Key -> UUID -> LogStatus -> Annex ()
logChange key u@(UUID _) s
	| isClusterUUID u = noop
	| otherwise = do
		config <- Annex.getGitConfig
		maybeAddLog
			(Annex.Branch.RegardingUUID [u])
			(locationLogFile config key)
			s
			(LogInfo (fromUUID u))
logChange _ NoUUID _ = noop

{- Returns a list of repository UUIDs that, according to the log, have
 - the value of a key. -}
loggedLocations :: Key -> Annex [UUID]
loggedLocations = getLoggedLocations presentLogInfo

{- Returns a list of repository UUIDs that the location log indicates
 - used to have the vale of a key, but no longer do.
 -}
loggedPreviousLocations :: Key -> Annex [UUID]
loggedPreviousLocations = getLoggedLocations notPresentLogInfo

{- Gets the location log on a particular date. -}
loggedLocationsHistorical :: RefDate -> Key -> Annex [UUID]
loggedLocationsHistorical = getLoggedLocations . historicalLogInfo

{- Gets the locations contained in a git ref. -}
loggedLocationsRef :: Ref -> Annex [UUID]
loggedLocationsRef ref = map (toUUID . fromLogInfo) . getLog <$> catObject ref

{- Parses the content of a log file and gets the locations in it. -}
parseLoggedLocations :: Clusters -> L.ByteString -> [UUID]
parseLoggedLocations clusters l = addClusterUUIDs clusters $
	map (toUUID . fromLogInfo . info)
		(filterPresent (parseLog l))

getLoggedLocations :: (RawFilePath -> Annex [LogInfo]) -> Key -> Annex [UUID]
getLoggedLocations getter key = do
	config <- Annex.getGitConfig
	locs <- map (toUUID . fromLogInfo) <$> getter (locationLogFile config key)
	clusters <- getClusters
	return $ addClusterUUIDs clusters locs

-- Add UUIDs of any clusters whose nodes are in the list.
addClusterUUIDs :: Clusters -> [UUID] -> [UUID]
addClusterUUIDs clusters locs
	| M.null clustermap = locs
	-- ^ optimisation for common case of no clusters
	| otherwise = clusterlocs ++ locs
  where
	clustermap = clusterNodeUUIDs clusters
	clusterlocs = map fromClusterUUID $ S.toList $ 
		S.unions $ mapMaybe findclusters locs
	findclusters u = M.lookup (ClusterNodeUUID u) clustermap

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
 - 
 - The vector clock in the log is updated minimally, so that any
 - other location log changes are guaranteed to overrule this.
 -}
setDead :: Key -> Annex ()
setDead key = do
	config <- Annex.getGitConfig
	let logfile = locationLogFile config key
	ls <- compactLog <$> readLog logfile
	mapM_ (go logfile) (filter (\l -> status l == InfoMissing) ls)
  where
	go logfile l = 
		let u = toUUID (fromLogInfo (info l))
		    c = case date l of
			VectorClock v -> CandidateVectorClock $
				v + realToFrac (picosecondsToDiffTime 1)
			Unknown -> CandidateVectorClock 0
		in addLog' (Annex.Branch.RegardingUUID [u]) logfile InfoDead
			(info l) c

data Unchecked a = Unchecked (Annex (Maybe a))

finishCheck :: Unchecked a -> Annex (Maybe a)
finishCheck (Unchecked a) = a

{- Finds all keys that have location log information.
 - (There may be duplicate keys in the list.)
 -
 - Keys that have been marked as dead are not included.
 -}
loggedKeys :: Annex (Maybe ([Unchecked Key], IO Bool))
loggedKeys = loggedKeys' (not <$$> checkDead)

loggedKeys' :: (Key -> Annex Bool) -> Annex (Maybe ([Unchecked Key], IO Bool))
loggedKeys' check = do
	config <- Annex.getGitConfig
	Annex.Branch.files >>= \case
		Nothing -> return Nothing
		Just (bfs, cleanup) -> do
			let l = mapMaybe (defercheck <$$> locationLogFileKey config) bfs
			return (Just (l, cleanup))
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
loggedKeysFor :: UUID -> Annex (Maybe [Key])
loggedKeysFor u = loggedKeysFor' u >>= \case
	Nothing -> return Nothing
	Just (l, cleanup) -> do
		l' <- catMaybes <$> mapM finishCheck l
		liftIO $ void cleanup
		return (Just l')

loggedKeysFor' :: UUID -> Annex (Maybe ([Unchecked Key], IO Bool))
loggedKeysFor' u = loggedKeys' isthere
  where
	isthere k = do
		us <- loggedLocations k
		let !there = u `elem` us
		return there

{- This is much faster than loggedKeys. -}
overLocationLogs :: v -> (Key -> [UUID] -> v -> Annex v) -> Annex v
overLocationLogs v = overLocationLogs' v (flip const)

overLocationLogs'
	 :: v 
	-> (Annex (Maybe (Key, RawFilePath, Maybe L.ByteString)) -> Annex v -> Annex v)
        -> (Key -> [UUID] -> v -> Annex v)
        -> Annex v
overLocationLogs' iv discarder keyaction = do
	config <- Annex.getGitConfig
	clusters <- getClusters
		
	let getk = locationLogFileKey config
	let go v reader = reader >>= \case
		Just (k, f, content) -> discarder reader $ do
			-- precache to make checkDead fast, and also to
			-- make any accesses done in keyaction fast.
			maybe noop (Annex.Branch.precache f) content
			ifM (checkDead k)
				( go v reader
				, do
					!v' <- keyaction k (maybe [] (parseLoggedLocations clusters) content) v
					go v' reader
				)
		Nothing -> return v

	Annex.Branch.overBranchFileContents getk (go iv) >>= \case
		Just r -> return r
		Nothing -> giveup "This repository is read-only, and there are unmerged git-annex branches, which prevents operating on allu keys. (Set annex.merge-annex-branches to false to ignore the unmerged git-annex branches.)"

-- Cannot import Logs.Cluster due to a cycle.
-- Annex.clusters gets populated when starting up git-annex.
getClusters :: Annex Clusters
getClusters = fromMaybe noClusters <$> Annex.getState Annex.clusters
