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
	overLocationLogsJournal,
	parseLoggedLocations,
	parseLoggedLocationsWithoutClusters,
) where

import Annex.Common
import qualified Annex.Branch
import Annex.Branch (FileContents)
import Annex.RepoSize.LiveUpdate
import Logs
import Logs.Presence
import Types.Cluster
import Annex.UUID
import Annex.CatFile
import Annex.VectorClock
import Git.Types (RefDate, Ref, Sha)
import qualified Annex

import Data.Time.Clock
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Set as S

{- Log a change in the presence of a key's value in current repository. -}
logStatus :: LiveUpdate -> Key -> LogStatus -> Annex ()
logStatus lu key s = do
	u <- getUUID
	logChange lu key u s

{- Run an action that gets the content of a key, and update the log
 - when it succeeds. -}
logStatusAfter :: LiveUpdate -> Key -> Annex Bool -> Annex Bool
logStatusAfter lu key a = ifM a
	( do
		logStatus lu key InfoPresent
		return True
	, return False
	)

{- Log a change in the presence of a key's value in a repository.
 -
 - Cluster UUIDs are not logged. Instead, when a node of a cluster is
 - logged to contain a key, loading the log will include the cluster's
 - UUID.
 -}
logChange :: LiveUpdate -> Key -> UUID -> LogStatus -> Annex ()
logChange lu key u@(UUID _) s
	| isClusterUUID u = noop
	| otherwise = do
		config <- Annex.getGitConfig
		changed <- maybeAddLog
			(Annex.Branch.RegardingUUID [u])
			(locationLogFile config key)
			s
			(LogInfo (fromUUID u))
		when changed $
			updateRepoSize lu u key s
logChange _ _ NoUUID _ = noop

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

{- Parses the content of a log file and gets the locations in it.
 -
 - Adds the UUIDs of any clusters whose nodes are in the list.
 -}
parseLoggedLocations :: Clusters -> L.ByteString -> [UUID]
parseLoggedLocations clusters =
	addClusterUUIDs clusters . parseLoggedLocationsWithoutClusters

parseLoggedLocationsWithoutClusters :: L.ByteString -> [UUID]
parseLoggedLocationsWithoutClusters l =
	map (toUUID . fromLogInfo . info)
		(filterPresent (parseLog l))

getLoggedLocations :: (RawFilePath -> Annex [LogInfo]) -> Key -> Annex [UUID]
getLoggedLocations getter key = do
	config <- Annex.getGitConfig
	locs <- map (toUUID . fromLogInfo) <$> getter (locationLogFile config key)
	clusters <- getClusters
	return $ addClusterUUIDs clusters locs

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
	go logfile l = do
		let u = toUUID (fromLogInfo (info l))
		    c = case date l of
			VectorClock v -> CandidateVectorClock $
				v + realToFrac (picosecondsToDiffTime 1)
			Unknown -> CandidateVectorClock 0
		addLog' (Annex.Branch.RegardingUUID [u]) logfile InfoDead
			(info l) c
		updateRepoSize NoLiveUpdate u key InfoDead

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
overLocationLogs
	:: Bool
	-> Bool
	-> v
	-> (Key -> [UUID] -> v -> Annex v)
	-> Annex (Annex.Branch.UnmergedBranches (v, Sha))
overLocationLogs ignorejournal noclusters v =
	overLocationLogs' ignorejournal noclusters v (flip const)

overLocationLogs'
	:: Bool
	-> Bool
	-> v
	-> (Annex (FileContents Key Bool) -> Annex v -> Annex v)
        -> (Key -> [UUID] -> v -> Annex v)
        -> Annex (Annex.Branch.UnmergedBranches (v, Sha))
overLocationLogs' ignorejournal noclusters iv discarder keyaction = do
	mclusters <- if noclusters then pure Nothing else Just <$> getClusters
	overLocationLogsHelper
		(Annex.Branch.overBranchFileContents ignorejournal)
		(\locparser _ _ content -> pure (locparser (fst <$> content)))
		True
		iv
		discarder
		keyaction
		mclusters

type LocChanges = 
	( S.Set UUID
	-- ^ locations that are in the journal, but not in the
	-- git-annex branch
	, S.Set UUID
	-- ^ locations that are in the git-annex branch,
	-- but have been removed in the journal
	)

{- Like overLocationLogs, but only adds changes in journalled files
 - compared with what was logged in the git-annex branch at the specified
 - commit sha. -}
overLocationLogsJournal
	:: v
	-> Sha
	-> (Key -> LocChanges -> v -> Annex v)
	-> Maybe Clusters
	-> Annex v
overLocationLogsJournal v branchsha keyaction mclusters = 
	overLocationLogsHelper
		(Annex.Branch.overJournalFileContents handlestale)
		changedlocs
		False
		-- ^ do not precache journalled content, which may be stale
		v (flip const) keyaction 
		mclusters
  where
	handlestale _ journalcontent = return (journalcontent, Just True)

	changedlocs locparser _key logf (Just (journalcontent, isstale)) = do
		branchcontent <- Annex.Branch.getRef branchsha logf
		let branchlocs = S.fromList $ locparser $ Just branchcontent
		let journallocs = S.fromList $ locparser $ Just $ case isstale of
			Just True -> Annex.Branch.combineStaleJournalWithBranch
				branchcontent journalcontent
			_ -> journalcontent
		return
			( S.difference journallocs branchlocs
			, S.difference branchlocs journallocs
			)
	changedlocs _ _ _ Nothing = pure (S.empty, S.empty)

overLocationLogsHelper
	:: ((RawFilePath -> Maybe Key) -> (Annex (FileContents Key b) -> Annex v) -> Annex a)
	-> ((Maybe L.ByteString -> [UUID]) -> Key -> RawFilePath -> Maybe (L.ByteString, Maybe b) -> Annex u)
	-> Bool
	-> v
	-> (Annex (FileContents Key b) -> Annex v -> Annex v)
        -> (Key -> u -> v -> Annex v)
	-> (Maybe Clusters)
        -> Annex a
overLocationLogsHelper runner locparserrunner canprecache iv discarder keyaction mclusters = do
	config <- Annex.getGitConfig

	let locparser = maybe
		parseLoggedLocationsWithoutClusters
		parseLoggedLocations
		mclusters
	let locparser' = maybe [] locparser
	let getk = locationLogFileKey config
	let go v reader = reader >>= \case
		Just (k, f, content) -> discarder reader $ do
			-- precache to make checkDead fast, and also to
			-- make any accesses done in keyaction fast.
			when canprecache $
				maybe noop (Annex.Branch.precache f . fst) content
			ifM (checkDead k)
				( go v reader
				, do
					!locs <- locparserrunner locparser' k f content
					!v' <- keyaction k locs v
					go v' reader
				)
		Nothing -> return v

	runner getk (go iv)

-- Cannot import Logs.Cluster due to a cycle.
-- Annex.clusters gets populated when starting up git-annex.
getClusters :: Annex Clusters
getClusters = maybe (pure noClusters) id =<< Annex.getState Annex.clusters
