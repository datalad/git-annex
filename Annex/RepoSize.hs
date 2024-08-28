{- git-annex repo sizes
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Annex.RepoSize (
	getRepoSizes,
	getLiveRepoSizes,
) where

import Annex.Common
import qualified Annex
import Annex.Branch (UnmergedBranches(..), getBranch)
import qualified Database.RepoSize as Db
import Annex.Journal
import Annex.RepoSize.LiveUpdate
import Logs
import Logs.Location
import Logs.UUID
import Git.Types (Sha)
import Git.FilePath
import Git.CatFile
import qualified Git.DiffTree as DiffTree

import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.Map.Strict as M
import qualified Data.Set as S

{- Gets the repo size map. Cached for speed.
 -
 - Note that this is the size of all repositories as of the first time it
 - was called. It does not update while git-annex is running.
 -}
getRepoSizes :: Bool -> Annex (M.Map UUID RepoSize)
getRepoSizes quiet = M.map fst <$> getRepoSizes' quiet

getRepoSizes' :: Bool -> Annex (M.Map UUID (RepoSize, SizeOffset))
getRepoSizes' quiet = do
	rsv <- Annex.getRead Annex.reposizes
	liftIO (takeMVar rsv) >>= \case
		Just sizemap -> do
			liftIO $ putMVar rsv (Just sizemap)
			return sizemap
		Nothing -> calcRepoSizes quiet rsv

{- Like getRepoSizes, but with live updates. -}
getLiveRepoSizes :: Bool -> Annex (M.Map UUID RepoSize)
getLiveRepoSizes quiet = do
	sizemap <- getRepoSizes' quiet
	go sizemap `onException` return (M.map fst sizemap)
  where
	go sizemap = do
		h <- Db.getRepoSizeHandle
		checkStaleSizeChanges h
		liveoffsets <- liftIO $ Db.liveRepoOffsets h
		let calc u (RepoSize size, SizeOffset startoffset) =
			case M.lookup u liveoffsets of
				Nothing -> RepoSize size
				Just (SizeOffset offset) -> RepoSize $
					size + (offset - startoffset)
		return $ M.mapWithKey calc sizemap

{- Fills an empty Annex.reposizes MVar with current information
 - from the git-annex branch, supplimented with journalled but
 - not yet committed information.
 -}
calcRepoSizes :: Bool -> MVar (Maybe (M.Map UUID (RepoSize, SizeOffset))) -> Annex (M.Map UUID (RepoSize, SizeOffset))
calcRepoSizes quiet rsv = go `onException` failed
  where
	go = do
		h <- Db.getRepoSizeHandle
		(oldsizemap, moldbranchsha) <- liftIO $ Db.getRepoSizes h
		!sizemap <- case moldbranchsha of
			Nothing -> calculatefromscratch h
			Just oldbranchsha -> do
				currbranchsha <- getBranch
				if oldbranchsha == currbranchsha
					then calcJournalledRepoSizes h oldsizemap oldbranchsha
					else incrementalupdate h oldsizemap oldbranchsha currbranchsha
		liftIO $ putMVar rsv (Just sizemap)
		return sizemap
	
	calculatefromscratch h = do
		unless quiet $
			showSideAction "calculating repository sizes"
		use h =<< calcBranchRepoSizes
	
	incrementalupdate h oldsizemap oldbranchsha currbranchsha =
		use h =<< diffBranchRepoSizes quiet oldsizemap oldbranchsha currbranchsha

	use h (sizemap, branchsha) = do
		liftIO $ Db.setRepoSizes h sizemap branchsha
		calcJournalledRepoSizes h sizemap branchsha

	failed = do
		liftIO $ putMVar rsv (Just M.empty)
		return M.empty

{- Sum up the sizes of all keys in all repositories, from the information
 - in the git-annex branch, but not the journal. Retuns the sha of the
 - branch commit that was used.
 -
 - The map includes the UUIDs of all known repositories, including
 - repositories that are empty. But clusters are not included.
 -
 - Note that private repositories, which do not get recorded in
 - the git-annex branch, will have 0 size. journalledRepoSizes
 - takes care of getting repo sizes for those.
 -}
calcBranchRepoSizes :: Annex (M.Map UUID RepoSize, Sha)
calcBranchRepoSizes = do
	knownuuids <- M.keys <$> uuidDescMap
	let startmap = M.fromList $ map (\u -> (u, RepoSize 0)) knownuuids
	overLocationLogs True True startmap accumsizes >>= \case
		UnmergedBranches v -> return v
		NoUnmergedBranches v -> return v
  where
	accumsizes k locs m = return $
		foldl' (flip $ M.alter $ addKeyRepoSize k) m locs

{- Given the RepoSizes calculated from the git-annex branch, updates it with
 - data from journalled location logs.
  -}
calcJournalledRepoSizes
	:: Db.RepoSizeHandle
	-> M.Map UUID RepoSize
	-> Sha 
	-> Annex (M.Map UUID (RepoSize, SizeOffset))
calcJournalledRepoSizes h startmap branchsha =
	-- Lock the journal to prevent updates to the size offsets
	-- in the repository size database while this is processing
	-- the journal files.
	lockJournal $ \_jl -> do
		sizemap <- overLocationLogsJournal startmap branchsha 
			(\k v m' -> pure (accumRepoSizes k v m'))
			Nothing
		offsets <- liftIO $ Db.recordedRepoOffsets h
		let getoffset u = fromMaybe (SizeOffset 0) $ M.lookup u offsets
		return $ M.mapWithKey (\u sz -> (sz, getoffset u)) sizemap

{- Incremental update by diffing. -}
diffBranchRepoSizes :: Bool -> M.Map UUID RepoSize -> Sha -> Sha -> Annex (M.Map UUID RepoSize, Sha)
diffBranchRepoSizes quiet oldsizemap oldbranchsha newbranchsha = do
	g <- Annex.gitRepo
	catObjectStream g $ \feeder closer reader -> do
		(l, cleanup) <- inRepo $
			DiffTree.diffTreeRecursive oldbranchsha newbranchsha
		feedtid <- liftIO $ async $ do
			forM_ l $ feedpairs feeder
			closer
		newsizemap <- readpairs 100000 reader oldsizemap Nothing
		liftIO $ wait feedtid
		ifM (liftIO cleanup)
			( do
				newsizemap' <- addemptyrepos newsizemap
				return (newsizemap', newbranchsha)
			, return (oldsizemap, oldbranchsha)
			)
  where
	feedpairs feeder ti = 
		let f = getTopFilePath (DiffTree.file ti)
		in case extLogFileKey locationLogExt f of
			Nothing -> noop
			Just k -> do
				feeder (k, DiffTree.srcsha ti)
				feeder (k, DiffTree.dstsha ti)

	readpairs n reader sizemap Nothing = liftIO reader >>= \case
		Just (_k, oldcontent) -> readpairs n reader sizemap (Just oldcontent)
		Nothing -> return sizemap
	readpairs n reader sizemap (Just oldcontent) = liftIO reader >>= \case
		Just (k, newcontent) ->
			let prevlog = parselog oldcontent
			    currlog = parselog newcontent
			    newlocs = S.difference currlog prevlog
			    removedlocs = S.difference prevlog currlog
			    !sizemap' = accumRepoSizes k (newlocs, removedlocs) sizemap
			in do
				n' <- if quiet
					then pure n
					else countdownToMessage n $
						showSideAction "calculating repository sizes"
				readpairs n' reader sizemap' Nothing
		Nothing -> return sizemap
	parselog = maybe mempty (S.fromList . parseLoggedLocationsWithoutClusters)
	
	addemptyrepos newsizemap = do
		knownuuids <- M.keys <$> uuidDescMap
		return $ foldl'
			(\m u -> M.insertWith (flip const) u (RepoSize 0) m)
			newsizemap
			knownuuids

addKeyRepoSize :: Key -> Maybe RepoSize -> Maybe RepoSize
addKeyRepoSize k mrs = case mrs of
	Just (RepoSize sz) -> Just $ RepoSize $ sz + ksz
	Nothing -> Just $ RepoSize ksz
  where
	ksz = fromMaybe 0 $ fromKey keySize k

removeKeyRepoSize :: Key -> Maybe RepoSize -> Maybe RepoSize
removeKeyRepoSize k mrs = case mrs of
	Just (RepoSize sz) -> Just $ RepoSize $ sz - ksz
	Nothing -> Nothing
  where
	ksz = fromMaybe 0 $ fromKey keySize k

accumRepoSizes :: Key -> (S.Set UUID, S.Set UUID) -> M.Map UUID RepoSize -> M.Map UUID RepoSize
accumRepoSizes k (newlocs, removedlocs) sizemap = 
	let !sizemap' = foldl' (flip $ M.alter $ addKeyRepoSize k) sizemap newlocs
	in foldl' (flip $ M.alter $ removeKeyRepoSize k) sizemap' removedlocs
