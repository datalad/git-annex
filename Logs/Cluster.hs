{- git-annex cluster log
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Logs.Cluster (
	module Types.Cluster,
	getClusters,
	loadClusters,
	preLoadClusters,
	recordCluster,
) where

import qualified Annex
import Annex.Common
import Types.Cluster
import Logs.Cluster.Basic
import Logs.Trust

import qualified Data.Map as M
import qualified Data.Set as S

getClusters :: Annex Clusters
getClusters = maybe loadClusters id =<< Annex.getState Annex.clusters

{- This works around a module dependency loop. -}
preLoadClusters :: Annex ()
preLoadClusters = Annex.changeState $ \s ->
	s { Annex.clusters = Just loadClusters }

{- Loads the clusters and caches it for later.
 -
 - This takes care of removing dead nodes from clusters,
 - to avoid inserting the cluster uuid into the location
 - log when only dead nodes contain the content of a key.
 -}
loadClusters :: Annex Clusters
loadClusters = do
	dead <- (S.fromList . map ClusterNodeUUID)
		<$> trustGet DeadTrusted
	clusters <- getClustersWith (M.map (`S.difference` dead))
	Annex.changeState $ \s -> s { Annex.clusters = Just (pure clusters) }
	return clusters
