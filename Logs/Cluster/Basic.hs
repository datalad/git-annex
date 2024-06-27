{- git-annex cluster log, basics
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Logs.Cluster.Basic (
	module Types.Cluster,
	getClustersWith,
	recordCluster,	
) where

import qualified Annex
import Annex.Common
import qualified Annex.Branch
import Types.Cluster
import Logs
import Logs.UUIDBased
import Logs.MapLog

import qualified Data.Set as S
import qualified Data.Map as M
import Data.ByteString.Builder
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString.Lazy as L

{- Gets the clusters. Note that this includes any dead nodes,
 - unless a function is provided to remove them.
 -}
getClustersWith
	:: (M.Map ClusterUUID (S.Set ClusterNodeUUID)
		-> M.Map ClusterUUID (S.Set ClusterNodeUUID))
	-> Annex Clusters
getClustersWith removedeadnodes = do
	m <- removedeadnodes 
		. convclusteruuids 
		. M.map value 
		. fromMapLog 
		. parseClusterLog
		<$> Annex.Branch.get clusterLog
	return $ Clusters
		{ clusterUUIDs = m
		, clusterNodeUUIDs = M.foldlWithKey inverter mempty m
		}
  where
	convclusteruuids :: M.Map UUID (S.Set ClusterNodeUUID) -> M.Map ClusterUUID (S.Set ClusterNodeUUID)
	convclusteruuids = M.fromList 
		. mapMaybe (\(mk, v) -> (, v) <$> mk)
		. M.toList . M.mapKeys mkClusterUUID
	inverter m k v = M.unionWith (<>) m 
		(M.fromList (map (, S.singleton k) (S.toList v)))

recordCluster :: ClusterUUID -> S.Set ClusterNodeUUID -> Annex ()
recordCluster clusteruuid nodeuuids = do
	-- If a private UUID has been configured as a cluster node, 
	-- avoid leaking it into the git-annex log.
	privateuuids <- annexPrivateRepos <$> Annex.getGitConfig
	let nodeuuids' = S.filter
		(\(ClusterNodeUUID n) -> S.notMember n privateuuids)
		nodeuuids
	
	c <- currentVectorClock
	Annex.Branch.change (Annex.Branch.RegardingUUID [fromClusterUUID clusteruuid]) clusterLog $
		(buildLogNew buildClusterNodeList)
			. changeLog c (fromClusterUUID clusteruuid) nodeuuids'
			. parseClusterLog

buildClusterNodeList :: S.Set ClusterNodeUUID -> Builder
buildClusterNodeList = assemble 
	. map (buildUUID . fromClusterNodeUUID) 
	. S.toList
  where
	assemble [] = mempty
	assemble (x:[]) = x
	assemble (x:y:l) = x <> " " <> assemble (y:l)

parseClusterLog :: L.ByteString -> Log (S.Set ClusterNodeUUID)
parseClusterLog = parseLogNew parseClusterNodeList

parseClusterNodeList :: A.Parser (S.Set ClusterNodeUUID)
parseClusterNodeList = S.fromList <$> many parseword
  where
	parseword = parsenode
		<* ((const () <$> A8.char ' ') <|> A.endOfInput)
	parsenode = ClusterNodeUUID
		<$> (toUUID <$> A8.takeWhile1 (/= ' '))

