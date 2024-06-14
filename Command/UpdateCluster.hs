{- git-annex command
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.UpdateCluster where

import Command
import qualified Annex
import Types.Cluster
import Logs.Cluster
import qualified Remote as R
import qualified Types.Remote as R
import qualified Command.UpdateProxy
import Utility.SafeOutput

import qualified Data.Map as M
import qualified Data.Set as S

cmd :: Command
cmd = noMessages $ command "updatecluster" SectionSetup 
	"update records of cluster nodes"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing $ do
	commandAction start
	commandAction Command.UpdateProxy.start

start :: CommandStart
start = startingCustomOutput (ActionItemOther Nothing) $ do
	rs <- R.remoteList
	let getnode r = do
		clusternames <- remoteAnnexClusterNode (R.gitconfig r)
		return $ M.fromList $ zip clusternames (repeat (S.singleton r))
	let myclusternodes = M.unionsWith S.union (mapMaybe getnode rs)
	myclusters <- annexClusters <$> Annex.getGitConfig
	recordedclusters <- getClusters
	descs <- R.uuidDescriptions
	
	-- Update the cluster log to list the currently configured nodes
	-- of each configured cluster.
	forM_ (M.toList myclusters) $ \(clustername, cu) -> do
		let mynodesremotes = fromMaybe mempty $
			M.lookup clustername myclusternodes
		let mynodes = S.map (ClusterNodeUUID . R.uuid) mynodesremotes
		let recordednodes = fromMaybe mempty $ M.lookup cu $
			clusterUUIDs recordedclusters
		if recordednodes == mynodes
			then liftIO $ putStrLn $ safeOutput $
				"No cluster node changes for cluster: " ++ clustername
			else do
				describechanges descs clustername recordednodes mynodes mynodesremotes
				recordCluster cu mynodes

	next $ return True
  where
	describechanges descs clustername oldnodes mynodes mynodesremotes = do
		forM_ (S.toList mynodesremotes) $ \r ->
			unless (S.member (ClusterNodeUUID (R.uuid r)) oldnodes) $
				liftIO $ putStrLn $ safeOutput $
					"Added node " ++ R.name r ++ " to cluster: " ++ clustername
		forM_ (S.toList oldnodes) $ \n ->
			unless (S.member n mynodes) $ do
				let desc = maybe (fromUUID (fromClusterNodeUUID n)) fromUUIDDesc $
					M.lookup (fromClusterNodeUUID n) descs
				liftIO $ putStrLn $ safeOutput $
					"Removed node " ++ desc ++ " from cluster: " ++ clustername
