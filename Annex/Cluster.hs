{- clusters
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Annex.Cluster where

import Annex.Common
import qualified Annex
import Types.Cluster
import Logs.Cluster
import P2P.Proxy
import P2P.Protocol
import P2P.IO
import Annex.Proxy
import Logs.Location
import Types.Command
import Remote.List
import qualified Remote
import qualified Types.Remote as Remote

import qualified Data.Map as M
import qualified Data.Set as S

{- Proxy to a cluster. -}
proxyCluster 
	:: ClusterUUID
	-> CommandPerform
	-> ServerMode
	-> ClientSide
	-> (forall a. ((a -> CommandPerform) -> Annex (Either ProtoFailure a) -> CommandPerform))
	-> CommandPerform
proxyCluster clusteruuid proxydone servermode clientside protoerrhandler = do
	getClientProtocolVersion (fromClusterUUID clusteruuid) clientside
		withclientversion protoerrhandler
  where
	proxymethods = ProxyMethods
		{ removedContent = \u k -> logChange k u InfoMissing
		, addedContent = \u k -> logChange k u InfoPresent
		}
	
	withclientversion (Just (clientmaxversion, othermsg)) = do
		-- The protocol versions supported by the nodes are not
		-- known at this point, and would be too expensive to
		-- determine. Instead, pick the newest protocol version
		-- that we and the client both speak. The proxy code
		-- checks protocol versions when operating on multiple
		-- nodes.
		let protocolversion = min maxProtocolVersion clientmaxversion
		selectnode <- clusterProxySelector clusteruuid protocolversion
		proxy proxydone proxymethods servermode clientside 
			(fromClusterUUID clusteruuid)
			selectnode protocolversion othermsg protoerrhandler
	withclientversion Nothing = proxydone

clusterProxySelector :: ClusterUUID -> ProtocolVersion -> Annex ProxySelector
clusterProxySelector clusteruuid protocolversion = do
	nodes <- (fromMaybe S.empty . M.lookup clusteruuid . clusterUUIDs)
		<$> getClusters
	clusternames <- annexClusters <$> Annex.getGitConfig
	remotes <- filter (isnode nodes clusternames) <$> remoteList
	remotesides <- mapM (proxySshRemoteSide protocolversion) remotes
	return $ ProxySelector
		{ proxyCHECKPRESENT = nodecontaining remotesides
		, proxyGET = nodecontaining remotesides
		-- The key is sent to multiple nodes at the same time,
		-- skipping nodes where it's known/expected to already be
		-- present to avoid needing to connect to those.
		, proxyPUT = \k -> do
			locs <- S.fromList <$> loggedLocations k
			let l = filter (flip S.notMember locs . remoteUUID) remotesides
			return $ if null l
				then remotesides
				else l
		-- Remove the key from every node that contains it.
		-- But, since it's possible the location log for some nodes
		-- could be out of date, actually try to remove from every
		-- node.
		, proxyREMOVE = const (pure remotesides)
		-- Content is not locked on the cluster as a whole,
		-- instead it can be locked on individual nodes that are
		-- proxied to the client.
		, proxyLOCKCONTENT = const (pure Nothing)
		, proxyUNLOCKCONTENT = pure Nothing
		}
  where
	-- Nodes of the cluster have remote.name.annex-cluster-node
	-- containing its name.
	isnode nodes clusternames r = 
		case remoteAnnexClusterNode (Remote.gitconfig r) of
			Nothing -> False
			Just names
				| any (isclustername clusternames) names ->
					flip S.member nodes $ 
						ClusterNodeUUID $ Remote.uuid r
				| otherwise -> False
	
	isclustername clusternames name = 
		M.lookup name clusternames == Just clusteruuid
	
	nodecontaining remotesides k = do
		locs <- S.fromList <$> loggedLocations k
		case filter (flip S.member locs . remoteUUID) remotesides of
			-- TODO: Avoid always using same remote
			(r:_) -> return (Just r)
			[] -> return Nothing
		
