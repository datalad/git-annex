{- clusters
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Annex.Cluster where

import Annex.Common
import Types.Cluster
import Logs.Cluster
import P2P.Proxy
import P2P.Protocol
import P2P.IO
import Logs.Location
import Types.Command
import Remote.List
import qualified Remote

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
		-- that we and the client both speak.
		let protocolversion = min maxProtocolVersion clientmaxversion
		selectnode <- clusterProxySelector clusteruuid
		proxy proxydone proxymethods servermode clientside selectnode
			protocolversion othermsg protoerrhandler
	withclientversion Nothing = proxydone

clusterProxySelector :: ClusterUUID -> Annex ProxySelector
clusterProxySelector clusteruuid = do
	nodes <- (fromMaybe S.empty . M.lookup clusteruuid . clusterUUIDs)
		<$> getClusters
	remotes <- filter (flip S.member nodes . ClusterNodeUUID . Remote.uuid)
		<$> remoteList
	return $ ProxySelector
		{ proxyCHECKPRESENT = \k -> error "TODO"
		, proxyLOCKCONTENT = \k -> error "TODO"
		, proxyUNLOCKCONTENT = error "TODO"
		, proxyREMOVE = \k -> error "TODO"
		, proxyGET = \k -> error "TODO"
		, proxyPUT = \k -> error "TODO"
		}
