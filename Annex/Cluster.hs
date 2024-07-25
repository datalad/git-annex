{- clusters
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module Annex.Cluster where

import Annex.Common
import qualified Annex
import Types.Cluster
import Logs.Cluster
import P2P.Proxy
import P2P.Protocol
import P2P.IO
import Annex.Proxy
import Annex.UUID
import Logs.Location
import Logs.PreferredContent
import Types.Command
import Remote.List
import qualified Remote
import qualified Types.Remote as Remote

import qualified Data.Map as M
import qualified Data.Set as S
import System.Random

{- Proxy to a cluster. -}
proxyCluster 
	:: ClusterUUID
	-> CommandPerform
	-> ServerMode
	-> ClientSide
	-> (forall a. Annex () -> ((a -> CommandPerform) -> Annex (Either ProtoFailure a) -> CommandPerform))
	-> CommandPerform
proxyCluster clusteruuid proxydone servermode clientside protoerrhandler = do
	getClientProtocolVersion (fromClusterUUID clusteruuid) clientside
		withclientversion (protoerrhandler noop)
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
		-- nodes, and allows nodes to have different protocol
		-- versions.
		let protocolversion = min maxProtocolVersion clientmaxversion
		sendClientProtocolVersion clientside othermsg protocolversion
			(getclientbypass protocolversion) (protoerrhandler noop)
	withclientversion Nothing = proxydone

	getclientbypass protocolversion othermsg =
		getClientBypass clientside protocolversion othermsg
			(withclientbypass protocolversion) (protoerrhandler noop)

	withclientbypass protocolversion (bypassuuids, othermsg) = do
		(selectnode, closenodes) <- clusterProxySelector clusteruuid
			protocolversion bypassuuids
		concurrencyconfig <- getConcurrencyConfig
		proxystate <- liftIO mkProxyState
		let proxyparams = ProxyParams
			{ proxyMethods = proxymethods
			, proxyState = proxystate
			, proxyServerMode = servermode
			, proxyClientSide = clientside
			, proxyUUID = fromClusterUUID clusteruuid
			, proxySelector = selectnode
			, proxyConcurrencyConfig = concurrencyconfig
			, proxyProtocolVersion = protocolversion
			}
		proxy proxydone proxyparams othermsg
			(protoerrhandler closenodes)

clusterProxySelector :: ClusterUUID -> ProtocolVersion -> Bypass -> Annex (ProxySelector, Annex ())
clusterProxySelector clusteruuid protocolversion (Bypass bypass) = do
	nodeuuids <- (fromMaybe S.empty . M.lookup clusteruuid . clusterUUIDs)
		<$> getClusters
	myclusters <- annexClusters <$> Annex.getGitConfig
	allremotes <- concat . Remote.byCost <$> remoteList
	hereu <- getUUID
	let bypass' = S.insert hereu bypass
	let clusterremotes = filter (isnode bypass' allremotes nodeuuids myclusters) allremotes
	fastDebug "Annex.Cluster" $ unwords
		[ "cluster gateway at", fromUUID hereu
		, "connecting to", show (map Remote.name clusterremotes)
		, "bypass", show (S.toList bypass)
		]
	nodes <- mapM (proxyRemoteSide protocolversion (Bypass bypass')) clusterremotes
	let closenodes = mapM_ closeRemoteSide nodes
	let proxyselector = ProxySelector
		{ proxyCHECKPRESENT = nodecontaining nodes
		, proxyGET = nodecontaining nodes
		-- The key is sent to multiple nodes at the same time,
		-- skipping nodes where it's known/expected to already be
		-- present to avoid needing to connect to those, and
		-- skipping nodes where it's not preferred content.
		, proxyPUT = \af k -> do
			locs <- S.fromList <$> loggedLocations k
			let l = filter (flip S.notMember locs . Remote.uuid . remote) nodes
			l' <- filterM (\n -> isPreferredContent (Just (Remote.uuid (remote n))) mempty (Just k) af True) l
			-- PUT to no nodes doesn't work, so fall
			-- back to all nodes.
			return $ nonempty [l', l] nodes
		-- Remove the key from every node that contains it.
		-- But, since it's possible the location log for some nodes
		-- could be out of date, actually try to remove from every
		-- node.
		, proxyREMOVE = const (pure nodes)
		, proxyGETTIMESTAMP = pure nodes
		-- Content is not locked on the cluster as a whole,
		-- instead it can be locked on individual nodes that are
		-- proxied to the client.
		, proxyLOCKCONTENT = const (pure Nothing)
		, proxyUNLOCKCONTENT = pure Nothing
		}
	return (proxyselector, closenodes)
  where
	-- Nodes of the cluster have remote.name.annex-cluster-node
	-- containing its name. 
	--
	-- Or, a node can be the cluster proxied by another gateway.
	isnode bypass' rs nodeuuids myclusters r = 
		case remoteAnnexClusterNode (Remote.gitconfig r) of
			Just names
				| any (isclustername myclusters) names ->
					flip S.member nodeuuids $ 
						ClusterNodeUUID $ Remote.uuid r
				| otherwise -> False
			Nothing -> isclusterviagateway bypass' rs r
	
	-- Is this remote the same cluster, proxied via another gateway?
	--
	-- Must avoid bypassed gateways to prevent cycles.
	isclusterviagateway bypass' rs r = 
		case mkClusterUUID (Remote.uuid r) of
			Just cu | cu == clusteruuid ->
				case remoteAnnexProxiedBy (Remote.gitconfig r) of
					Just proxyuuid | proxyuuid `S.notMember` bypass' ->
						not $ null $
							filter isclustergateway $
							filter (\p -> Remote.uuid p == proxyuuid) rs
					_ -> False
			_ -> False
	
	isclustergateway r = any (== clusteruuid) $ 
		remoteAnnexClusterGateway $ Remote.gitconfig r

	isclustername myclusters name = 
		M.lookup name myclusters == Just clusteruuid
	
	nodecontaining nodes k = do
		locs <- S.fromList <$> loggedLocations k
		case filter (flip S.member locs . Remote.uuid . remote) nodes of
			[] -> return Nothing
			(node:[]) -> return (Just node)
			(node:rest) ->
				-- The list of nodes is ordered by cost.
				-- Use any of the ones with equally low
				-- cost.
				let lowestcost = Remote.cost (remote node)
				    samecost = node : takeWhile (\n -> Remote.cost (remote n) == lowestcost) rest
				in do
					n <- liftIO $ getStdRandom $
						randomR (0, length samecost - 1)
					return (Just (samecost !! n))
		
	nonempty (l:ls) fallback
		| null l = nonempty ls fallback
		| otherwise = l
	nonempty [] fallback = fallback
