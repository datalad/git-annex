{- git-annex command
 -
 - Copyright 2018-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.P2PStdIO where

import Command
import P2P.IO
import P2P.Annex
import P2P.Proxy
import qualified P2P.Protocol as P2P
import qualified Annex
import Annex.UUID
import qualified CmdLine.GitAnnexShell.Checks as Checks
import Remote.Helper.Ssh (openP2PShellConnection', closeP2PShellConnection)
import Logs.Location
import Logs.Cluster
import qualified Remote

import System.IO.Error

cmd :: Command
cmd = noMessages $ command "p2pstdio" SectionPlumbing
	"communicate in P2P protocol over stdio"
	paramUUID (withParams seek)

seek :: CmdParams -> CommandSeek
seek [u] = commandAction $ start $ toUUID u
seek _ = giveup "missing UUID parameter"

start :: UUID -> CommandStart
start theiruuid = startingCustomOutput (ActionItemOther Nothing) $ do
	servermode <- liftIO $ do
		ro <- Checks.checkEnvSet Checks.readOnlyEnv
		ao <- Checks.checkEnvSet Checks.appendOnlyEnv
		return $ case (ro, ao) of
			(True, _) -> P2P.ServeReadOnly
			(False, True) -> P2P.ServeAppendOnly
			(False, False) -> P2P.ServeReadWrite
	Annex.getState Annex.proxyremote >>= \case
		Nothing ->
			performLocal theiruuid servermode
		Just (Right r) ->
			performProxy theiruuid servermode r
		Just (Left clusteruuid) -> 
			performProxyCluster theiruuid clusteruuid servermode

performLocal :: UUID -> P2P.ServerMode -> CommandPerform
performLocal theiruuid servermode = do
	myuuid <- getUUID
	let conn = stdioP2PConnection Nothing
	let server = do
		P2P.net $ P2P.sendMessage (P2P.AUTH_SUCCESS myuuid)
		P2P.serveAuthed servermode myuuid
	runst <- liftIO $ mkRunState $ Serving theiruuid Nothing
	p2pErrHandler (const p2pDone) (runFullProto runst conn server)

performProxy :: UUID -> P2P.ServerMode -> Remote -> CommandPerform
performProxy clientuuid servermode remote = do
	clientside <- proxyClientSide clientuuid
	getClientProtocolVersion (Remote.uuid remote) clientside 
		(withclientversion clientside)
		p2pErrHandler
  where
	withclientversion clientside (Just (clientmaxversion, othermsg)) = do
		remoteside <- proxySshRemoteSide clientmaxversion remote
		protocolversion <- either (const (min P2P.maxProtocolVersion clientmaxversion)) id
			<$> runRemoteSide remoteside 
				(P2P.net P2P.getProtocolVersion)
		let closer = do
			closeRemoteSide remoteside
			p2pDone
		proxy closer proxyMethods servermode clientside
			(const $ return remoteside)
			protocolversion othermsg p2pErrHandler
	withclientversion _ Nothing = p2pDone

performProxyCluster :: UUID -> ClusterUUID -> P2P.ServerMode -> CommandPerform
performProxyCluster clientuuid clusteruuid servermode = do
	clientside <- proxyClientSide clientuuid
	getClientProtocolVersion (fromClusterUUID clusteruuid) clientside 
		(withclientversion clientside)
		p2pErrHandler
  where
	withclientversion clientside (Just (clientmaxversion, othermsg)) = do
		let protocolversion = min P2P.maxProtocolVersion clientmaxversion
		let selectnode = giveup "FIXME" -- FIXME
		proxy p2pDone proxyMethods servermode clientside selectnode
			protocolversion othermsg p2pErrHandler
	withclientversion _ Nothing = p2pDone

proxyMethods :: ProxyMethods
proxyMethods = ProxyMethods
	{ removedContent = \u k -> logChange k u InfoMissing
	, addedContent = \u k -> logChange k u InfoPresent
	}

proxyClientSide :: UUID -> Annex ClientSide
proxyClientSide clientuuid = do
	clientrunst <- liftIO (mkRunState $ Serving clientuuid Nothing)
	return $ ClientSide clientrunst (stdioP2PConnection Nothing)

-- FIXME: Support special remotes.
proxySshRemoteSide :: P2P.ProtocolVersion -> Remote -> Annex RemoteSide
proxySshRemoteSide clientmaxversion remote = mkRemoteSide (Remote.uuid remote) $
	openP2PShellConnection' remote clientmaxversion >>= \case
		Just conn@(P2P.IO.OpenConnection (remoterunst, remoteconn, _)) ->
			return $ Just 
				( remoterunst
				, remoteconn
				, void $ liftIO $ closeP2PShellConnection conn
				)
		_  -> return Nothing

p2pErrHandler :: (a -> CommandPerform) -> Annex (Either ProtoFailure a) -> CommandPerform
p2pErrHandler cont a = a >>= \case
	-- Avoid displaying an error when the client hung up on us.
	Left (ProtoFailureIOError e) | isEOFError e -> p2pDone
	Left e -> giveup (describeProtoFailure e)
	Right v -> cont v

p2pDone :: CommandPerform
p2pDone = next $ return True
