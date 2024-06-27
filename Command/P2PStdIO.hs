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
import Annex.Proxy
import Annex.UUID
import qualified CmdLine.GitAnnexShell.Checks as Checks
import Logs.Location
import Logs.Cluster
import Annex.Cluster
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
performProxy clientuuid servermode r = do
	clientside <- proxyClientSide clientuuid
	getClientProtocolVersion (Remote.uuid r) clientside 
		(withclientversion clientside)
		p2pErrHandler
  where
	withclientversion clientside (Just (clientmaxversion, othermsg)) = do
		remoteside <- proxySshRemoteSide clientmaxversion mempty r
		protocolversion <- either (const (min P2P.maxProtocolVersion clientmaxversion)) id
			<$> runRemoteSide remoteside 
				(P2P.net P2P.getProtocolVersion)
		let closer = do
			closeRemoteSide remoteside
			p2pDone
		concurrencyconfig <- noConcurrencyConfig
		let runproxy othermsg' = proxy closer proxymethods
			servermode clientside
			(Remote.uuid r)
			(singleProxySelector remoteside)
			concurrencyconfig
			protocolversion othermsg' p2pErrHandler
		sendClientProtocolVersion clientside othermsg protocolversion
			runproxy p2pErrHandler
	withclientversion _ Nothing = p2pDone
	
	proxymethods = ProxyMethods
		{ removedContent = \u k -> logChange k u InfoMissing
		, addedContent = \u k -> logChange k u InfoPresent
		}

performProxyCluster :: UUID -> ClusterUUID -> P2P.ServerMode -> CommandPerform
performProxyCluster clientuuid clusteruuid servermode = do
	clientside <- proxyClientSide clientuuid
	proxyCluster clusteruuid p2pDone servermode clientside p2pErrHandler

proxyClientSide :: UUID -> Annex ClientSide
proxyClientSide clientuuid = do
	clientrunst <- liftIO (mkRunState $ Serving clientuuid Nothing)
	return $ ClientSide clientrunst (stdioP2PConnection Nothing)

p2pErrHandler :: (a -> CommandPerform) -> Annex (Either ProtoFailure a) -> CommandPerform
p2pErrHandler cont a = a >>= \case
	-- Avoid displaying an error when the client hung up on us.
	Left (ProtoFailureIOError e) | isEOFError e -> p2pDone
	Left e -> giveup (describeProtoFailure e)
	Right v -> cont v

p2pDone :: CommandPerform
p2pDone = next $ return True
