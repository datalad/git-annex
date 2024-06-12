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
		Nothing -> performLocal theiruuid servermode
		Just r -> performProxy theiruuid servermode r

performLocal :: UUID -> P2P.ServerMode -> CommandPerform
performLocal theiruuid servermode = do
	myuuid <- getUUID
	let conn = stdioP2PConnection Nothing
	let server = do
		P2P.net $ P2P.sendMessage (P2P.AUTH_SUCCESS myuuid)
		P2P.serveAuthed servermode myuuid
	runst <- liftIO $ mkRunState $ Serving theiruuid Nothing
	runFullProto runst conn server >>= \case
		Right () -> done
		-- Avoid displaying an error when the client hung up on us.
		Left (ProtoFailureIOError e) | isEOFError e -> done
		Left e -> giveup (describeProtoFailure e)
  where
	done = next $ return True

performProxy :: UUID -> P2P.ServerMode -> Remote -> CommandPerform
performProxy clientuuid servermode remote = do
	clientside <- ClientSide
		<$> liftIO (mkRunState $ Serving clientuuid Nothing)
		<*> pure (stdioP2PConnection Nothing)
	getClientProtocolVersion remote clientside 
		(withclientversion clientside)
		protoerrhandler
  where
	withclientversion clientside (Just (clientmaxversion, othermsg)) =
		connectremote clientmaxversion $ \remoteside ->
			proxy done proxymethods servermode clientside remoteside 
				othermsg protoerrhandler
	withclientversion _ Nothing = done
	
	proxymethods = ProxyMethods
		{ removedContent = \u k -> logChange k u InfoMissing
		, addedContent = \u k -> logChange k u InfoPresent
		}

	-- FIXME: Support special remotes.
	connectremote clientmaxversion cont = 
		openP2PShellConnection' remote clientmaxversion >>= \case
			Just conn@(P2P.IO.OpenConnection (remoterunst, remoteconn, _)) ->
				cont (RemoteSide remoterunst remoteconn (Remote.uuid remote))
					`finally` liftIO (closeP2PShellConnection conn)
			_  -> giveup "Unable to connect to remote."

	protoerrhandler cont a = a >>= \case
		-- Avoid displaying an error when the client hung up on us.
		Left (ProtoFailureIOError e) | isEOFError e -> done
		Left e -> giveup (describeProtoFailure e)
		Right v -> cont v
	
	done = next $ return True
