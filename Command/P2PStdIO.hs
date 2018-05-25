{- git-annex command
 -
 - Copyright 2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.P2PStdIO where

import Command
import P2P.IO
import P2P.Annex
import qualified P2P.Protocol as P2P
import qualified Annex
import Annex.UUID
import qualified CmdLine.GitAnnexShell.Checks as Checks

cmd :: Command
cmd = noMessages $ command "p2pstdio" SectionPlumbing
	"communicate in P2P protocol over stdio"
	paramUUID (withParams seek)

seek :: CmdParams -> CommandSeek
seek [u] = commandAction $ start $ toUUID u
seek _ = giveup "missing UUID parameter"

start :: UUID -> CommandStart
start theiruuid = do
	servermode <- liftIO $ do
		ro <- Checks.checkEnvSet Checks.readOnlyEnv
		ao <- Checks.checkEnvSet Checks.appendOnlyEnv
		return $ case (ro, ao) of
			(True, _) -> P2P.ServeReadOnly
			(False, True) -> P2P.ServeAppendOnly
			(False, False) -> P2P.ServeReadWrite
	myuuid <- getUUID
	conn <- stdioP2PConnection <$> Annex.gitRepo
	let server = do
		P2P.net $ P2P.sendMessage (P2P.AUTH_SUCCESS myuuid)
		P2P.serveAuthed servermode myuuid
	runst <- liftIO $ mkRunState $ Serving theiruuid Nothing
	runFullProto runst conn server >>= \case
		Right () -> next $ next $ return True
		Left e -> giveup e
