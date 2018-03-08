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
import qualified CmdLine.GitAnnexShell.Fields as Fields

cmd :: Command
cmd = noMessages $ command "p2pstdio" SectionPlumbing
	"communicate in P2P protocol over stdio"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing start

start :: CommandStart
start = do
	servermode <- liftIO $ 
		Checks.checkEnvSet Checks.readOnlyEnv >>= return . \case
			True -> P2P.ServeReadOnly
			False -> P2P.ServeReadWrite
	theiruuid <- Fields.getField Fields.remoteUUID >>= \case
		Nothing -> giveup "missing remoteuuid field"
		Just u -> return (toUUID u)
	myuuid <- getUUID
	conn <- stdioP2PConnection <$> Annex.gitRepo
	let server = do
		P2P.net $ P2P.sendMessage (P2P.AUTH_SUCCESS myuuid)
		P2P.serveAuthed servermode myuuid
	runFullProto (Serving theiruuid Nothing) conn server >>= \case
		Right () -> next $ next $ return True
		Left e -> giveup e
