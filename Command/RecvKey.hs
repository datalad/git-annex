{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.RecvKey where

import Common.Annex
import Command
import Annex.Content
import Annex.Action
import Annex
import Utility.Rsync
import Logs.Transfer
import Command.SendKey (fieldTransfer)
import qualified CmdLine.GitAnnexShell.Fields as Fields

cmd :: Command
cmd = noCommit $ command "recvkey" SectionPlumbing 
	"runs rsync in server mode to receive content"
	paramKey (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withKeys start

start :: Key -> CommandStart
start key = fieldTransfer Download key $ \_p -> do
	-- Always verify content when a direct mode repo is sending a file,
	-- as the file could change while being transferred.
	fromdirect <- isJust <$> Fields.getField Fields.direct
	let verify = if fromdirect then AlwaysVerify else DefaultVerify
	ifM (getViaTmp verify key go)
		( do
			-- forcibly quit after receiving one key,
			-- and shutdown cleanly
			_ <- shutdown True
			return True
		, return False
		)
  where
	go tmp = unVerified $ do
		opts <- filterRsyncSafeOptions . maybe [] words
			<$> getField "RsyncOptions"
		liftIO $ rsyncServerReceive (map Param opts) tmp
