{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.RecvKey where

import Command
import Annex.Content
import Annex.Action
import Annex
import Utility.Rsync
import Types.Transfer
import Logs.Location
import Command.SendKey (fieldTransfer)

cmd :: Command
cmd = noCommit $ command "recvkey" SectionPlumbing 
	"runs rsync in server mode to receive content"
	paramKey (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withKeys (commandAction . start)

start :: (SeekInput, Key) -> CommandStart
start (_, key) = fieldTransfer Download key $ \_p -> do
	-- This matches the retrievalSecurityPolicy of Remote.Git
	let rsp = RetrievalAllKeysSecure
	ifM (getViaTmp rsp DefaultVerify key (AssociatedFile Nothing) Nothing go)
		( do
			logStatus NoLiveUpdate key InfoPresent
			_ <- quiesce True
			return True
		, return False
		)
  where
	go tmp = unVerified $ do
		opts <- filterRsyncSafeOptions . maybe [] words
			<$> getField "RsyncOptions"
		liftIO $ rsyncServerReceive (map Param opts) (fromOsPath tmp)
