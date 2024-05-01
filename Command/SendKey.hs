{- git-annex command
 -
 - Copyright 2010,2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.SendKey where

import Command
import Annex.Content
import Annex
import Utility.Rsync
import Annex.Transfer
import qualified CmdLine.GitAnnexShell.Fields as Fields
import Utility.Metered

cmd :: Command
cmd = noCommit $ 
	command "sendkey" SectionPlumbing 
		"runs rsync in server mode to send content"
		paramKey (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withKeys (commandAction . start)

start :: (SeekInput, Key) -> CommandStart
start (_, key) = do
	opts <- filterRsyncSafeOptions . maybe [] words
		<$> getField "RsyncOptions"
	ifM (inAnnex key)
		( fieldTransfer Upload key $ \_p ->
			sendAnnex key rollback $ \f _sz -> 
				liftIO $ rsyncServerSend (map Param opts) f
		, do
			warning "requested key is not present"
			liftIO exitFailure
		)
  where
	{- No need to do any rollback; when sendAnnex fails, a nonzero
	 - exit will be propagated, and the remote will know the transfer
	 - failed. -}
	rollback = noop

fieldTransfer :: Direction -> Key -> (MeterUpdate -> Annex Bool) -> CommandStart
fieldTransfer direction key a = do
	fastDebug "Command.SendKey" "transfer start"
	let afile = AssociatedFile Nothing
	ok <- maybe (a $ const noop)
		-- Using noRetry here because we're the sender.
		(\u -> runner (Transfer direction (toUUID u) (fromKey id key)) Nothing afile Nothing noRetry a)
		=<< Fields.getField Fields.remoteUUID
	fastDebug "Command.SendKey" "transfer done"
	liftIO $ exitBool ok
  where
	{- Allow the key to be sent to the remote even if there seems to be
	 - another transfer of that key going on to that remote.
	 - That one may be stale, etc.
	 -}
	runner
		| direction == Upload = alwaysRunTransfer
		| otherwise = runTransfer
