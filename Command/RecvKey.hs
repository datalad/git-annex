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
import qualified Types.Key
import qualified Types.Backend
import qualified Backend

cmd :: Command
cmd = noCommit $ command "recvkey" SectionPlumbing 
	"runs rsync in server mode to receive content"
	paramKey (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withKeys start

start :: Key -> CommandStart
start key = fieldTransfer Download key $ \_p ->
	ifM (getViaTmp key go)
		( do
			-- forcibly quit after receiving one key,
			-- and shutdown cleanly
			_ <- shutdown True
			return True
		, return False
		)
  where
	go tmp = do
		opts <- filterRsyncSafeOptions . maybe [] words
			<$> getField "RsyncOptions"
		ok <- liftIO $ rsyncServerReceive (map Param opts) tmp

		-- The file could have been received with permissions that
		-- do not allow reading it, so this is done before the
		-- directcheck.
		freezeContent tmp

		if ok
			then ifM (isJust <$> Fields.getField Fields.direct)
				( directcheck tmp
				, return True
				)
			else return False
	{- If the sending repository uses direct mode, the file
	 - it sends could be modified as it's sending it. So check
	 - that the right size file was received, and that the key/value
	 - Backend is happy with it. -}
	directcheck tmp = do
		oksize <- case Types.Key.keySize key of
		        Nothing -> return True
		        Just size -> do
				size' <- liftIO $ getFileSize tmp
				return $ size == size'
		if oksize
			then case Backend.maybeLookupBackendName (Types.Key.keyBackendName key) of
				Nothing -> do
					warning "recvkey: received key from direct mode repository using unknown backend; cannot check; discarding"
					return False
				Just backend -> maybe (return True) runverify
					(Types.Backend.verifyKeyContent backend)
			else do
				warning "recvkey: received key with wrong size; discarding"
				return False
	  where
		runverify check = ifM (check key tmp)
			( return True
			, do
				warning "recvkey: received key from direct mode repository seems to have changed as it was transferred; discarding"
				return False
			)
