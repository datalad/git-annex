{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.RecvKey where

import Common.Annex
import Command
import CmdLine
import Annex.Content
import Utility.Rsync
import Logs.Transfer
import Command.SendKey (fieldTransfer)
import qualified Fields
import qualified Types.Key
import qualified Types.Backend
import qualified Backend

def :: [Command]
def = [noCommit $ command "recvkey" paramKey seek
	"runs rsync in server mode to receive content"]

seek :: [CommandSeek]
seek = [withKeys start]

start :: Key -> CommandStart
start key = ifM (inAnnex key)
	( error "key is already present in annex"
	, fieldTransfer Download key $ \_p -> do
		ifM (getViaTmp key go)
			( do
				-- forcibly quit after receiving one key,
				-- and shutdown cleanly
				_ <- shutdown True
				return True
			, return False
			)
	)
  where
	go tmp = ifM (liftIO $ rsyncServerReceive tmp)
		( ifM (isJust <$> Fields.getField Fields.direct)
			( directcheck tmp
			, return True
			)
		, return False
		)
	{- If the sending repository uses direct mode, the file
	 - it sends could be modified as it's sending it. So check
	 - that the right size file was received, and that the key/value
	 - Backend is happy with it. -}
	directcheck tmp = do
		oksize <- case Types.Key.keySize key of
		        Nothing -> return True
		        Just size -> do
				size' <- fromIntegral . fileSize
       	        	        	<$> liftIO (getFileStatus tmp)
				return $ size == size'
		if oksize
			then case Backend.maybeLookupBackendName (Types.Key.keyBackendName key) of
				Nothing -> return False
				Just backend -> maybe (return True) (\a -> a key tmp)
					(Types.Backend.fsckKey backend)
			else return False
