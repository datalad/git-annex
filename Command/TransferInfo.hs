{- git-annex command
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.TransferInfo where

import Command
import Annex.Content
import Logs.Transfer
import qualified CmdLine.GitAnnexShell.Fields as Fields
import Utility.Metered

cmd :: Command
cmd = noCommit $ 
	command "transferinfo" SectionPlumbing
		"updates sender on number of bytes of content received"
		paramKey (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

{- Security:
 - 
 - The transfer info file contains the user-supplied key, but
 - the built-in guards prevent slashes in it from showing up in the filename.
 - It also contains the UUID of the remote. But slashes are also filtered
 - out of that when generating the filename.
 - 
 - Checks that the key being transferred is inAnnex, to prevent
 - malicious spamming of bogus keys. Does not check that a transfer
 - of the key is actually in progress, because this could be started
 - concurrently with sendkey, and win the race.
 -}
start :: [String] -> CommandStart
start (k:[]) = do
	case file2key k of
		Nothing -> error "bad key"
		(Just key) -> whenM (inAnnex key) $ do
			file <- Fields.getField Fields.associatedFile
			u <- maybe (error "missing remoteuuid") toUUID
				<$> Fields.getField Fields.remoteUUID
			let t = Transfer
				{ transferDirection = Upload
				, transferUUID = u
				, transferKey = key
				}
			tinfo <- liftIO $ startTransferInfo file
			(update, tfile, _) <- mkProgressUpdater t tinfo
			liftIO $ mapM_ void
				[ tryIO $ forever $ do
					bytes <- readUpdate
					maybe (error "transferinfo protocol error")
						(update . toBytesProcessed) bytes
				, tryIO $ removeFile tfile
				, exitSuccess
				]
	stop
start _ = error "wrong number of parameters"

readUpdate :: IO (Maybe Integer)
readUpdate = readish <$> getLine
