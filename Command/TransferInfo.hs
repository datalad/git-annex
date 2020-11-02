{- git-annex command
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.TransferInfo where

import Command
import Annex.Content
import Types.Transfer
import Logs.Transfer
import Utility.Metered
import Utility.SimpleProtocol
import qualified CmdLine.GitAnnexShell.Fields as Fields
import qualified Utility.RawFilePath as R

cmd :: Command
cmd = noCommit $ 
	command "transferinfo" SectionPlumbing
		"updates sender on number of bytes of content received"
		paramKey (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

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
	case deserializeKey k of
		Nothing -> error "bad key"
		(Just key) -> whenM (inAnnex key) $ do
			afile <- AssociatedFile . (fmap toRawFilePath)
				<$> Fields.getField Fields.associatedFile
			u <- maybe (error "missing remoteuuid") toUUID
				<$> Fields.getField Fields.remoteUUID
			let t = Transfer
				{ transferDirection = Upload
				, transferUUID = u
				, transferKeyData = fromKey id key
				}
			tinfo <- liftIO $ startTransferInfo afile
			(update, tfile, createtfile, _) <- mkProgressUpdater t tinfo
			createtfile
			liftIO $ mapM_ void
				[ tryIO $ forever $ do
					bytes <- readUpdate
					maybe (error "transferinfo protocol error")
						(update . toBytesProcessed) bytes
				, tryIO $ R.removeLink tfile
				, exitSuccess
				]
	stop
start _ = giveup "wrong number of parameters"

readUpdate :: IO (Maybe Integer)
readUpdate = maybe Nothing readish <$> getProtocolLine stdin
