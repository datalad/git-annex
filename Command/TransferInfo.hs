{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.TransferInfo where

import Common.Annex
import Command
import Annex.Content
import Logs.Transfer
import Types.Remote
import Types.Key

def :: [Command]
def = [noCommit $ command "transferinfo" paramdesc seek
	"updates sender on number of bytes of content received"]

seek :: [CommandSeek]
seek = [withWords start]

paramdesc :: String
paramdesc = paramKey `paramPair` paramUUID `paramPair` paramOptional paramFile

start :: [String] -> CommandStart
start (k:u:f:[]) = start' (file2key k) (toUUID u) (Just f) >> stop
start (k:u:[]) = start' (file2key k) (toUUID u) Nothing >> stop
start _ = error "wrong number of parameters"

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
start' :: Maybe Key -> UUID -> AssociatedFile -> Annex ()
start' Nothing _ _ = error "bad key"
start' (Just key) u file = whenM (inAnnex key) $ do
	let t = Transfer
		{ transferDirection = Upload
		, transferUUID = u
		, transferKey = key
		}
	info <- liftIO $ startTransferInfo file
	(update, tfile) <- mkProgressUpdater t info
	liftIO $ mapM_ void
		[ tryIO $ forever $ do
			bytes <- readish <$> getLine
			maybe (error "transferinfo protocol error") update bytes
		, tryIO $ removeFile tfile
		, exitSuccess
		]
