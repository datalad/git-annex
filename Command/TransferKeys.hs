{- git-annex command
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.TransferKeys where

import Command
import qualified Annex
import Annex.Content
import Logs.Location
import Annex.Transfer
import qualified Remote
import Utility.SimpleProtocol (dupIoHandles)
import Git.Types (RemoteName)
import qualified Database.Keys
import Annex.BranchState
import Types.Messages
import Types.Key

import Text.Read (readMaybe)

data TransferRequest = TransferRequest Direction (Either UUID RemoteName) KeyData AssociatedFile
	deriving (Show, Read)

data TransferResponse
	= TransferOutput SerializedOutput
	| TransferResult Bool
	deriving (Show, Read)

cmd :: Command
cmd = command "transferkeys" SectionPlumbing "transfers keys"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing (commandAction start)

start :: CommandStart
start = do
	enableInteractiveBranchAccess
	(readh, writeh) <- liftIO dupIoHandles
	Annex.setOutput $ SerializedOutput $
		hPutStrLn writeh . show . TransferOutput
	runRequests readh writeh runner
	stop
  where
	runner (TransferRequest direction _ keydata file) remote
		| direction == Upload = notifyTransfer direction file $
			upload (Remote.uuid remote) key file stdRetry $ \p -> do
				tryNonAsync (Remote.storeKey remote key file p) >>= \case
					Left e -> do
						warning (show e)
						return False
					Right () -> do
						Remote.logStatus remote key InfoPresent
						return True
		| otherwise = notifyTransfer direction file $
			download (Remote.uuid remote) key file stdRetry $ \p ->
				getViaTmp (Remote.retrievalSecurityPolicy remote) (RemoteVerify remote) key file $ \t -> do
					r <- tryNonAsync (Remote.retrieveKeyFile remote key file (fromRawFilePath t) p) >>= \case
						Left e -> do
							warning (show e)
							return (False, UnVerified)
						Right v -> return (True, v)
					-- Make sure we get the current
					-- associated files data for the key,
					-- not old cached data.
					Database.Keys.closeDb			
					return r
	  where
		key = mkKey (const keydata)

runRequests
	:: Handle
	-> Handle
	-> (TransferRequest -> Remote -> Annex Bool)
	-> Annex ()
runRequests readh writeh a = go Nothing Nothing
  where
	go lastremoteoruuid lastremote = unlessM (liftIO $ hIsEOF readh) $ do
		l <- liftIO $ hGetLine readh
		case readMaybe l of
			Just tr@(TransferRequest _ remoteoruuid _ _) -> do
				-- Often the same remote will be used
				-- repeatedly, so cache the last one to
				-- avoid looking up repeatedly.
				mremote <- if lastremoteoruuid == Just remoteoruuid
					then pure lastremote
					else eitherToMaybe <$> Remote.byName'
						(either fromUUID id remoteoruuid)
				case mremote of
					Just remote -> do
						sendresult =<< a tr remote
						go (Just remoteoruuid) mremote
					Nothing -> protocolError l
			Nothing -> protocolError l

	sendresult b = liftIO $ do
		hPutStrLn writeh $ show $ TransferResult b
		hFlush writeh

-- FIXME this is bad when used with inAnnex
sendRequest :: Transfer -> TransferInfo -> Handle -> IO ()
sendRequest t tinfo h = hPutStrLn h $ show $ TransferRequest
	(transferDirection t)
	(maybe (Left (transferUUID t)) (Right . Remote.name) (transferRemote tinfo))
	(keyData (transferKey t))
	(associatedFile tinfo)

-- | Read a response from this command.
--
-- Before the final response, this will return whatever SerializedOutput
-- should be displayed as the transfer is performed.
readResponse :: Handle -> IO (Either SerializedOutput Bool)
readResponse h = do
	l <- liftIO $ hGetLine h
	case readMaybe l of
		Just (TransferOutput so) -> return (Left so)
		Just (TransferResult r) -> return (Right r)
		Nothing -> protocolError l

protocolError :: String -> a
protocolError l = error $ "transferkeys protocol error: " ++ show l
