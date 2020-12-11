{- git-annex command
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Transferrer where

import Command
import qualified Annex
import Annex.Content
import Logs.Location
import Annex.Transfer
import qualified Remote
import Utility.SimpleProtocol (dupIoHandles)
import qualified Database.Keys
import Annex.BranchState
import Types.Messages
import Annex.TransferrerPool
import Types.Transferrer
import qualified Utility.SimpleProtocol as Proto

cmd :: Command
cmd = command "transferrer" SectionPlumbing "transfers content"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing (commandAction start)

start :: CommandStart
start = do
	enableInteractiveBranchAccess
	(readh, writeh) <- liftIO dupIoHandles
	let outputwriter = sendTransferResponse writeh . TransferOutput
	let outputresponsereader = do
		l <- hGetLine readh
		return $ case Proto.parseMessage l of
			Just (TransferSerializedOutputResponse r) -> Just r
			Nothing -> Nothing
	Annex.setOutput $ SerializedOutput outputwriter outputresponsereader
	runRequests readh writeh runner
	stop
  where
	runner (UploadRequest _ key (TransferAssociatedFile file)) remote =
		-- This is called by eg, Annex.Transfer.upload,
		-- so caller is responsible for doing notification,
		-- and for retrying, and updating location log.
		upload' (Remote.uuid remote) key file noRetry
			(Remote.action . Remote.storeKey remote key file)
			noNotification
	runner (DownloadRequest _ key (TransferAssociatedFile file)) remote =
		-- This is called by eg, Annex.Transfer.download
		-- so caller is responsible for doing notification
		-- and for retrying, and updating location log.
		let go p = getViaTmp (Remote.retrievalSecurityPolicy remote) (RemoteVerify remote) key file $ \t -> do
			Remote.verifiedAction (Remote.retrieveKeyFile remote key file (fromRawFilePath t) p)
		in download' (Remote.uuid remote) key file noRetry go 
			noNotification
	runner (AssistantUploadRequest _ key (TransferAssociatedFile file)) remote =
		notifyTransfer Upload file $
			upload' (Remote.uuid remote) key file stdRetry $ \p -> do
				tryNonAsync (Remote.storeKey remote key file p) >>= \case
					Left e -> do
						warning (show e)
						return False
					Right () -> do
						Remote.logStatus remote key InfoPresent
						return True
	runner (AssistantDownloadRequest _ key (TransferAssociatedFile file)) remote =
		notifyTransfer Download file $
			download' (Remote.uuid remote) key file stdRetry $ \p ->
				logStatusAfter key $ getViaTmp (Remote.retrievalSecurityPolicy remote) (RemoteVerify remote) key file $ \t -> do
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

runRequests
	:: Handle
	-> Handle
	-> (TransferRequest -> Remote -> Annex Bool)
	-> Annex ()
runRequests readh writeh a = go Nothing Nothing
  where
	go lastremoteoruuid lastremote = unlessM (liftIO $ hIsEOF readh) $ do
		l <- liftIO $ hGetLine readh
		case Proto.parseMessage l of
			Just tr -> do
				let remoteoruuid = transferRequestRemote tr
				-- Often the same remote will be used
				-- repeatedly, so cache the last one to
				-- avoid looking up repeatedly.
				mremote <- if lastremoteoruuid == Just remoteoruuid
					then pure lastremote
					else case remoteoruuid of
						TransferRemoteName n ->
							eitherToMaybe <$> Remote.byName' n
						TransferRemoteUUID u -> 
							Remote.byUUID u
				case mremote of
					Just remote -> do
						sendresult =<< a tr remote
						go (Just remoteoruuid) mremote
					Nothing -> transferrerProtocolError l
			Nothing -> transferrerProtocolError l

	sendresult = liftIO . sendTransferResponse writeh . TransferResult

sendTransferResponse :: Handle -> TransferResponse -> IO ()
sendTransferResponse h r = do
	hPutStrLn h $ unwords $ Proto.formatMessage r
	hFlush h
