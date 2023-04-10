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
cmd = noCommit $ command "transferrer" SectionPlumbing "transfers content"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing (commandAction start)

start :: CommandStart
start = do
	enableInteractiveBranchAccess
	(readh, writeh) <- liftIO dupIoHandles
	let outputwriter = sendTransferResponse writeh . TransferOutput
	let outputresponsereader = do
		l <- getNextLine readh
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
		-- and for retrying, and updating location log,
		-- and stall canceling.
		upload' (Remote.uuid remote) key file Nothing noRetry
			(Remote.action . Remote.storeKey remote key file)
			noNotification
	runner (DownloadRequest _ key (TransferAssociatedFile file)) remote =
		-- This is called by eg, Annex.Transfer.download
		-- so caller is responsible for doing notification
		-- and for retrying, and updating location log,
		-- and stall canceling.
		let go p = getViaTmp (Remote.retrievalSecurityPolicy remote) (RemoteVerify remote) key file $ \t -> do
			Remote.verifiedAction (Remote.retrieveKeyFile remote key file (fromRawFilePath t) p (RemoteVerify remote))
		in download' (Remote.uuid remote) key file Nothing noRetry go 
			noNotification
	runner (AssistantUploadRequest _ key (TransferAssociatedFile file)) remote =
		notifyTransfer Upload file $
			upload' (Remote.uuid remote) key file Nothing stdRetry $ \p -> do
				tryNonAsync (Remote.storeKey remote key file p) >>= \case
					Left e -> do
						warning (UnquotedString (show e))
						return False
					Right () -> do
						Remote.logStatus remote key InfoPresent
						return True
	runner (AssistantDownloadRequest _ key (TransferAssociatedFile file)) remote =
		notifyTransfer Download file $
			download' (Remote.uuid remote) key file Nothing stdRetry $ \p ->
				logStatusAfter key $ getViaTmp (Remote.retrievalSecurityPolicy remote) (RemoteVerify remote) key file $ \t -> do
					r <- tryNonAsync (Remote.retrieveKeyFile remote key file (fromRawFilePath t) p (RemoteVerify remote)) >>= \case
						Left e -> do
							warning (UnquotedString (show e))
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
		l <- liftIO $ getNextLine readh
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
sendTransferResponse h r = silenceIOErrors $ do
	hPutStrLn h $ unwords $ Proto.formatMessage r
	hFlush h

getNextLine :: Handle -> IO String
getNextLine = silenceIOErrors . hGetLine

{- If the pipe we're talking to gets closed due to the parent git-annex
 - having exited, read/write would throw an exception due to sigpipe,
 - which gets displayed on the console in an ugly way. This silences that
 - display, and exits on exception instead.
 -
 - Normally signals like SIGINT get propagated to this process
 - from the parent process. However, since this process is run in its own
 - process group, that propagation requires the parent to actively
 - propagate the signal. One way that could not happen is if the parent
 - gets a signal it cannot catch. Another way is if the parent is hit by
 - the signal before it can set up the signal propagation.
 -}
silenceIOErrors :: IO a -> IO a
silenceIOErrors a = catchIO a (const exitFailure)
