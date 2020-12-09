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

import Text.Read (readMaybe)

cmd :: Command
cmd = command "transferrer" SectionPlumbing "transfers content"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing (commandAction start)

start :: CommandStart
start = do
	enableInteractiveBranchAccess
	(readh, writeh) <- liftIO dupIoHandles
	Annex.setOutput $ SerializedOutput
		(\v -> hPutStrLn writeh (show (TransferOutput v)) >> hFlush writeh)
		(readMaybe <$> hGetLine readh)
	runRequests readh writeh runner
	stop
  where
	runner (TransferRequest AnnexLevel direction _ keydata file) remote
		| direction == Upload =
			-- This is called by eg, Annex.Transfer.upload,
			-- so caller is responsible for doing notification,
			-- and for retrying.
			upload' (Remote.uuid remote) key file noRetry
				(Remote.action . Remote.storeKey remote key file)
				noNotification
		| otherwise =
			-- This is called by eg, Annex.Transfer.download
			-- so caller is responsible for doing notification
			-- and for retrying.
			let go p = getViaTmp (Remote.retrievalSecurityPolicy remote) (RemoteVerify remote) key file $ \t -> do
				Remote.verifiedAction (Remote.retrieveKeyFile remote key file (fromRawFilePath t) p)
			in download' (Remote.uuid remote) key file noRetry go 
				noNotification
	  where
		key = mkKey (const keydata)
	runner (TransferRequest AssistantLevel direction _ keydata file) remote
		| direction == Upload = notifyTransfer direction file $
			upload' (Remote.uuid remote) key file stdRetry $ \p -> do
				tryNonAsync (Remote.storeKey remote key file p) >>= \case
					Left e -> do
						warning (show e)
						return False
					Right () -> do
						Remote.logStatus remote key InfoPresent
						return True
		| otherwise = notifyTransfer direction file $
			download' (Remote.uuid remote) key file stdRetry $ \p ->
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
			Just tr@(TransferRequest _ _ remoteoruuid _ _) -> do
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
					Nothing -> transferrerProtocolError l
			Nothing -> transferrerProtocolError l

	sendresult b = liftIO $ do
		hPutStrLn writeh $ show $ TransferResult b
		hFlush writeh
