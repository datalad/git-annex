{- git-annex command, used internally by assistant in version
 - 8.20201127 and older and provided only to avoid upgrade breakage.
 - Remove at some point when such old versions of git-annex are unlikely
 - to be running any longer.
 -
 - Copyright 2012, 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Command.TransferKeys where

import Command
import Annex.Content
import Logs.Location
import Annex.Transfer
import qualified Remote
import Utility.SimpleProtocol (dupIoHandles)
import Git.Types (RemoteName)
import qualified Database.Keys
import Annex.BranchState

data TransferRequest = TransferRequest Direction Remote Key AssociatedFile

cmd :: Command
cmd = command "transferkeys" SectionPlumbing "transfers keys (deprecated)"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing (commandAction start)

start :: CommandStart
start = do
	enableInteractiveBranchAccess
	(readh, writeh) <- liftIO dupIoHandles
	runRequests readh writeh runner
	stop
  where
	runner (TransferRequest direction remote key af)
		| direction == Upload = notifyTransfer direction af $
			upload' (Remote.uuid remote) key af Nothing stdRetry $ \p -> do
				tryNonAsync (Remote.storeKey remote key af Nothing p) >>= \case
					Left e -> do
						warning (UnquotedString (show e))
						return False
					Right () -> do
						Remote.logStatus NoLiveUpdate remote key InfoPresent
						return True
		| otherwise = notifyTransfer direction af $
			download' (Remote.uuid remote) key af Nothing stdRetry $ \p ->
				logStatusAfter NoLiveUpdate key $ getViaTmp (Remote.retrievalSecurityPolicy remote) (RemoteVerify remote) key af Nothing $ \t -> do
					r <- tryNonAsync (Remote.retrieveKeyFile remote key af t p (RemoteVerify remote)) >>= \case
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
	-> (TransferRequest -> Annex Bool)
	-> Annex ()
runRequests readh writeh a = do
	liftIO $ hSetBuffering readh NoBuffering
	go =<< readrequests
  where
	go (d:rn:k:f:rest) = do
		case (deserialize d, deserialize rn, deserialize k, deserialize f) of
			(Just direction, Just remotename, Just key, Just file) -> do
				mremote <- Remote.byName' remotename
				case mremote of
					Left _ -> sendresult False
					Right remote -> sendresult =<< a
						(TransferRequest direction remote key file)
			_ -> sendresult False
		go rest
	go [] = noop
	go [""] = noop
	go v = giveup $ "transferkeys protocol error: " ++ show v

	readrequests = liftIO $ split fieldSep <$> hGetContents readh
	sendresult b = liftIO $ do
		hPutStrLn writeh $ serialize b
		hFlush writeh

sendRequest :: Transfer -> TransferInfo -> Handle -> IO ()
sendRequest t tinfo h = do
	hPutStr h $ intercalate fieldSep
		[ serialize (transferDirection t)
		, maybe (serialize ((fromUUID (transferUUID t)) :: String))
			(serialize . Remote.name)
			(transferRemote tinfo)
		, serialize (transferKey t)
		, serialize (associatedFile tinfo)
		, "" -- adds a trailing null
		]
	hFlush h

readResponse :: Handle -> IO Bool
readResponse h = fromMaybe False . deserialize <$> hGetLine h

fieldSep :: String
fieldSep = "\0"

class TCSerialized a where
	serialize :: a -> String
	deserialize :: String -> Maybe a

instance TCSerialized Bool where
	serialize True = "1"
	serialize False = "0"
	deserialize "1" = Just True
	deserialize "0" = Just False
	deserialize _ = Nothing

instance TCSerialized Direction where
	serialize Upload = "u"
	serialize Download = "d"
	deserialize "u" = Just Upload
	deserialize "d" = Just Download
	deserialize _ = Nothing

instance TCSerialized AssociatedFile where
	serialize (AssociatedFile (Just f)) = fromOsPath f
	serialize (AssociatedFile Nothing) = ""
	deserialize "" = Just (AssociatedFile Nothing)
	deserialize f = Just (AssociatedFile (Just (toOsPath f)))

instance TCSerialized RemoteName where
	serialize n = n
	deserialize n = Just n

instance TCSerialized Key where
	serialize = serializeKey
	deserialize = deserializeKey
