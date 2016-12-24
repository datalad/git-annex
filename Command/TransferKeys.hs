{- git-annex command, used internally by assistant
 -
 - Copyright 2012, 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
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

data TransferRequest = TransferRequest Direction Remote Key AssociatedFile

cmd :: Command
cmd = command "transferkeys" SectionPlumbing "transfers keys"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing start

start :: CommandStart
start = do
	(readh, writeh) <- liftIO dupIoHandles
	runRequests readh writeh runner
	stop
  where
	runner (TransferRequest direction remote key file)
		| direction == Upload = notifyTransfer direction file $
			upload (Remote.uuid remote) key file forwardRetry $ \p -> do
				ok <- Remote.storeKey remote key file p
				when ok $
					Remote.logStatus remote key InfoPresent
				return ok
		| otherwise = notifyTransfer direction file $
			download (Remote.uuid remote) key file forwardRetry $ \p ->
				getViaTmp (RemoteVerify remote) key $ \t -> do
					r <- Remote.retrieveKeyFile remote key file t p
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
	go v = error $ "transferkeys protocol error: " ++ show v

	readrequests = liftIO $ split fieldSep <$> hGetContents readh
	sendresult b = liftIO $ do
		hPutStrLn writeh $ serialize b
		hFlush writeh

sendRequest :: Transfer -> TransferInfo -> Handle -> IO ()
sendRequest t tinfo h = do
	hPutStr h $ intercalate fieldSep
		[ serialize (transferDirection t)
		, maybe (serialize (fromUUID (transferUUID t)))
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
	serialize (Just f) = f
	serialize Nothing = ""
	deserialize "" = Just Nothing
	deserialize f = Just $ Just f

instance TCSerialized RemoteName where
	serialize n = n
	deserialize n = Just n

instance TCSerialized Key where
	serialize = key2file
	deserialize = file2key
