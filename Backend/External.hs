{- git-annex external backend
 -
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances #-}

module Backend.External (makeBackend) where

import Annex.Common
import Annex.ExternalAddonProcess
import Types.Key
import Types.Backend
import Types.KeySource
import Utility.Metered
import qualified Utility.SimpleProtocol as Proto

import qualified Data.ByteString as S
import qualified Data.Map.Strict as M
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)

newtype ExternalBackendName = ExternalBackendName S.ByteString
	deriving (Show, Eq, Ord)

-- Makes Backend representing an external backend of any type.
-- If the program is not available or doesn't work, makes a Backend
-- that cannot generate or verify keys, but that still lets the keys be
-- basically used.
makeBackend :: S.ByteString -> HasExt -> Annex Backend
makeBackend bname hasext = withExternalState ebname $ \st -> do
	withExternalAddon st (pure unavailbackend) $ \_ext -> do
		canverify <- handleRequest st CANVERIFY (pure False) $ \case
			CANVERIFY_YES -> result True
			CANVERIFY_NO -> result False
			_ -> Nothing
		isstable <- handleRequest st ISSTABLE (pure False) $ \case
			ISSTABLE_YES -> result True
			ISSTABLE_NO -> result False
			_ -> Nothing
		iscryptographicallysecure <- handleRequest st ISCRYPTOGRAPHICALLYSECURE (pure False) $ \case
			ISCRYPTOGRAPHICALLYSECURE_YES -> result True
			ISCRYPTOGRAPHICALLYSECURE_NO -> result False
			_ -> Nothing
		return $ Backend
			{ backendVariety = ExternalKey bname hasext
			, genKey = Just $ genKeyExternal ebname
			, verifyKeyContent = if canverify
				then Just $ verifyKeyContentExternal ebname
				else Nothing
			, canUpgradeKey = Nothing
			, fastMigrate = Nothing
			, isStableKey = const isstable
			, isCryptographicallySecure = const iscryptographicallysecure
			}
  where
	ebname = ExternalBackendName bname
	unavailbackend = Backend
		{ backendVariety = ExternalKey bname hasext
		, genKey = Nothing
		, verifyKeyContent = Nothing
		, canUpgradeKey = Nothing
		, fastMigrate = Nothing
		, isStableKey = const False
		, isCryptographicallySecure = const False
		}

genKeyExternal :: ExternalBackendName -> KeySource -> MeterUpdate -> Annex Key
genKeyExternal bname ks p = error "TODO"

verifyKeyContentExternal :: ExternalBackendName -> Key -> FilePath -> Annex Bool
verifyKeyContentExternal bname k f = error "TODO"

-- State about a running external backend program.
data ExternalState = ExternalState
	{ externalAddonProcess :: Either ExternalAddonStartError ExternalAddonProcess
	}

handleRequest :: ExternalState -> Request -> Annex a -> ResponseHandler a -> Annex a
handleRequest st req whenunavail responsehandler =
	withExternalAddon st whenunavail $ \p -> do
		sendMessage p req
		receiveResponse p responsehandler (Just . handleAsyncMessage)
  where
	handleAsyncMessage (ERROR err) = do
		warning ("external special remote error: " ++ err)
		whenunavail

withExternalAddon :: ExternalState -> a -> (ExternalAddonProcess -> a) -> a
withExternalAddon st whenunavail a = case externalAddonProcess st of
	Right addon -> a addon
	Left _ -> whenunavail

sendMessage :: Proto.Sendable m => ExternalAddonProcess -> m -> Annex ()
sendMessage p m = liftIO $ do
	protocolDebug p True line
	hPutStrLn (externalSend p) line
	hFlush (externalSend p)
  where
	line = unwords $ Proto.formatMessage m

{- A response handler can yeild a result, or it can request that another
 - message be consumed from the external. -}
data ResponseHandlerResult a
	= Result a
	| GetNextMessage (ResponseHandler a)

type ResponseHandler a = Response -> Maybe (Annex (ResponseHandlerResult a))

result :: a -> Maybe (Annex (ResponseHandlerResult a))
result = Just . return . Result

{- Waits for a message from the external backend, and passes it to the
 - apppropriate handler. 
 -
 - If the handler returns Nothing, this is a protocol error.
 -}
receiveResponse
	:: ExternalAddonProcess
	-> ResponseHandler a
	-> (AsyncMessage -> Maybe (Annex a))
	-> Annex a
receiveResponse p handleresponse handleasync =
	go =<< liftIO (catchMaybeIO $ hGetLine $ externalReceive p)
  where
	go Nothing = protocolError False ""
	go (Just s) = do
		liftIO $ protocolDebug p False s
		case Proto.parseMessage s :: Maybe Response of
			Just resp -> case handleresponse resp of
				Nothing -> protocolError True s
				Just callback -> callback >>= \case
					Result a -> return a
					GetNextMessage handleresponse' ->
						receiveResponse p handleresponse' handleasync
			Nothing -> case Proto.parseMessage s :: Maybe AsyncMessage of
				Just msg -> maybe (protocolError True s) id (handleasync msg)
				Nothing -> protocolError False s

	protocolError parsed s = giveup $ "external backend protocol error, unexpectedly received \"" ++ s ++ "\" " ++
		if parsed
			then "(message not allowed at this time)"
			else "(unable to parse message)"

-- Information about pools of of running external backends that are
-- available to use is stored in this global.
{-# NOINLINE poolVar #-}
poolVar :: MVar (M.Map ExternalBackendName (ExternalAddonPID, [ExternalState]))
poolVar = unsafePerformIO $ newMVar M.empty

-- Starts a new instance of an external backend.
-- Does not add it to the poolVar; caller should add it once it's done
-- using it.
newExternalState :: ExternalBackendName -> ExternalAddonPID -> Annex ExternalState
newExternalState (ExternalBackendName name) pid = do
	st <- startExternalAddonProcess basecmd pid
	st' <- case st of
		Left (ProgramNotInstalled msg) -> warnonce msg >> return st
		Left (ProgramFailure msg) -> warnonce msg >> return st
		Right p -> do
			sendMessage p GETVERSION
			v <- receiveResponse p
				(\resp -> case resp of
					VERSION v -> result v
					_ -> Nothing
				)
				(const Nothing)
			if v `notElem` supportedProtocolVersions
				then do
					warnonce (basecmd ++ " uses an unsupported version of the external backend protocol")
					return $ Left (ProgramFailure "bad protocol version")
				else return (Right p)
	return $ ExternalState { externalAddonProcess = st' }
  where
	basecmd = "git-annex-backend-X" ++ decodeBS' name
	warnonce msg = when (pid == 1) $
		warning msg

-- Runs an action with an ExternalState, starting a new external backend
-- process if necessary. It is returned to the pool once the action
-- finishes successfully. On exception, it's shut down.
withExternalState :: ExternalBackendName -> (ExternalState -> Annex a) -> Annex a
withExternalState bname a = do
	st <- get
	r <- a st `onException` shutdown st
	put st -- only when no exception is thrown
	return r
  where
	get = do
		m <- liftIO $ takeMVar poolVar
		case fromMaybe (1, []) (M.lookup bname m) of
			(pid, []) -> do
				let m' = M.insert bname (succ pid, []) m
				liftIO $ putMVar poolVar m'
				newExternalState bname pid
			(pid, (st:rest)) -> do
				let m' = M.insert bname (pid, rest) m
				liftIO $ putMVar poolVar m'
				return st
	put st = liftIO $ modifyMVar_ poolVar $
		pure . M.adjust (\(pid, l) -> (pid, st:l)) bname
	shutdown st = liftIO $
		withExternalAddon st noop (flip externalShutdown False)

-- This is a key as seen by the protocol consumer. When the "E" variant
-- of the external backend is in use, it does not include an extension.
-- And it's assumed not to contain spaces or newlines, or anything besides
-- ascii alphanumerics, because the protocol does not allow keys containing
-- such things.
newtype ProtoKey = ProtoKey Key
	deriving (Show)

instance Proto.Serializable ProtoKey where
	serialize (ProtoKey k) = Proto.serialize k
	deserialize = fmap ProtoKey . Proto.deserialize

data Request
	= GETVERSION
	| CANVERIFY
	| ISSTABLE
	| ISCRYPTOGRAPHICALLYSECURE
	| GENKEY FilePath
	| VERIFYKEYCONTENT ProtoKey FilePath
	deriving (Show)

data Response
	= VERSION ProtocolVersion
	| CANVERIFY_YES
	| CANVERIFY_NO
	| ISSTABLE_YES
	| ISSTABLE_NO
	| ISCRYPTOGRAPHICALLYSECURE_YES
	| ISCRYPTOGRAPHICALLYSECURE_NO
	| GENKEY_SUCCESS ProtoKey
	| GENKEY_FAILURE ErrorMsg
	| VERIFYKEYCONTENT_SUCCESS
	| VERIFYKEYCONTENT_FAILURE
	| PROGRESS BytesProcessed
	| DEBUG String
	deriving (Show)

data AsyncMessage
	= ERROR ErrorMsg
	deriving (Show)

type ErrorMsg = String

newtype ProtocolVersion = ProtocolVersion Int
	deriving (Show, Eq)

supportedProtocolVersions :: [ProtocolVersion]
supportedProtocolVersions = [ProtocolVersion 1]

instance Proto.Serializable ProtocolVersion where
	serialize (ProtocolVersion n) = show n
	deserialize = ProtocolVersion <$$> readish

instance Proto.Sendable AsyncMessage where
	formatMessage (ERROR err) = [ "ERROR", Proto.serialize err ]

instance Proto.Receivable AsyncMessage where
	parseCommand "ERROR" = Proto.parse1 ERROR
	parseCommand _ = Proto.parseFail

instance Proto.Sendable Request where
	formatMessage GETVERSION = ["GETVERSION"]
	formatMessage CANVERIFY = ["CANVERIFY"]
	formatMessage ISSTABLE = ["ISSTABLE"]
	formatMessage ISCRYPTOGRAPHICALLYSECURE = ["ISCRYPTOGRAPHICALLYSECURE"]
	formatMessage (GENKEY file) = ["GENKEY", Proto.serialize file]
	formatMessage (VERIFYKEYCONTENT key file) =
		["VERIFYKEYCONTENT", Proto.serialize key, Proto.serialize file]

instance Proto.Receivable Response where
	parseCommand "VERSION" = Proto.parse1 VERSION
	parseCommand "CANVERIFY-YES" = Proto.parse0 CANVERIFY_YES
	parseCommand "CANVERIFY-NO" = Proto.parse0 CANVERIFY_NO
	parseCommand "ISSTABLE-YES" = Proto.parse0 ISSTABLE_YES
	parseCommand "ISSTABLE-NO" = Proto.parse0 ISSTABLE_NO
	parseCommand "ISCRYPTOGRAPHICALLYSECURE-YES" = Proto.parse0 ISCRYPTOGRAPHICALLYSECURE_YES
	parseCommand "ISCRYPTOGRAPHICALLYSECURE-NO" = Proto.parse0 ISCRYPTOGRAPHICALLYSECURE_NO
	parseCommand "GENKEY-SUCCESS" = Proto.parse1 GENKEY_SUCCESS
	parseCommand "GENKEY-FAILURE" = Proto.parse1 GENKEY_FAILURE
	parseCommand "VERIFYKEYCONTENT-SUCCESS" = Proto.parse0 VERIFYKEYCONTENT_SUCCESS
	parseCommand "VERIFYKEYCONTENT-FAILURE" = Proto.parse0 VERIFYKEYCONTENT_FAILURE
	parseCommand "PROGRESS" = Proto.parse1 PROGRESS
	parseCommand "DEBUG" = Proto.parse1 DEBUG
	parseCommand _ = Proto.parseFail
