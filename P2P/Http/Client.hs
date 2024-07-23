{- P2P protocol over HTTP, client
 -
 - https://git-annex.branchable.com/design/p2p_protocol_over_http/
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds, TypeApplications #-}
{-# LANGUAGE CPP #-}

module P2P.Http.Client where

import Types
import Annex.Url

#ifdef WITH_SERVANT
import qualified Annex
import Annex.UUID
import Types.Remote
import P2P.Http
import P2P.Http.Url
import Annex.Common
import P2P.Protocol hiding (Offset, Bypass, auth)
import Annex.Concurrent
import Utility.Url (BasicAuth(..))
import qualified Git.Credential as Git

import Servant hiding (BasicAuthData(..))
import Servant.Client.Streaming
import qualified Servant.Types.SourceT as S
import Network.HTTP.Types.Status
import Network.HTTP.Client
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI
import qualified Data.Map as M
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent
import System.IO.Unsafe
#endif

type ClientAction a
	= ClientEnv
	-> ProtocolVersion
	-> B64UUID ServerSide
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Maybe Auth
	-> Annex (Either ClientError a)

p2pHttpClient
	:: Remote
	-> (String -> Annex a)
#ifdef WITH_SERVANT
	-> ClientAction a
#endif
	-> Annex a
#ifdef WITH_SERVANT
p2pHttpClient rmt fallback clientaction =
	case p2pHttpBaseUrl <$> remoteAnnexP2PHttpUrl (gitconfig rmt) of
		Nothing -> error "internal"
		Just baseurl -> do
			mgr <- httpManager <$> getUrlOptions
			let clientenv = mkClientEnv mgr baseurl
			ccv <- Annex.getRead Annex.gitcredentialcache
			Git.CredentialCache cc <- liftIO $ atomically $
				readTMVar ccv
			case M.lookup (Git.CredentialBaseURL credentialbaseurl) cc of
				Nothing -> go clientenv Nothing False Nothing allProtocolVersions
				Just cred -> go clientenv (Just cred) True (credauth cred) allProtocolVersions
  where
	go clientenv mcred credcached mauth (v:vs) = do
		myuuid <- getUUID
		res <- clientaction clientenv v
			(B64UUID (uuid rmt))
			(B64UUID myuuid)
			[]
			mauth
		case res of
			Right resp -> do
				unless credcached $ cachecred mcred
				return resp
			Left (FailureResponse _ resp)
				| statusCode (responseStatusCode resp) == 404 && not (null vs) ->
					go clientenv mcred credcached mauth vs
				| statusCode (responseStatusCode resp) == 401 ->
					case mcred of
						Nothing -> authrequired clientenv (v:vs)
						Just cred -> do
							inRepo $ Git.rejectUrlCredential cred
							fallback (showstatuscode resp)
				| otherwise -> fallback (showstatuscode resp)
			Left (ConnectionError ex) -> case fromException ex of
				Just (HttpExceptionRequest _ (ConnectionFailure err)) -> fallback $
					"unable to connect to HTTP server: " ++ show err
				_ -> fallback (show ex)
			Left clienterror -> fallback $
					"git-annex HTTP API server returned an unexpected response: " ++ show clienterror
	go _ _ _ _ [] = error "internal"

	authrequired clientenv vs = do
		cred <- prompt $ 
			inRepo $ Git.getUrlCredential credentialbaseurl
		go clientenv (Just cred) False (credauth cred) vs

	showstatuscode resp = 
		show (statusCode (responseStatusCode resp))
			++ " " ++
		decodeBS (statusMessage (responseStatusCode resp))

	credentialbaseurl = case p2pHttpUrlString <$> remoteAnnexP2PHttpUrl (gitconfig rmt) of
		Nothing -> error "internal"
		Just url -> p2pHttpUrlWithoutUUID url

	credauth cred = do
		ba <- Git.credentialBasicAuth cred
		return $ Auth
			(encodeBS (basicAuthUser ba))
			(encodeBS (basicAuthPassword ba))
				
	cachecred mcred = case mcred of
		Just cred -> do
			inRepo $ Git.approveUrlCredential cred
			ccv <- Annex.getRead Annex.gitcredentialcache
			liftIO $ atomically $ do
				Git.CredentialCache cc <- takeTMVar ccv
				putTMVar ccv $ Git.CredentialCache $
					M.insert (Git.CredentialBaseURL credentialbaseurl) cred cc
		Nothing -> noop
#else
runP2PHttpClient rmt fallback = fallback "This remote uses an annex+http url, but this version of git-annex is not build with support for that."
#endif

#ifdef WITH_SERVANT

clientGet
	:: ClientEnv
	-> ProtocolVersion
	-> B64Key
	-> B64UUID ServerSide
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Maybe B64FilePath
	-> Maybe Auth
	-> RawFilePath
	-> IO Validity
clientGet clientenv (ProtocolVersion ver) k su cu bypass af auth dest = do
	startsz <- tryWhenExists $ getFileSize dest
	let mo = fmap (Offset . fromIntegral) startsz
	withClientM (cli k cu bypass af mo auth) clientenv $ \case
		Left err -> throwM err
		Right respheaders -> do
			b <- S.unSourceT (getResponse respheaders) gatherByteString
			liftIO $ withBinaryFile (fromRawFilePath dest) WriteMode $ \h -> do
				case startsz of
					Just startsz' | startsz' /= 0 ->
						hSeek h AbsoluteSeek startsz'
					_ -> noop
				len <- go 0 h (L.toChunks b)
				let DataLength dl = case lookupResponseHeader @DataLengthHeader' respheaders of
					Header hdr -> hdr
					_ -> error "missing data length header"
				if dl == len
					then return Valid
					else return Invalid
  where
	go n _ [] = return n
	go n h (b:bs) = do
		let !n' = n + fromIntegral (B.length b)
		B.hPut h b
		go n' h bs

	cli =case ver of
		3 -> v3 su V3
		2 -> v2 su V2
		1 -> v1 su V1
		0 -> v0 su V0
		_ -> error "unsupported protocol version"
	
	v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI

gatherByteString :: S.StepT IO B.ByteString -> IO L.ByteString
gatherByteString = unsafeInterleaveIO . go
  where
	go S.Stop = return LI.Empty
	go (S.Error err) = giveup err
	go (S.Skip s) = go s
	go (S.Effect ms) = ms >>= go
	go (S.Yield v s) = LI.Chunk v <$> unsafeInterleaveIO (go s)

clientCheckPresent :: Key -> ClientAction Bool
clientCheckPresent key clientenv (ProtocolVersion ver) su cu bypass auth =
	liftIO $ withClientM (cli su (B64Key key) cu bypass auth) clientenv $ \case
		Left err -> return (Left err)
		Right (CheckPresentResult res) -> return (Right res)
  where
	cli = case ver of
		3 -> flip v3 V3
		2 -> flip v2 V2
		1 -> flip v1 V1
		0 -> flip v0 V0
		_ -> error "unsupported protocol version"
	
	_ :<|> _ :<|> _ :<|> _ :<|>
		v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI

clientRemove
	:: ClientEnv
	-> ProtocolVersion
	-> B64Key
	-> B64UUID ServerSide
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Maybe Auth
	-> IO RemoveResultPlus
clientRemove clientenv (ProtocolVersion ver) key su cu bypass auth =
	withClientM cli clientenv $ \case
		Left err -> throwM err
		Right res -> return res
  where
	cli = case ver of
		3 -> v3 su V3 key cu bypass auth
		2 -> v2 su V2 key cu bypass auth
		1 -> plus <$> v1 su V1 key cu bypass auth
		0 -> plus <$> v0 su V0 key cu bypass auth
		_ -> error "unsupported protocol version"
	
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI

clientRemoveBefore
	:: ClientEnv
	-> ProtocolVersion
	-> B64Key
	-> B64UUID ServerSide
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Timestamp
	-> Maybe Auth
	-> IO RemoveResultPlus
clientRemoveBefore clientenv (ProtocolVersion ver) key su cu bypass ts auth =
	withClientM (cli su key cu bypass ts auth) clientenv $ \case
		Left err -> throwM err
		Right res -> return res
  where
	cli = case ver of
		3 -> flip v3 V3
		_ -> error "unsupported protocol version"
	
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		v3 :<|> _ = client p2pHttpAPI

clientGetTimestamp
	:: ClientEnv
	-> ProtocolVersion
	-> B64UUID ServerSide
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Maybe Auth
	-> IO GetTimestampResult
clientGetTimestamp clientenv (ProtocolVersion ver) su cu bypass auth = 
	withClientM (cli su cu bypass auth) clientenv $ \case
		Left err -> throwM err
		Right res -> return res
  where
	cli = case ver of
		3 -> flip v3 V3
		_ -> error "unsupported protocol version"
	
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|>
		v3 :<|> _ = client p2pHttpAPI

clientPut
	:: ClientEnv
	-> ProtocolVersion
	-> B64Key
	-> B64UUID ServerSide
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Maybe Auth
	-> Maybe Offset
	-> AssociatedFile
	-> FilePath
	-> FileSize
	-> Annex Bool
	-> Annex PutResultPlus
clientPut clientenv (ProtocolVersion ver) k su cu bypass auth moffset af contentfile contentfilesize validitycheck = do
	checkv <- liftIO newEmptyTMVarIO
	checkresultv <- liftIO newEmptyTMVarIO
	let checker = do
		liftIO $ atomically $ takeTMVar checkv
		validitycheck >>= liftIO . atomically . putTMVar checkresultv
	checkerthread <- liftIO . async =<< forkState checker
	v <- liftIO $ withBinaryFile contentfile ReadMode $ \h -> do
		when (offset /= 0) $
			hSeek h AbsoluteSeek offset
		withClientM (cli (stream h checkv checkresultv)) clientenv return
	case v of
		Left err -> do
			void $ liftIO $ atomically $ tryPutTMVar checkv ()
			join $ liftIO (wait checkerthread)
			throwM err
		Right res -> do
			join $ liftIO (wait checkerthread)
			return res
  where
	stream h checkv checkresultv = S.SourceT $ \a -> do
		bl <- L.hGetContents h
		v <- newMVar (0, filter (not . B.null) (L.toChunks bl))
		a (go v)
	  where
		go v = S.fromActionStep B.null $ modifyMVar v $ \case
			(n, (b:[])) -> do
				let !n' = n + B.length b
				ifM (checkvalid n')
					( return ((n', []), b)
					-- The key's content is invalid, but
					-- the amount of data is the same as
					-- the DataLengthHeader indicates.
					-- Truncate the stream by one byte to
					-- indicate to the server that it's
					-- not valid.
					, return 
						( (n' - 1, [])
						, B.take (B.length b - 1) b
						)
					)
			(n, []) -> do
				void $ checkvalid n
				return ((n, []), mempty)
			(n, (b:bs)) ->
				let !n' = n + B.length b
				in return ((n', bs), b)

		checkvalid n = do
			void $ liftIO $ atomically $ tryPutTMVar checkv ()
			valid <- liftIO $ atomically $ readTMVar checkresultv
			if not valid
				then return (n /= fromIntegral nlen)
				else return True

	baf = case af of
		AssociatedFile Nothing -> Nothing
		AssociatedFile (Just f) -> Just (B64FilePath f)

	len = DataLength nlen

	nlen = contentfilesize - offset

	offset = case moffset of
		Nothing -> 0
		Just (Offset o) -> fromIntegral o

	cli src = case ver of
		3 -> v3 su V3 len k cu bypass baf moffset src auth
		2 -> v2 su V2 len k cu bypass baf moffset src auth
		1 -> plus <$> v1 su V1 len k cu bypass baf moffset src auth
		0 -> plus <$> v0 su V0 len k cu bypass baf moffset src auth
		_ -> error "unsupported protocol version"

	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|>
		_ :<|>
		v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI

clientPutOffset
	:: ClientEnv
	-> ProtocolVersion
	-> B64Key
	-> B64UUID ServerSide
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Maybe Auth
	-> IO PutOffsetResultPlus
clientPutOffset clientenv (ProtocolVersion ver) k su cu bypass auth
	| ver == 0 = return (PutOffsetResultPlus (Offset 0))
	| otherwise = 
		withClientM cli clientenv $ \case
			Left err -> throwM err
			Right res -> return res
  where
	cli = case ver of
		3 -> v3 su V3 k cu bypass auth
		2 -> v2 su V2 k cu bypass auth
		1 -> plus <$> v1 su V1 k cu bypass auth
		_ -> error "unsupported protocol version"
	
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|>
		_ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		v3 :<|> v2 :<|> v1 :<|> _ = client p2pHttpAPI

clientLockContent
	:: ClientEnv
	-> ProtocolVersion
	-> B64Key
	-> B64UUID ServerSide
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Maybe Auth
	-> IO LockResult
clientLockContent clientenv (ProtocolVersion ver) k su cu bypass auth = 
	withClientM (cli k cu bypass auth) clientenv $ \case
		Left err -> throwM err
		Right res -> return res
  where
	cli = case ver of
		3 -> v3 su V3
		2 -> v2 su V2
		1 -> v1 su V1
		0 -> v0 su V0
		_ -> error "unsupported protocol version"
	
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|>
		_ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|>
		v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI

clientKeepLocked
	:: ClientEnv
	-> ProtocolVersion
	-> LockID
	-> B64UUID ServerSide
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Maybe Auth
	-> (TMVar Bool -> IO ())
	-- ^ The TMVar can be filled any number of times with True to send
	-- repeated keep locked requests, eg to keep a connection alive.
	-- Once filled with False, the lock will be dropped.
	-> IO ()
clientKeepLocked clientenv (ProtocolVersion ver) lckid su cu bypass auth a = do
	keeplocked <- newEmptyTMVarIO
	tid <- async $ a keeplocked
	let cli' = cli lckid (Just cu) bypass auth
		(Just connectionKeepAlive) (Just keepAlive)
		(S.fromStepT (unlocksender keeplocked))
	withClientM cli' clientenv $ \case
		Right (LockResult _ _) ->
			wait tid
		Left err  -> do
			wait tid
			throwM err
  where
	unlocksender keeplocked =
		S.Yield (UnlockRequest False) $ S.Effect $ do
			return $ S.Effect $ do
				stilllocked <- liftIO $ atomically $ takeTMVar keeplocked
				return $ if stilllocked
					then unlocksender keeplocked
					else S.Yield (UnlockRequest True) S.Stop
	
	cli = case ver of
		3 -> v3 su V3
		2 -> v2 su V2
		1 -> v1 su V1
		0 -> v0 su V0
		_ -> error "unsupported protocol version"
	
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|>
		_ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI

#endif
-- ^ WITH_SERVANT
