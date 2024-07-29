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

module P2P.Http.Client (
	module P2P.Http.Client,
	module P2P.Http.Types,
	Validity(..),
) where

import Types
import P2P.Http.Types
import P2P.Protocol hiding (Offset, Bypass, auth, FileSize)
import Utility.Metered
import Utility.FileSize
import Types.NumCopies

#ifdef WITH_SERVANT
import qualified Annex
import Annex.UUID
import Annex.Url
import Types.Remote
import P2P.Http
import P2P.Http.Url
import Annex.Common
import Annex.Concurrent
import Utility.Url (BasicAuth(..))
import Utility.HumanTime
import qualified Git.Credential as Git

import Servant hiding (BasicAuthData(..))
import Servant.Client.Streaming
import qualified Servant.Types.SourceT as S
import Network.HTTP.Types.Status
import Network.HTTP.Client
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Internal as LI
import qualified Data.Map as M
import Data.Time.Clock.POSIX
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent
import System.IO.Unsafe
#endif
import qualified Data.ByteString.Lazy as L

type ClientAction a
#ifdef WITH_SERVANT
	= ClientEnv
	-> ProtocolVersion
	-> B64UUID ServerSide
	-> B64UUID ClientSide
	-> [B64UUID Bypass]
	-> Maybe Auth
	-> Annex (Either ClientError a)
#else
	= ()
#endif

p2pHttpClient
	:: Remote
	-> (String -> Annex a)
	-> ClientAction a
	-> Annex a
p2pHttpClient rmt fallback clientaction = 
	p2pHttpClientVersions (const True) rmt fallback clientaction >>= \case
		Just res -> return res
		Nothing -> fallback "git-annex HTTP API server is missing an endpoint"

p2pHttpClientVersions
	:: (ProtocolVersion -> Bool)
	-> Remote
	-> (String -> Annex a)
	-> ClientAction a
	-> Annex (Maybe a)
#ifdef WITH_SERVANT
p2pHttpClientVersions allowedversion rmt fallback clientaction =
	case p2pHttpBaseUrl <$> remoteAnnexP2PHttpUrl (gitconfig rmt) of
		Nothing -> error "internal"
		Just baseurl -> do
			mgr <- httpManager <$> getUrlOptions
			let clientenv = mkClientEnv mgr baseurl
			ccv <- Annex.getRead Annex.gitcredentialcache
			Git.CredentialCache cc <- liftIO $ atomically $
				readTMVar ccv
			case M.lookup (Git.CredentialBaseURL credentialbaseurl) cc of
				Nothing -> go clientenv Nothing False Nothing versions
				Just cred -> go clientenv (Just cred) True (credauth cred) versions
  where
	versions = filter allowedversion allProtocolVersions
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
				return (Just resp)
			Left (FailureResponse _ resp)
				| statusCode (responseStatusCode resp) == 404 && not (null vs) ->
					go clientenv mcred credcached mauth vs
				| statusCode (responseStatusCode resp) == 401 ->
					case mcred of
						Nothing -> authrequired clientenv (v:vs)
						Just cred -> do
							inRepo $ Git.rejectUrlCredential cred
							Just <$> fallback (showstatuscode resp)
				| otherwise -> Just <$> fallback (showstatuscode resp)
			Left (ConnectionError ex) -> case fromException ex of
				Just (HttpExceptionRequest _ (ConnectionFailure err)) -> Just <$> fallback
					("unable to connect to HTTP server: " ++ show err)
				_ -> Just <$> fallback (show ex)
			Left clienterror -> Just <$> fallback
					("git-annex HTTP API server returned an unexpected response: " ++ show clienterror)
	go _ _ _ _ [] = return Nothing

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
		Just url -> url

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
p2pHttpClient _rmt fallback () = fallback
	"This remote uses an annex+http url, but this version of git-annex is not build with support for that."
#endif

clientGet
	:: Key
	-> AssociatedFile
	-> (L.ByteString -> IO BytesProcessed)
	-- ^ Must consume the entire ByteString before returning its
	-- total size.
	-> Maybe FileSize
	-- ^ Size of existing file, when resuming.
	-> ClientAction Validity
#ifdef WITH_SERVANT
clientGet k af consumer startsz clientenv (ProtocolVersion ver) su cu bypass auth = liftIO $ do
	let offset = fmap (Offset . fromIntegral) startsz
	withClientM (cli (B64Key k) cu bypass baf offset auth) clientenv $ \case
		Left err -> return (Left err)
		Right respheaders -> do
			b <- S.unSourceT (getResponse respheaders) gather
			BytesProcessed len <- consumer b
			let DataLength dl = case lookupResponseHeader @DataLengthHeader' respheaders of
				Header hdr -> hdr
				_ -> error "missing data length header"
			return $ Right $ 
				if dl == len then Valid else Invalid
  where
	cli =case ver of
		3 -> v3 su V3
		2 -> v2 su V2
		1 -> v1 su V1
		0 -> v0 su V0
		_ -> error "unsupported protocol version"
	
	v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI

	gather = unsafeInterleaveIO . gather'
	gather' S.Stop = return LI.Empty
	gather' (S.Error err) = giveup err
	gather' (S.Skip s) = gather' s
	gather' (S.Effect ms) = ms >>= gather'
	gather' (S.Yield v s) = LI.Chunk v <$> unsafeInterleaveIO (gather' s)
	
	baf = associatedFileToB64FilePath af
#else
clientGet _ _ _ _ = ()
#endif

clientCheckPresent :: Key -> ClientAction Bool
#ifdef WITH_SERVANT
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
#else
clientCheckPresent _ = ()
#endif

-- Similar to P2P.Protocol.remove.
clientRemoveWithProof
	:: Maybe SafeDropProof
	-> Key
	-> Annex RemoveResultPlus
	-> Remote
	-> Annex RemoveResultPlus
clientRemoveWithProof proof k unabletoremove remote =
	case safeDropProofEndTime =<< proof of
		Nothing -> removeanytime
		Just endtime -> removebefore endtime
  where
	removeanytime = p2pHttpClient remote giveup (clientRemove k)

	removebefore endtime =
		p2pHttpClientVersions useversion remote giveup clientGetTimestamp >>= \case
			Just (GetTimestampResult (Timestamp remotetime)) ->
				removebefore' endtime remotetime
			-- Peer is too old to support REMOVE-BEFORE.
			Nothing -> removeanytime
				
	removebefore' endtime remotetime =
		canRemoveBefore endtime remotetime (liftIO getPOSIXTime) >>= \case
			Just remoteendtime -> p2pHttpClient remote giveup $
				clientRemoveBefore k (Timestamp remoteendtime)
			Nothing -> unabletoremove
	
	useversion v = v >= ProtocolVersion 3

clientRemove :: Key -> ClientAction RemoveResultPlus
#ifdef WITH_SERVANT
clientRemove k clientenv (ProtocolVersion ver) su cu bypass auth =
	liftIO $ withClientM cli clientenv return
  where
	bk = B64Key k

	cli = case ver of
		3 -> v3 su V3 bk cu bypass auth
		2 -> v2 su V2 bk cu bypass auth
		1 -> plus <$> v1 su V1 bk cu bypass auth
		0 -> plus <$> v0 su V0 bk cu bypass auth
		_ -> error "unsupported protocol version"
	
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI
#else
clientRemove _ = ()
#endif

clientRemoveBefore
	:: Key
	-> Timestamp
	-> ClientAction RemoveResultPlus
#ifdef WITH_SERVANT
clientRemoveBefore k ts clientenv (ProtocolVersion ver) su cu bypass auth =
	liftIO $ withClientM (cli su (B64Key k) cu bypass ts auth) clientenv return
  where
	cli = case ver of
		3 -> flip v3 V3
		_ -> error "unsupported protocol version"
	
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		v3 :<|> _ = client p2pHttpAPI
#else
clientRemoveBefore _ _ = ()
#endif

clientGetTimestamp :: ClientAction GetTimestampResult
#ifdef WITH_SERVANT
clientGetTimestamp clientenv (ProtocolVersion ver) su cu bypass auth = 
	liftIO $ withClientM (cli su cu bypass auth) clientenv return
  where
	cli = case ver of
		3 -> flip v3 V3
		_ -> error "unsupported protocol version"
	
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|>
		v3 :<|> _ = client p2pHttpAPI
#else
clientGetTimestamp = ()
#endif

clientPut
	:: MeterUpdate
	-> Key
	-> Maybe Offset
	-> AssociatedFile
	-> FilePath
	-> FileSize
	-> Annex Bool
	-- ^ Called after sending the file to check if it's valid.
	-> ClientAction PutResultPlus
#ifdef WITH_SERVANT
clientPut meterupdate k moffset af contentfile contentfilesize validitycheck clientenv (ProtocolVersion ver) su cu bypass auth = do
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
			return (Left err)
		Right res -> do
			join $ liftIO (wait checkerthread)
			return (Right res)
  where
	stream h checkv checkresultv = S.SourceT $ \a -> do
		bl <- hGetContentsMetered h meterupdate
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
	
	bk = B64Key k

	cli src = case ver of
		3 -> v3 su V3 len bk cu bypass baf moffset src auth
		2 -> v2 su V2 len bk cu bypass baf moffset src auth
		1 -> plus <$> v1 su V1 len bk cu bypass baf moffset src auth
		0 -> plus <$> v0 su V0 len bk cu bypass baf moffset src auth
		_ -> error "unsupported protocol version"

	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|>
		_ :<|>
		v3 :<|> v2 :<|> v1 :<|> v0 :<|> _ = client p2pHttpAPI
#else
clientPut _ _ _ _ _ _ _ = ()
#endif

clientPutOffset
	:: Key
	-> ClientAction PutOffsetResultPlus
#ifdef WITH_SERVANT
clientPutOffset k clientenv (ProtocolVersion ver) su cu bypass auth
	| ver == 0 = return (Right (PutOffsetResultPlus (Offset 0)))
	| otherwise = liftIO $ withClientM cli clientenv return
  where
	bk = B64Key k

	cli = case ver of
		3 -> v3 su V3 bk cu bypass auth
		2 -> v2 su V2 bk cu bypass auth
		1 -> plus <$> v1 su V1 bk cu bypass auth
		_ -> error "unsupported protocol version"
	
	_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		_ :<|>
		_ :<|>
		_ :<|> _ :<|> _ :<|> _ :<|>
		v3 :<|> v2 :<|> v1 :<|> _ = client p2pHttpAPI
#else
clientPutOffset _ = ()
#endif

clientLockContent
	:: Key
	-> ClientAction LockResult
#ifdef WITH_SERVANT
clientLockContent k clientenv (ProtocolVersion ver) su cu bypass auth = 
	liftIO $ withClientM (cli (B64Key k) cu bypass auth) clientenv return
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
#else
clientLockContent _ = ()
#endif

clientKeepLocked
	:: LockID
	-> UUID
	-> a
	-> (VerifiedCopy -> Annex a)
	-- ^ Callback is run only after successfully connecting to the http
	-- server. The lock will remain held until the callback returns,
	-- and then will be dropped.
	-> ClientAction a
#ifdef WITH_SERVANT
clientKeepLocked lckid remoteuuid unablelock callback clientenv (ProtocolVersion ver) su cu bypass auth = do
	readyv <- liftIO newEmptyTMVarIO
	keeplocked <- liftIO newEmptyTMVarIO
	let cli' = cli lckid (Just cu) bypass auth
		(Just connectionKeepAlive) (Just keepAlive)
		(S.fromStepT (unlocksender readyv keeplocked))
	starttime <- liftIO getPOSIXTime
	tid <- liftIO $ async $ withClientM cli' clientenv $ \case
		Right (LockResult _ _) -> 
			atomically $ writeTMVar readyv (Right False)
		Left err -> 
			atomically $ writeTMVar readyv (Left err)
	let releaselock = liftIO $ do
		atomically $ putTMVar keeplocked False
		wait tid
	liftIO (atomically $ takeTMVar readyv) >>= \case
		Left err -> do
			liftIO $ wait tid
			return (Left err)
		Right False -> do
			liftIO $ wait tid
			return (Right unablelock)
		Right True -> do
			let checker = return $ Left $ starttime + retentionduration
			Right 
				<$> withVerifiedCopy LockedCopy remoteuuid checker callback
					`finally` releaselock
  where
	retentionduration = fromIntegral $
		durationSeconds p2pDefaultLockContentRetentionDuration

	unlocksender readyv keeplocked =
		S.Yield (UnlockRequest False) $ S.Effect $ do
			return $ S.Effect $ do
				liftIO $ atomically $ void $
					tryPutTMVar readyv (Right True)
				stilllocked <- liftIO $ atomically $
					takeTMVar keeplocked
				return $ if stilllocked
					then unlocksender readyv keeplocked
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
#else
clientKeepLocked _ _ _ _ = ()
#endif
