{- git-annex command
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.P2PHttp where

import Command
import P2P.Http.Server
import P2P.Http.Client
import P2P.Http.Url
import qualified P2P.Protocol as P2P
import Annex.Url
import Utility.Env
import Utility.MonotonicClock

import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.Client.Streaming
import Control.Concurrent.STM
import Network.Socket (PortNumber)
import qualified Data.Map as M

cmd :: Command
cmd = withAnnexOptions [jobsOption] $ command "p2phttp" SectionPlumbing
	"communicate in P2P protocol over http"
	paramNothing (seek <$$> optParser)

data Options = Options
	{ portOption :: Maybe PortNumber
	, authEnvOption :: Bool
	, authEnvHttpOption :: Bool
	, unauthReadOnlyOption :: Bool
	, unauthAppendOnlyOption :: Bool
	, wideOpenOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser Options
optParser _ = Options
	<$> optional (option auto
		( long "port" <> metavar paramNumber
		<> help "specify port to listen on"
		))
	<*> switch
		( long "authenv"
		<> help "authenticate users from environment (https only)"
		)
	<*> switch
		( long "authenv-http"
		<> help "authenticate users from environment (including http)"
		)
	<*> switch
		( long "unauth-readonly"
		<> help "allow unauthenticated users to read the repository"
		)
	<*> switch
		( long "unauth-appendonly"
		<> help "allow unauthenticated users to read and append to the repository"
		)
	<*> switch
		( long "wideopen"
		<> help "give unauthenticated users full read+write access"
		)

seek :: Options -> CommandSeek
seek o = getAnnexWorkerPool $ \workerpool -> do
	-- XXX remove this
	when (isNothing (portOption o)) $ do
		liftIO $ putStrLn "test begins"
		testLocking
		giveup "TEST DONE" 
	withLocalP2PConnections workerpool $ \acquireconn -> liftIO $ do
		authenv <- getAuthEnv
		st <- mkP2PHttpServerState acquireconn workerpool $
			mkGetServerMode authenv o
		Warp.run (fromIntegral port) (p2pHttpApp st)
  where
	port = fromMaybe
		(fromIntegral defaultP2PHttpProtocolPort)
		(portOption o)

mkGetServerMode :: M.Map Auth P2P.ServerMode -> Options -> GetServerMode
mkGetServerMode _ o _ Nothing
	| wideOpenOption o = Just P2P.ServeReadWrite
	| unauthAppendOnlyOption o = Just P2P.ServeAppendOnly
	| unauthReadOnlyOption o = Just P2P.ServeReadOnly
	| otherwise = Nothing
mkGetServerMode authenv o issecure (Just auth) =
	case (issecure, authEnvOption o, authEnvHttpOption o) of
		(Secure, True, _) -> checkauth
		(NotSecure, _, True) -> checkauth
		_ -> noauth
  where
	checkauth = case M.lookup auth authenv of
		Just servermode -> Just servermode
		Nothing -> noauth
	noauth = mkGetServerMode authenv o issecure Nothing

getAuthEnv :: IO (M.Map Auth P2P.ServerMode)
getAuthEnv = do
	environ <- getEnvironment
	let permmap = M.fromList (mapMaybe parseperms environ)
	return $ M.fromList $
		map (addperms permmap) $
			mapMaybe parseusername environ
  where
	parseperms (k, v) = case deprefix "GIT_ANNEX_P2PHTTP_PERMISSIONS_" k of
		Nothing -> Nothing
		Just username -> case v of
			"readonly" -> Just
				(encodeBS username, P2P.ServeReadOnly)
			"appendonly" -> Just
				(encodeBS username, P2P.ServeAppendOnly)
			_ -> Nothing

	parseusername (k, v) = case deprefix "GIT_ANNEX_P2PHTTP_PASSWORD_" k of
		Nothing -> Nothing
		Just username -> Just $ Auth (encodeBS username) (encodeBS v)

	deprefix prefix s
		| prefix `isPrefixOf` s = Just (drop (length prefix) s)
		| otherwise = Nothing

	addperms permmap auth@(Auth user _) = 
		case M.lookup user permmap of
			Nothing -> (auth, P2P.ServeReadWrite)
			Just perms -> (auth, perms)

testLocking = do
	mgr <- httpManager <$> getUrlOptions
	burl <- liftIO $ parseBaseUrl "http://localhost:8080/"
	let k = B64Key (fromJust $ deserializeKey ("SHA256E-s6--5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03" :: String))
	res <- liftIO $ clientLockContent (mkClientEnv mgr burl)
		(P2P.ProtocolVersion 3)
		k
		(B64UUID (toUUID ("f11773f0-11e1-45b2-9805-06db16768efe" :: String)))
		(B64UUID (toUUID ("cu" :: String)))
		[] 
		Nothing
	case res of
		LockResult True (Just lckid) ->
			liftIO $ clientKeepLocked (mkClientEnv mgr burl)
				(P2P.ProtocolVersion 3)
				lckid
				(B64UUID (toUUID ("f11773f0-11e1-45b2-9805-06db16768efe" :: String)))
				(B64UUID (toUUID ("cu" :: String)))
				[] 
				Nothing $ \keeplocked -> do
					print "running, press enter to drop lock"
					_ <- getLine
					atomically $ writeTMVar keeplocked False	
		_ -> liftIO $ print ("lockin failed", res)

testLockContent = do
	mgr <- httpManager <$> getUrlOptions
	burl <- liftIO $ parseBaseUrl "http://localhost:8080/"
	res <- liftIO $ clientLockContent (mkClientEnv mgr burl)
		(P2P.ProtocolVersion 3)
		(B64Key (fromJust $ deserializeKey ("SHA256E-s6--5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03" :: String)))
		(B64UUID (toUUID ("f11773f0-11e1-45b2-9805-06db16768efe" :: String)))
		(B64UUID (toUUID ("cu" :: String)))
		[] 
		Nothing
	liftIO $ print res

testKeepLocked = do
	mgr <- httpManager <$> getUrlOptions
	burl <- liftIO $ parseBaseUrl "http://localhost:8080/"
	liftIO $ clientKeepLocked (mkClientEnv mgr burl)
		(P2P.ProtocolVersion 3)
		(B64UUID (toUUID ("lck" :: String)))
		(B64UUID (toUUID ("f11773f0-11e1-45b2-9805-06db16768efe" :: String)))
		(B64UUID (toUUID ("cu" :: String)))
		[] 
		Nothing $ \keeplocked -> do
			print "running, press enter to drop lock"
			_ <- getLine
			atomically $ writeTMVar keeplocked False

testGet = do
	mgr <- httpManager <$> getUrlOptions
	burl <- liftIO $ parseBaseUrl "http://localhost:8080/"
	res <- liftIO $ clientGet (mkClientEnv mgr burl)
		(P2P.ProtocolVersion 3)
		(B64Key (fromJust $ deserializeKey ("SHA256E-s1048576000--e3b67ce72aa2571c799d6419e3e36828461ac1c78f8ef300c7f9c8ae671c517f" :: String)))
		(B64UUID (toUUID ("f11773f0-11e1-45b2-9805-06db16768efe" :: String)))
		(B64UUID (toUUID ("cu" :: String)))
		[]
		Nothing
		Nothing
		"outfile"
	liftIO $ print res

testPut = do
	mgr <- httpManager <$> getUrlOptions
	burl <- liftIO $ parseBaseUrl "http://localhost:8080/"
	res <- clientPut (mkClientEnv mgr burl)
		(P2P.ProtocolVersion 3)
		(B64Key (fromJust $ deserializeKey ("SHA256E-s1048576000--b460ca923520db561d01b99483e9e2fe65ff9dfbdd52c17acba6ac4e874e27d5")))
		(B64UUID (toUUID ("f11773f0-11e1-45b2-9805-06db16768efe" :: String)))
		(B64UUID (toUUID ("cu" :: String)))
		[]
		Nothing
		Nothing
		(AssociatedFile (Just "foo"))
		"emptyfile"
		0
		(liftIO (print "validity check") >> return False)
	liftIO $ print res

testPutOffset = do
	mgr <- httpManager <$> getUrlOptions
	burl <- liftIO $ parseBaseUrl "http://localhost:8080/"
	res <- liftIO $ clientPutOffset (mkClientEnv mgr burl)
		(P2P.ProtocolVersion 3)
		(B64Key (fromJust $ deserializeKey ("SHA256E-s1048576000--b460ca923520db561d01b99483e9e2fe65ff9dfbdd52c17acba6ac4e874e27d5")))
		(B64UUID (toUUID ("f11773f0-11e1-45b2-9805-06db16768efe" :: String)))
		(B64UUID (toUUID ("cu" :: String)))
		[]
		Nothing
	liftIO $ print res

testRemove = do
	mgr <- httpManager <$> getUrlOptions
	burl <- liftIO $ parseBaseUrl "http://localhost:8080/"
	res <- liftIO $ clientRemove (mkClientEnv mgr burl)
		(P2P.ProtocolVersion 3)
		(B64Key (fromJust $ deserializeKey ("WORM-s30-m1720547401--foo" :: String)))
		(B64UUID (toUUID ("f11773f0-11e1-45b2-9805-06db16768efe" :: String)))
		(B64UUID (toUUID ("cu" :: String)))
		[]
		Nothing
	liftIO $ print res

testRemoveBefore = do
	mgr <- httpManager <$> getUrlOptions
	burl <- liftIO $ parseBaseUrl "http://localhost:8080/"
	MonotonicTimestamp t <- liftIO currentMonotonicTimestamp
	--liftIO $ threadDelaySeconds (Seconds 10)
	let ts = MonotonicTimestamp (t + 10)
	liftIO $ print ("running with timestamp", ts)
	res <- liftIO $ clientRemoveBefore (mkClientEnv mgr burl)
		(P2P.ProtocolVersion 3)
		(B64Key (fromJust $ deserializeKey ("WORM-s30-m1720617630--bar" :: String)))
		(B64UUID (toUUID ("f11773f0-11e1-45b2-9805-06db16768efe" :: String)))
		(B64UUID (toUUID ("cu" :: String)))
		[]
		(Timestamp ts)
		Nothing
	liftIO $ print res

testGetTimestamp = do
	mgr <- httpManager <$> getUrlOptions
	burl <- liftIO $ parseBaseUrl "http://localhost:8080/"
	res <- liftIO $ clientGetTimestamp (mkClientEnv mgr burl)
		(P2P.ProtocolVersion 3)
		(B64UUID (toUUID ("f11773f0-11e1-45b2-9805-06db16768efe" :: String)))
		(B64UUID (toUUID ("cu" :: String)))
		[]
		Nothing
	liftIO $ print res

