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
import P2P.Http
import qualified P2P.Protocol as P2P
import Annex.Url
import Utility.Env

import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.Client.Streaming
import Control.Concurrent
import Control.Concurrent.STM
import Network.Socket (PortNumber)
import qualified Data.Map as M

cmd :: Command
cmd = command "p2phttp" SectionPlumbing
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
seek o = startConcurrency commandStages $
	withLocalP2PConnections $ \acquireconn -> liftIO $ do
		authenv <- getAuthEnv
		st <- mkP2PHttpServerState acquireconn $
			mkGetServerMode authenv o
		Warp.run (fromIntegral port) (p2pHttpApp st)
  where
	port = fromMaybe 80 (portOption o)

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

testKeepLocked = do
	mgr <- httpManager <$> getUrlOptions
	burl <- liftIO $ parseBaseUrl "http://localhost:8080/"
	keeplocked <- liftIO newEmptyTMVarIO
	_ <- liftIO $ forkIO $ do
		print "running, press enter to drop lock"
		_ <- getLine
		atomically $ writeTMVar keeplocked False
	liftIO $ clientKeepLocked (mkClientEnv mgr burl)
		(P2P.ProtocolVersion 3)
		(B64UUID (toUUID ("lck" :: String)))
		(B64UUID (toUUID ("cu" :: String)))
		(B64UUID (toUUID ("su" :: String)))
		[]
		keeplocked

testCheckPresent = do
	mgr <- httpManager <$> getUrlOptions
	burl <- liftIO $ parseBaseUrl "http://localhost:8080/"
	res <- liftIO $ clientCheckPresent (mkClientEnv mgr burl)
		(P2P.ProtocolVersion 3)
		(B64Key (fromJust $ deserializeKey ("WORM-s30-m1720547401--foo" :: String)))
		(B64UUID (toUUID ("cu" :: String)))
		(B64UUID (toUUID ("f11773f0-11e1-45b2-9805-06db16768efe" :: String)))
		[]
		Nothing
	liftIO $ print res
