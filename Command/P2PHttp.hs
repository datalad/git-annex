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
import P2P.Http.Url
import qualified P2P.Protocol as P2P
import Utility.Env

import Servant
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import Network.Socket (PortNumber)
import qualified Data.Map as M
import Data.String

cmd :: Command
cmd = noMessages $ withAnnexOptions [jobsOption] $
	command "p2phttp" SectionPlumbing
		"communicate in P2P protocol over http"
		paramNothing (seek <$$> optParser)

data Options = Options
	{ portOption :: Maybe PortNumber
	, bindOption :: Maybe String
	, certFileOption :: Maybe FilePath
	, privateKeyFileOption :: Maybe FilePath
	, chainFileOption :: [FilePath]
	, authEnvOption :: Bool
	, authEnvHttpOption :: Bool
	, unauthReadOnlyOption :: Bool
	, unauthAppendOnlyOption :: Bool
	, wideOpenOption :: Bool
	, proxyConnectionsOption :: Maybe Integer
	, clusterJobsOption :: Maybe Int
	}

optParser :: CmdParamsDesc -> Parser Options
optParser _ = Options
	<$> optional (option auto
		( long "port" <> metavar paramNumber
		<> help "specify port to listen on"
		))
	<*> optional (strOption
		( long "bind" <> metavar paramAddress
		<> help "specify address to bind to"
		))
	<*> optional (strOption
		( long "certfile" <> metavar paramFile
		<> help "TLS certificate file for HTTPS"
		))
	<*> optional (strOption
		( long "privatekeyfile" <> metavar paramFile
		<> help "TLS private key file for HTTPS"
		))
	<*> many (strOption
		( long "chainfile" <> metavar paramFile
		<> help "TLS chain file"
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
	<*> optional (option auto
		( long "proxyconnections" <> metavar paramNumber
		<> help "maximum number of idle connections when proxying"
		))
	<*> optional (option auto
		( long "clusterjobs" <> metavar paramNumber
		<> help "number of concurrent node accesses per connection"
		))

seek :: Options -> CommandSeek
seek o = getAnnexWorkerPool $ \workerpool ->
	withP2PConnections workerpool
		(fromMaybe 1 $ proxyConnectionsOption o)
		(fromMaybe 1 $ clusterJobsOption o)
		(go workerpool)
  where
	go workerpool acquireconn = liftIO $ do
		authenv <- getAuthEnv
		st <- mkP2PHttpServerState acquireconn workerpool $
			mkGetServerMode authenv o
		let settings = Warp.setPort port $ Warp.setHost host $
			Warp.defaultSettings
		case (certFileOption o, privateKeyFileOption o) of
			(Nothing, Nothing) -> Warp.runSettings settings (p2pHttpApp st)
			(Just certfile, Just privatekeyfile) -> do
				let tlssettings = Warp.tlsSettingsChain
					certfile (chainFileOption o) privatekeyfile
				Warp.runTLS tlssettings settings (p2pHttpApp st)
			_ -> giveup "You must use both --certfile and --privatekeyfile options to enable HTTPS."
	
	port = maybe
		(fromIntegral defaultP2PHttpProtocolPort)
		fromIntegral
		(portOption o)
	host = maybe
		(fromString "*") -- both ipv4 and ipv6
		fromString
		(bindOption o)

mkGetServerMode :: M.Map Auth P2P.ServerMode -> Options -> GetServerMode
mkGetServerMode _ o _ Nothing
	| wideOpenOption o =
		ServerMode P2P.ServeReadWrite False
	| unauthAppendOnlyOption o =
		ServerMode P2P.ServeAppendOnly canauth
	| unauthReadOnlyOption o =
		ServerMode P2P.ServeReadOnly canauth
	| otherwise = CannotServeRequests
  where
	canauth = authEnvOption o || authEnvHttpOption o
mkGetServerMode authenv o issecure (Just auth) =
	case (issecure, authEnvOption o, authEnvHttpOption o) of
		(Secure, True, _) -> checkauth
		(NotSecure, _, True) -> checkauth
		_ -> noauth
  where
	checkauth = case M.lookup auth authenv of
		Just servermode -> ServerMode servermode False
		Nothing -> noauth
	noauth = mkGetServerMode authenv noautho issecure Nothing
	noautho = o { authEnvOption = False, authEnvHttpOption = False }

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
