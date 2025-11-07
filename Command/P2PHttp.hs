{- git-annex command
 -
 - Copyright 2024-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.P2PHttp where

import Command hiding (jobsOption)
import P2P.Http.Server
import P2P.Http.Url
import qualified P2P.Protocol as P2P
import Utility.Env
import Annex.UUID
import qualified Git
import qualified Git.Construct
import qualified Annex
import Types.Concurrency
import qualified Utility.RawFilePath as R
import Utility.FileMode

import Servant
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import Network.Socket (PortNumber)
import qualified Network.Socket as Socket
import System.PosixCompat.Files (isSymbolicLink)
import qualified Data.Map as M
import Data.String
import Control.Concurrent.STM

cmd :: Command
cmd = noMessages $ dontCheck repoExists $ 
	noRepo (startIO <$$> optParser) $
		command "p2phttp" SectionPlumbing
			"communicate in P2P protocol over http"
			paramNothing (startAnnex <$$> optParser)

data Options = Options
	{ portOption :: Maybe PortNumber
	, bindOption :: Maybe String
	, socketOption :: Maybe FilePath
	, certFileOption :: Maybe FilePath
	, privateKeyFileOption :: Maybe FilePath
	, chainFileOption :: [FilePath]
	, authEnvOption :: Bool
	, authEnvHttpOption :: Bool
	, unauthReadOnlyOption :: Bool
	, unauthAppendOnlyOption :: Bool
	, unauthNoLockingOption :: Bool
	, wideOpenOption :: Bool
	, proxyConnectionsOption :: Maybe Integer
	, jobsOption :: Maybe Concurrency
	, clusterJobsOption :: Maybe Int
	, lockedFilesOption :: Maybe Integer
	, directoryOption :: [FilePath]
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
		( long "socket" <> metavar paramPath
		<> help "bind to unix domain socket"
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
		( long "unauth-nolocking"
		<> help "prevent unauthenticated users from locking content in the repository"
		)
	<*> switch
		( long "wideopen"
		<> help "give unauthenticated users full read+write access"
		)
	<*> optional (option auto
		( long "proxyconnections" <> metavar paramNumber
		<> help "maximum number of idle connections when proxying"
		))
	<*> optional jobsOptionParser
	<*> optional (option auto
		( long "clusterjobs" <> metavar paramNumber
		<> help "number of concurrent node accesses per connection"
		))
	<*> optional (option auto
		( long "lockedfiles" <> metavar paramNumber
		<> help "number of content files that can be locked"
		))
	<*> many (strOption
		( long "directory" <> metavar paramPath
		<> help "serve repositories in subdirectories of a directory"
		))

startAnnex :: Options -> Annex ()
startAnnex o
	| null (directoryOption o) = ifM ((/=) NoUUID <$> getUUID)
		( do
			lockedfilesqsem <- liftIO $ 
				mkLockedFilesQSem (lockedFilesOption o)
			authenv <- liftIO getAuthEnv
			st <- mkServerState o authenv lockedfilesqsem
			liftIO $ runServer o st
		-- Run in a git repository that is not a git-annex repository.
		, liftIO $ startIO o 
		)
	| otherwise = liftIO $ startIO o

startIO :: Options -> IO ()
startIO o
	| null (directoryOption o) = 
		giveup "Use the --directory option to specify which git-annex repositories to serve."
	| otherwise = do
		authenv <- getAuthEnv
		st <- mkst authenv mempty
		runServer o st
  where
	mkst authenv oldst = do
		lockedfilesqsem <- mkLockedFilesQSem (lockedFilesOption o)
		repos <- findRepos o
		sts <- forM repos $ \r -> do
			strd <- Annex.new r
			Annex.eval strd (mkstannex authenv oldst lockedfilesqsem)
		return (mconcat sts)
			{ updateRepos = updaterepos authenv
			}
	
	mkstannex authenv oldst lockedfilesqsem = do
		u <- getUUID
		if u == NoUUID
			then return mempty
			else case M.lookup u (servedRepos oldst) of
				Nothing -> mkServerState o authenv lockedfilesqsem
				Just old -> return $ P2PHttpServerState
					{ servedRepos = M.singleton u old
					, serverShutdownCleanup = mempty
					, updateRepos = mempty
					}
	
	updaterepos authenv oldst = do
		newst <- mkst authenv oldst
		return $ newst
			{ serverShutdownCleanup = 
				serverShutdownCleanup newst 
					<> serverShutdownCleanup oldst
			}

runServer :: Options -> P2PHttpServerState -> IO ()
runServer o mst = go `finally` serverShutdownCleanup mst
  where
	go = do
		let settings = Warp.setPort port $ Warp.setHost host $
			Warp.defaultSettings
		mstv <- newTMVarIO mst
		let app = p2pHttpApp mstv
		case (certFileOption o, privateKeyFileOption o) of
			(Nothing, Nothing) -> case socketOption o of
				Nothing -> Warp.runSettings settings app
				Just socketpath -> 
					withsocket socketpath $ \sock ->
						Warp.runSettingsSocket settings sock app
			(Just certfile, Just privatekeyfile) -> do
				let tlssettings = Warp.tlsSettingsChain
					certfile (chainFileOption o) privatekeyfile
				case socketOption o of
					Nothing -> Warp.runTLS tlssettings settings app
					Just socketpath -> 
						withsocket socketpath $ \sock ->
							Warp.runTLSSocket tlssettings settings sock app
			_ -> giveup "You must use both --certfile and --privatekeyfile options to enable HTTPS."
	port = maybe
		(fromIntegral defaultP2PHttpProtocolPort)
		fromIntegral
		(portOption o)
	host = maybe
		(fromString "*") -- both ipv4 and ipv6
		fromString
		(bindOption o)
	withsocket socketpath =
		bracket (opensocket socketpath) Socket.close
	opensocket socketpath = protectedOutput $ do
		sock <- Socket.socket Socket.AF_UNIX Socket.Stream 0
		Socket.bind sock $ Socket.SockAddrUnix socketpath
		Socket.listen sock Socket.maxListenQueue
		return sock

mkServerState :: Options -> M.Map Auth P2P.ServerMode -> LockedFilesQSem -> Annex P2PHttpServerState
mkServerState o authenv lockedfilesqsem = 
	withAnnexWorkerPool (jobsOption o) $
		mkP2PHttpServerState
			(mkGetServerMode authenv o)
			return
			(fromMaybe 1 $ proxyConnectionsOption o)
			(fromMaybe 1 $ clusterJobsOption o)
			lockedfilesqsem

mkGetServerMode :: M.Map Auth P2P.ServerMode -> Options -> GetServerMode
mkGetServerMode _ o _ Nothing
	| wideOpenOption o = ServerMode
		{ serverMode = P2P.ServeReadWrite
		, unauthenticatedLockingAllowed = unauthlock
		, authenticationAllowed = False 
		}
	| unauthAppendOnlyOption o = ServerMode 
		{ serverMode = P2P.ServeAppendOnly
		, unauthenticatedLockingAllowed = unauthlock
		, authenticationAllowed = canauth
		}
	| unauthReadOnlyOption o = ServerMode
		{ serverMode = P2P.ServeReadOnly
		, unauthenticatedLockingAllowed = unauthlock
		, authenticationAllowed = canauth
		}
	| otherwise = CannotServeRequests
  where
	canauth = authEnvOption o || authEnvHttpOption o
	unauthlock = not (unauthNoLockingOption o)
mkGetServerMode authenv o issecure (Just auth) =
	case (issecure, authEnvOption o, authEnvHttpOption o) of
		(Secure, True, _) -> checkauth
		(NotSecure, _, True) -> checkauth
		_ -> noauth
  where
	checkauth = case M.lookup auth authenv of
		Just servermode -> ServerMode 
			{ serverMode = servermode
			, authenticationAllowed = False
			, unauthenticatedLockingAllowed = False
			}
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

findRepos :: Options -> IO [Git.Repo]
findRepos o = do
	files <- concat
		<$> mapM (dirContents . toOsPath) (directoryOption o)
	concat <$> mapM go files
  where
	go f = Git.Construct.checkForRepo f >>= \case
		Just loc -> return [Git.Construct.newFrom loc]
		Nothing -> 
			-- Avoid following symlinks, both to avoid
			-- cycles and in case there is an unexpected
			-- symlink to some other directory we are not
			-- supposed to serve.
			ifM (isSymbolicLink <$> R.getSymbolicLinkStatus (fromOsPath f))
				( return []
				-- Ignore any errors getting the contents of a
				-- subdirectory.
				, catchNonAsync
					(concat <$> (mapM go =<< dirContents f))
					(const (return []))
				)
