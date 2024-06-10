{- git-annex command
 -
 - Copyright 2016-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, CPP #-}

module Command.EnableTor where

import Command
import qualified Annex
import P2P.Address
import P2P.Annex
import Utility.Tor
import Annex.UUID
#ifndef mingw32_HOST_OS
import Annex.Path
#endif
import P2P.IO
import qualified P2P.Protocol as P2P
import Utility.ThreadScheduler
import RemoteDaemon.Transport.Tor
import Git.Types
import Config

import Control.Concurrent.Async
import qualified Network.Socket as S
#ifndef mingw32_HOST_OS
import Utility.Su
import System.Posix.User
#endif

cmd :: Command
cmd = noCommit $ dontCheck repoExists $
	command "enable-tor" SectionSetup "enable tor hidden service"
		"uid" (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

-- This runs as root, so avoid making any commits or initializing
-- git-annex, or doing other things that create root-owned files.
start :: [String] -> CommandStart
#ifndef mingw32_HOST_OS
start os = do
#else
start _os = do
#endif
#ifndef mingw32_HOST_OS
	let ai = ActionItemOther Nothing
	let si = SeekInput []
	curruserid <- liftIO getEffectiveUserID
	if curruserid == 0
		then case readish =<< headMaybe os of
			Nothing -> giveup "Need user-id parameter."
			Just userid -> go userid
		else starting "enable-tor" ai si $ do
			gitannex <- liftIO programPath
			let ps = [Param (cmdname cmd), Param (show curruserid)]
			sucommand <- liftIO $ mkSuCommand gitannex ps
			cleanenv <- liftIO $ cleanStandaloneEnvironment
			maybe noop (showLongNote . UnquotedString)
				(describePasswordPrompt' sucommand)
			ifM (liftIO $ runSuCommand sucommand cleanenv)
				( next checkHiddenService
				, giveup $ unwords $
					[ "Failed to run as root:" , gitannex ] ++ toCommand ps
				)
#else
	go 0
#endif
  where
	go userid = do
		-- Usually git will refuse to read local configs of a git
		-- repo belonging to another user. But in this case, the
		-- user wants this command, run as root, to operate on
		-- their repo. Behave as if --git-dir had been used to
		-- specify that the git directory is intended to be used.
		Annex.adjustGitRepo $ \r -> return $ r
			{ gitDirSpecifiedExplicitly = True }
		reloadConfig

		uuid <- getUUID
		when (uuid == NoUUID) $
			giveup "This can only be run in a git-annex repository."
		(onionaddr, onionport) <- liftIO $
			addHiddenService torAppName userid (fromUUID uuid)
		storeP2PAddress $ TorAnnex onionaddr onionport
		stop

checkHiddenService :: CommandCleanup
checkHiddenService = bracket setup cleanup go
  where
	setup = do
		showLongNote "Tor hidden service is configured. Checking connection to it. This may take a few minutes."
		startlistener
	
	cleanup = liftIO . cancel
	
	go _ = check (150 :: Int) =<< filter istoraddr <$> loadP2PAddresses
	
	istoraddr (TorAnnex _ _) = True

	check 0 _ = giveup "Still unable to connect to hidden service. It might not yet be usable by others. Please check Tor's logs for details."
	check _ [] = giveup "Somehow didn't get an onion address."
	check n addrs@(addr:_) =
		-- Connect but don't bother trying to auth,
		-- we just want to know if the tor circuit works.
		liftIO (tryNonAsync $ connectPeer Nothing addr) >>= \case
			Left e -> do
				warning $ UnquotedString $ "Unable to connect to hidden service. It may not yet have propagated to the Tor network. (" ++ show e ++ ") Will retry.."
				liftIO $ threadDelaySeconds (Seconds 2)
				check (n-1) addrs
			Right conn -> do
				liftIO $ closeConnection conn
				showLongNote "Tor hidden service is working."
				return True
	
	-- Unless the remotedaemon is already listening on the hidden
	-- service's socket, start a listener. This is only run during the
	-- check, and it refuses all auth attempts.
	startlistener = do
		u <- getUUID
		msock <- torSocketFile
		case msock of
			Just sockfile -> ifM (liftIO $ haslistener sockfile)
				( liftIO $ async $ return ()
				, liftIO $ async $ runlistener sockfile u
				)
			Nothing -> giveup "Could not find socket file in Tor configuration!"
	
	runlistener sockfile u = serveUnixSocket sockfile $ \h -> do
		let conn = P2PConnection
			{ connRepo = Nothing
			, connCheckAuth = const False
			, connIhdl = h
			, connOhdl = h
			, connIdent = ConnIdent Nothing
			}
		runst <- mkRunState Client
		void $ runNetProto runst conn $ P2P.serveAuth u
		hClose h

	haslistener sockfile = catchBoolIO $ do
		soc <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
		S.connect soc (S.SockAddrUnix sockfile)
		S.close soc
		return True
