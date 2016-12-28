{- git-annex command
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.EnableTor where

import Command
import qualified Annex
import P2P.Address
import Utility.Tor
import Annex.UUID
import Config.Files
import P2P.IO
import qualified P2P.Protocol as P2P
import Utility.ThreadScheduler

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
seek = withWords start

-- This runs as root, so avoid making any commits or initializing
-- git-annex, or doing other things that create root-owned files.
start :: [String] -> CommandStart
start os = do
	uuid <- getUUID
	when (uuid == NoUUID) $
		giveup "This can only be run in a git-annex repository."
#ifndef mingw32_HOST_OS
	curruserid <- liftIO getEffectiveUserID
	if curruserid == 0
		then case readish =<< headMaybe os of
			Nothing -> giveup "Need user-id parameter."
			Just userid -> go uuid userid
		else do
			showStart "enable-tor" ""
			gitannex <- liftIO readProgramFile
			let ps = [Param (cmdname cmd), Param (show curruserid)]
			sucommand <- liftIO $ mkSuCommand gitannex ps
			maybe noop showLongNote
				(describePasswordPrompt' sucommand)
			ifM (liftIO $ runSuCommand sucommand)
				( next $ next checkHiddenService
				, giveup $ unwords $
					[ "Failed to run as root:" , gitannex ] ++ toCommand ps
				)
#else
	go uuid 0
#endif
  where
	go uuid userid = do
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
	check n addrs@(addr:_) = do
		g <- Annex.gitRepo
		-- Connect but don't bother trying to auth,
		-- we just want to know if the tor circuit works.
		cv <- liftIO $ tryNonAsync $ connectPeer g addr
		case cv of
			Left e -> do
				warning $ "Unable to connect to hidden service. It may not yet have propigated to the Tor network. (" ++ show e ++ ") Will retry.."
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
		r <- Annex.gitRepo
		u <- getUUID
		uid <- liftIO getRealUserID
		let ident = fromUUID u
		v <- liftIO $ getHiddenServiceSocketFile torAppName uid ident
		case v of
			Just sockfile -> ifM (liftIO $ haslistener sockfile)
				( liftIO $ async $ return ()
				, liftIO $ async $ runlistener sockfile u r
				)
			Nothing -> giveup "Could not find socket file in Tor configuration!"
	
	runlistener sockfile u r = serveUnixSocket sockfile $ \h -> do
		let conn = P2PConnection
			{ connRepo = r
			, connCheckAuth = const False
			, connIhdl = h
			, connOhdl = h
			}
		void $ runNetProto conn $ P2P.serveAuth u
		hClose h

	haslistener sockfile = catchBoolIO $ do
		soc <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
		S.connect soc (S.SockAddrUnix sockfile)
		S.close soc
		return True
