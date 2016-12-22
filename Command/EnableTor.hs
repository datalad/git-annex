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
import Utility.ThreadScheduler

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
			showLongNote "Need root access to enable tor..."
			gitannex <- liftIO readProgramFile
			let ps = [Param (cmdname cmd), Param (show curruserid)]
			ifM (liftIO $ runAsRoot gitannex ps)
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
checkHiddenService = do
	showLongNote "Tor hidden service is configured. Checking connection to it. This may take a few minutes."
	go (150 :: Int) =<< filter istoraddr <$> loadP2PAddresses
  where
	istoraddr (TorAnnex _ _) = True

	go 0 _ = giveup "Still unable to connect to hidden service. It might not yet be usable by others. Please check Tor's logs for details."
	go _ [] = giveup "Somehow didn't get an onion address."
	go n addrs@(addr:_) = do
		g <- Annex.gitRepo
		-- Connect to ourselves; don't bother trying to auth,
		-- we just want to know if the circuit works.
		cv <- liftIO $ tryNonAsync $ connectPeer g addr
		case cv of
			Left e -> do
				warning $ "Unable to connect to hidden service. It may not yet have propigated to the Tor network. (" ++ show e ++ ") Will retry.."
				liftIO $ threadDelaySeconds (Seconds 2)
				go (n-1) addrs
			Right conn -> do
				liftIO $ closeConnection conn
				showLongNote "Tor hidden service is working."
				return True
