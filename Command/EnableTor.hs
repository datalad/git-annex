{- git-annex command
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.EnableTor where

import Command
import P2P.Address
import Utility.Tor
import Annex.UUID
import Config.Files

#ifndef mingw32_HOST_OS
import Utility.Su
import System.Posix.User
#endif

-- This runs as root, so avoid making any commits or initializing
-- git-annex, or doing other things that create root-owned files.
cmd :: Command
cmd = noCommit $ dontCheck repoExists $
	command "enable-tor" SectionSetup "enable tor hidden service"
		"uid" (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start os = do
#ifndef mingw32_HOST_OS
	curruserid <- liftIO getEffectiveUserID
	if curruserid == 0
		then case readish =<< headMaybe os of
			Nothing -> giveup "Need user-id parameter."
			Just userid -> go userid
		else do
			liftIO $ putStrLn "Need root access to enable tor..."
			gitannex <- liftIO readProgramFile
			let ps = [Param (cmdname cmd), Param (show curruserid)]
			ifM (liftIO $ runAsRoot gitannex ps)
				( stop
				, giveup $ unwords $
					[ "Failed to run as root:" , gitannex ] ++ toCommand ps
				)
#else
	go 0
#endif
  where
	go userid = do
		uuid <- getUUID
		when (uuid == NoUUID) $
			giveup "This can only be run in a git-annex repository."
		(onionaddr, onionport) <- liftIO $
			addHiddenService "tor-annex" userid (fromUUID uuid)
		storeP2PAddress $ TorAnnex onionaddr onionport
		stop
