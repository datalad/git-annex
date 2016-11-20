{- git-annex command
 -
 - Copyright 2014-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.RemoteDaemon where

import Command
import RemoteDaemon.Core
import Utility.Daemon

cmd :: Command
cmd = noCommit $
	command "remotedaemon" SectionMaintenance
		"persistent communication with remotes"
		paramNothing (run <$$> const parseDaemonOptions)

run :: DaemonOptions -> CommandSeek
run o
	| stopDaemonOption o = error "--stop not implemented for remotedaemon"
	| foregroundDaemonOption o = liftIO runInteractive
	| otherwise = do
#ifndef mingw32_HOST_OS
		nullfd <- liftIO $ openFd "/dev/null" ReadOnly Nothing defaultFileFlags
		liftIO $ daemonize nullfd Nothing False runNonInteractive
#else
		liftIO $ foreground Nothing runNonInteractive	
#endif
