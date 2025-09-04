{- git-annex command
 -
 - Copyright 2014-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.RemoteDaemon where

import Command
import RemoteDaemon.Core
import Utility.Daemon
#ifndef mingw32_HOST_OS
import Annex.Path
import Utility.OpenFd
#endif

cmd :: Command
cmd = noCommit $
	command "remotedaemon" SectionCommon
		"persistent communication with remotes"
		paramNothing (run <$$> const (parseDaemonOptions False))

run :: DaemonOptions -> CommandSeek
run o
	| stopDaemonOption o = giveup "--stop not implemented for remotedaemon"
	| foregroundDaemonOption o = liftIO runInteractive
	| otherwise = do
#ifndef mingw32_HOST_OS
		git_annex <- fromOsPath <$> liftIO programPath
		ps <- gitAnnexDaemonizeParams
		let logfd = openFdWithMode (toRawFilePath "/dev/null") ReadOnly Nothing 
			defaultFileFlags
			(CloseOnExecFlag True)
		liftIO $ daemonize git_annex ps logfd Nothing False runNonInteractive
#else
		liftIO $ foreground Nothing runNonInteractive	
#endif
