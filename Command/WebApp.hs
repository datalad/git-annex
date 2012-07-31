{- git-annex webapp launcher
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.WebApp where

import Common.Annex
import Command
import Assistant
import Assistant.DaemonStatus
import Assistant.TransferQueue
import Assistant.Threads.WebApp
import Utility.WebApp
import Utility.ThreadScheduler
import Utility.Daemon (checkDaemon)
import Init
import qualified Command.Watch

import Control.Concurrent.STM

def :: [Command]
def = [oneShot $ noRepo firstRun $ dontCheck repoExists $
	withOptions [Command.Watch.foregroundOption, Command.Watch.stopOption] $
        command "webapp" paramNothing seek "launch webapp"]

seek :: [CommandSeek]
seek = [withFlag Command.Watch.stopOption $ \stopdaemon ->
	withFlag Command.Watch.foregroundOption $ \foreground ->
	withNothing $ start foreground stopdaemon]

start :: Bool -> Bool -> CommandStart
start foreground stopdaemon = notBareRepo $ do
	if stopdaemon
		then stopDaemon
		else ifM (isInitialized) ( go , liftIO firstRun )
	stop
	where
		go = do
			f <- liftIO . absPath =<< fromRepo gitAnnexHtmlShim
			ifM (checkpid <&&> checkshim f) $
				( liftIO $ openBrowser f 
				, startDaemon True foreground $ Just openBrowser
				)
		checkpid = do
			pidfile <- fromRepo gitAnnexPidFile
			liftIO $ isJust <$> checkDaemon pidfile
		checkshim f = liftIO $ doesFileExist f

openBrowser :: FilePath -> IO ()
openBrowser htmlshim = unlessM (runBrowser url) $
	error $ "failed to start web browser on url " ++ url
	where
		url = "file://" ++ htmlshim

firstRun :: IO ()
firstRun = do
	dstatus <- atomically . newTMVar =<< newDaemonStatus
	transferqueue <- newTransferQueue
	webAppThread Nothing dstatus transferqueue $ Just $ \f -> do
		openBrowser f
		waitForTermination
