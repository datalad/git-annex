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
import Utility.Daemon (checkDaemon, lockPidFile)
import Init
import qualified Command.Watch
import qualified Git.CurrentRepo
import qualified Annex

import Control.Concurrent
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
				, startDaemon True foreground $ Just $
					const openBrowser
				)
		checkpid = do
			pidfile <- fromRepo gitAnnexPidFile
			liftIO $ isJust <$> checkDaemon pidfile
		checkshim f = liftIO $ doesFileExist f

openBrowser :: FilePath -> IO ()
openBrowser htmlshim = unlessM (runBrowser url) $
	error $ "failed to start web browser on url " ++ url
	where
		url = fileUrl htmlshim

fileUrl :: FilePath -> String
fileUrl file = "file://" ++ file

{- Run the webapp without a repository, which prompts the user, makes one,
 - changes to it, starts the regular assistant, and redirects the
 - browser to its url.
 -
 - This is a very tricky dance -- The first webapp calls the signaler,
 - which signals the main thread when it's ok to continue by writing to a
 - MVar. The main thread starts the second webapp, and uses its callback
 - to write its url back to the MVar, from where the signaler retrieves it,
 - returning it to the first webapp, which does the redirect.
 -
 - Note that it's important that mainthread never terminates! Much
 - of this complication is due to needing to keep the mainthread running.
 -}
firstRun :: IO ()
firstRun = do
	dstatus <- atomically . newTMVar =<< newDaemonStatus
	transferqueue <- newTransferQueue
	v <- newEmptyMVar
	let callback a = Just $ a v
	webAppThread Nothing dstatus transferqueue (callback signaler) (callback mainthread)
	where
		signaler v = do
			putMVar v ""
			putStrLn "signaler waiting..."
			r <- takeMVar v
			putStrLn "signaler got value"
			return r
		mainthread v _url htmlshim = do
			openBrowser htmlshim

			_wait <- takeMVar v

			state <- Annex.new =<< Git.CurrentRepo.get
			Annex.eval state $ do
				dummydaemonize
				startAssistant True id $ Just $ sendurlback v
		sendurlback v url _htmlshim = putMVar v url
		{- Set up the pid file in the new repo. -}
		dummydaemonize = do
			liftIO . lockPidFile =<< fromRepo gitAnnexPidFile
