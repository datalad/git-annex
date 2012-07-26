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
import Utility.WebApp
import Utility.Daemon
import qualified Annex

import Control.Concurrent
import System.Posix.Process

def :: [Command]
def = [command "webapp" paramNothing seek "launch webapp"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = notBareRepo $ do
	r <- checkpid
	when (r == Nothing) $
		startassistant
	f <- liftIO . absPath =<< fromRepo gitAnnexHtmlShim
	let url = "file://" ++ f
	ifM (liftIO $ runBrowser url)
		( stop
		, error $ "failed to start web browser on url " ++ url
		)
	where
		checkpid = do
			pidfile <- fromRepo gitAnnexPidFile
			liftIO $ checkDaemon pidfile
		startassistant = do
			{- Fork a separate process to run the assistant,
			 - with a copy of the Annex state. -}
			state <- Annex.getState id
			liftIO $ void $ forkProcess $
				Annex.eval state $ startDaemon True False
			waitdaemon (100 :: Int)
		waitdaemon 0 = error "failed to start git-annex assistant"
		waitdaemon n = do
			r <- checkpid
			case r of
				Just _ -> return ()
				Nothing -> do
					liftIO $
						threadDelay 100000 -- 0.1 seconds
					

waitdaemon (n - 1)
