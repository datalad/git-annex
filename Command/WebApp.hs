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
import Utility.Daemon (checkDaemon)
import qualified Annex
import Option

import Control.Concurrent
import System.Posix.Process

def :: [Command]
def = [withOptions [restartOption] $
        command "webapp" paramNothing seek "launch webapp"]

restartOption :: Option
restartOption = Option.flag [] "restart" "restart the assistant daemon"

seek :: [CommandSeek]
seek = [withFlag restartOption $ \restart -> withNothing $ start restart]

start :: Bool -> CommandStart
start restart = notBareRepo $ do
	f <- liftIO . absPath =<< fromRepo gitAnnexHtmlShim
	if restart
		then do
			stopDaemon
			nuke =<< fromRepo gitAnnexPidFile
			startassistant f
		else unlessM (checkpid f) $
			startassistant f
	let url = "file://" ++ f
	ifM (liftIO $ runBrowser url)
		( stop
		, error $ "failed to start web browser on url " ++ url
		)
	where
		nuke f = void $ liftIO $ catchMaybeIO $ removeFile f
		checkpid f = do
			pidfile <- fromRepo gitAnnexPidFile
			liftIO $
				doesFileExist f <&&> (isJust <$> checkDaemon pidfile)
		startassistant f = do
			nuke f
			{- Fork a separate process to run the assistant,
			 - with a copy of the Annex state. -}
			state <- Annex.getState id
			liftIO $ void $ forkProcess $
				Annex.eval state $ startDaemon True False
			waitdaemon f (1000 :: Int)
		waitdaemon _ 0 = error "failed to start git-annex assistant"
		waitdaemon f n = unlessM (checkpid f) $ do
			-- wait 0.1 seconds before retry
			liftIO $ threadDelay 100000
			waitdaemon f (n - 1)
