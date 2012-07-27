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
import Option

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
			void $ liftIO . nukeFile =<< fromRepo gitAnnexPidFile
			startassistant f
		else ifM (checkpid <&&> checkshim f) $
			( liftIO $ go f 
			, startassistant f
			)
	stop
	where
		checkpid = do
			pidfile <- fromRepo gitAnnexPidFile
			liftIO $ isJust <$> checkDaemon pidfile
		checkshim f = liftIO $ doesFileExist f
		startassistant = startDaemon True False . Just . go
		go f = unlessM (runBrowser url) $
			error $ "failed to start web browser on url " ++ url
			where
				url = "file://" ++ f
