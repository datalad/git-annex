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
import qualified Command.Watch

def :: [Command]
def = [withOptions [Command.Watch.foregroundOption, Command.Watch.stopOption] $
        command "webapp" paramNothing seek "launch webapp"]

seek :: [CommandSeek]
seek = [withFlag Command.Watch.stopOption $ \stopdaemon ->
	withFlag Command.Watch.foregroundOption $ \foreground ->
	withNothing $ start foreground stopdaemon]

start :: Bool -> Bool -> CommandStart
start foreground stopdaemon = notBareRepo $ do
	if stopdaemon
		then stopDaemon
		else do
			f <- liftIO . absPath =<< fromRepo gitAnnexHtmlShim
			ifM (checkpid <&&> checkshim f) $
				( liftIO $ go f 
				, startDaemon True foreground $ Just $ go f
				)
	stop
	where
		checkpid = do
			pidfile <- fromRepo gitAnnexPidFile
			liftIO $ isJust <$> checkDaemon pidfile
		checkshim f = liftIO $ doesFileExist f
		go f = unlessM (runBrowser url) $
			error $ "failed to start web browser on url " ++ url
			where
				url = "file://" ++ f
