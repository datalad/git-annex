{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Version where

import Common.Annex
import Command
import qualified Build.SysConfig as SysConfig
import Annex.Version

command :: [Command]
command = [Command "version" paramNothing noChecks seek "show version info"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = do
	liftIO $ putStrLn $ "git-annex version: " ++ SysConfig.packageversion
	v <- getVersion
	liftIO $ putStrLn $ "local repository version: " ++ fromMaybe "unknown" v
	liftIO $ putStrLn $ "default repository version: " ++ defaultVersion
	liftIO $ putStrLn $ "supported repository versions: " ++ vs supportedVersions
	liftIO $ putStrLn $ "upgrade supported from repository versions: " ++ vs upgradableVersions
	stop
	where
		vs = join " "
