{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Version where

import Control.Monad.State (liftIO)
import Data.String.Utils
import Data.Maybe

import Command
import qualified SysConfig
import Version

command :: [Command]
command = [standaloneCommand "version" paramNothing seek "show version info"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStartNothing
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
