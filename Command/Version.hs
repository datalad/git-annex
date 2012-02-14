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
import CmdLine

def :: [Command]
def = [noRepo showPackageVersion $ dontCheck repoExists $
	command "version" paramNothing seek "show version info"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = do
	v <- getVersion
	liftIO $ do
		showPackageVersion
		putStrLn $ "local repository version: " ++ fromMaybe "unknown" v
		putStrLn $ "default repository version: " ++ defaultVersion
		putStrLn $ "supported repository versions: " ++ vs supportedVersions
		putStrLn $ "upgrade supported from repository versions: " ++ vs upgradableVersions
	-- avoid normal cleanup
	_ <- shutdown True
	liftIO exitSuccess
	where
		vs = join " "

showPackageVersion :: IO ()
showPackageVersion = putStrLn $ "git-annex version: " ++ SysConfig.packageversion
