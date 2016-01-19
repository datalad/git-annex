{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Version where

import Common.Annex
import Command
import qualified Build.SysConfig as SysConfig
import Annex.Version
import BuildFlags
import qualified Types.Backend as B
import qualified Types.Remote as R
import qualified Remote
import qualified Backend

cmd :: Command
cmd = dontCheck repoExists $ noCommit $ 
	noRepo (seekNoRepo <$$> optParser) $ 
		command "version" SectionQuery "show version info"
			paramNothing (seek <$$> optParser)

data VersionOptions = VersionOptions
	{ rawOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser VersionOptions
optParser _ = VersionOptions
	<$> switch
		( long "raw"
		<> help "output only program version"
		)

seek :: VersionOptions -> CommandSeek
seek o
	| rawOption o = liftIO showRawVersion
	| otherwise = showVersion

seekNoRepo :: VersionOptions -> IO ()
seekNoRepo o
	| rawOption o = showRawVersion
	| otherwise = showPackageVersion

showVersion :: Annex ()
showVersion = do
	v <- getVersion
	liftIO $ do
		showPackageVersion
		vinfo "local repository version" $ fromMaybe "unknown" v
		vinfo "supported repository versions" $
			unwords supportedVersions
		vinfo "upgrade supported from repository versions" $
			unwords upgradableVersions

showPackageVersion :: IO ()
showPackageVersion = do
	vinfo "git-annex version" SysConfig.packageversion
	vinfo "build flags" $ unwords buildFlags
	vinfo "key/value backends" $ unwords $ map B.name Backend.list
	vinfo "remote types" $ unwords $ map R.typename Remote.remoteTypes

showRawVersion :: IO ()
showRawVersion = do
	putStr SysConfig.packageversion
	hFlush stdout -- no newline, so flush

vinfo :: String -> String -> IO ()
vinfo k v = putStrLn $ k ++ ": " ++ v
