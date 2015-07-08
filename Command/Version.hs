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
cmd = withOptions [rawOption] $ dontCheck repoExists $ noCommit $ 
	noRepo (parseparams startNoRepo) $ 
		command "version" SectionQuery "show version info"
			paramNothing (parseparams seek)
  where
	parseparams = withParams

rawOption :: Option
rawOption = flagOption [] "raw" "output only program version"

seek :: CmdParams -> CommandSeek
seek = withNothing $ ifM (getOptionFlag rawOption) (startRaw, start)

startRaw :: CommandStart
startRaw = do
	liftIO $ do
		putStr SysConfig.packageversion
		hFlush stdout
	stop

start :: CommandStart
start = do
	v <- getVersion
	liftIO $ do

		showPackageVersion
		info "local repository version" $ fromMaybe "unknown" v
		info "supported repository version" supportedVersion
		info "upgrade supported from repository versions" $
			unwords upgradableVersions
	stop

startNoRepo :: CmdParams -> IO ()
startNoRepo _ = showPackageVersion

showPackageVersion :: IO ()
showPackageVersion = do
	info "git-annex version" SysConfig.packageversion
	info "build flags" $ unwords buildFlags
	info "key/value backends" $ unwords $ map B.name Backend.list
	info "remote types" $ unwords $ map R.typename Remote.remoteTypes

info :: String -> String -> IO ()
info k v = putStrLn $ k ++ ": " ++ v
