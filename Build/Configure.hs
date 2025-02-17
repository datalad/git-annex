{- Checks system configuration and generates Build/SysConfig and Build/Version. -}

{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE CPP #-}

module Build.Configure where

import Build.TestConfig
import Build.Version
import Utility.SafeCommand
import Utility.Env.Basic
import qualified Git.Version
import Utility.SystemDirectory
import Utility.OsPath

import Control.Monad
import Control.Applicative
import Prelude

tests :: [TestCase]
tests =
	[ TestCase "UPGRADE_LOCATION" getUpgradeLocation
	, TestCase "git" $ testCmd "git" "git --version >/dev/null"
	, TestCase "git version" getGitVersion
	, testCp "cp_a" "-a"
	, testCp "cp_p" "-p"
	, testCp "cp_preserve_timestamps" "--preserve=timestamps"
	, testCpReflinkAuto
	, testCpNoPreserveXattr
	, TestCase "xargs -0" $ testCmd "xargs_0" "xargs -0 </dev/null"
	, TestCase "rsync" $ testCmd "rsync" "rsync --version >/dev/null"
	, TestCase "curl" $ testCmd "curl" "curl --version >/dev/null"
	, TestCase "bup" $ testCmd "bup" "bup --version >/dev/null"
	, TestCase "borg" $ testCmd "borg" "borg --version >/dev/null"
	, TestCase "nice" $ testCmd "nice" "nice true >/dev/null"
	, TestCase "ionice" $ testCmd "ionice" "ionice -c3 true >/dev/null"
	, TestCase "nocache" $ testCmd "nocache" "nocache true >/dev/null"
	, TestCase "gpg" $ maybeSelectCmd "gpg"
		[ ("gpg", "--version >/dev/null")
		, ("gpg2", "--version >/dev/null") ]
	, TestCase "lsof" $ findCmdPath "lsof" "lsof"
	, TestCase "git-remote-gcrypt" $ findCmdPath "gcrypt" "git-remote-gcrypt"
	, TestCase "ssh connection caching" getSshConnectionCaching
	]

tmpDir :: String
tmpDir = "tmp"

testFile :: String
testFile = tmpDir ++ "/testfile"

testCp :: ConfigKey -> String -> TestCase
testCp k option = TestCase cmd $ testCmd k cmdline
  where
	cmd = "cp " ++ option
	cmdline = cmd ++ " " ++ testFile ++ " " ++ testFile ++ ".new"

testCpReflinkAuto :: TestCase
#ifdef mingw32_HOST_OS
-- Windows does not support reflink so don't even try to use the option.
testCpReflinkAuto = TestCase k (return $ Config k (BoolConfig False))
#else
testCpReflinkAuto = testCp k "--reflink=auto"
#endif
  where
	k = "cp_reflink_supported"

testCpNoPreserveXattr :: TestCase
testCpNoPreserveXattr = testCp 
	"cp_no_preserve_xattr_supported" 
	"--no-preserve=xattr"

getUpgradeLocation :: Test
getUpgradeLocation = do
	e <- getEnv "UPGRADE_LOCATION"
	return $ Config "upgradelocation" $ MaybeStringConfig e

getGitVersion :: Test
getGitVersion = go =<< getEnv "FORCE_GIT_VERSION"
  where
	go (Just s) = return $ Config "gitversion" $ StringConfig s
	go Nothing = do
		v <- Git.Version.installed
		let oldestallowed = Git.Version.normalize "2.1"
		when (v < oldestallowed) $
			error $ "installed git version " ++ show v ++ " is too old! (Need " ++ show oldestallowed ++ " or newer)"
		return $ Config "gitversion" $ StringConfig $ show v

getSshConnectionCaching :: Test
getSshConnectionCaching = Config "sshconnectioncaching" . BoolConfig <$>
	boolSystem "sh" [Param "-c", Param "ssh -o ControlPersist=yes -V >/dev/null 2>/dev/null"]

setup :: IO ()
setup = do
	createDirectoryIfMissing True (toOsPath tmpDir)
	writeFile testFile "test file contents"

cleanup :: IO ()
cleanup = removeDirectoryRecursive (toOsPath tmpDir)

run :: [TestCase] -> IO ()
run ts = do
	setup
	config <- runTests ts
	writeSysConfig config
	writeVersion =<< getVersion
	cleanup
