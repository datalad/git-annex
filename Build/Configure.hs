{- Checks system configuration and generates SysConfig.hs. -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Build.Configure where

import Control.Applicative
import Control.Monad.IfElse
import Control.Monad

import Build.TestConfig
import Build.Version
import Utility.PartialPrelude
import Utility.Process
import Utility.SafeCommand
import Utility.ExternalSHA
import Utility.Env
import Utility.Exception
import qualified Git.Version
import Utility.DottedVersion
import Utility.Directory

tests :: [TestCase]
tests =
	[ TestCase "version" (Config "packageversion" . StringConfig <$> getVersion)
	, TestCase "UPGRADE_LOCATION" getUpgradeLocation
	, TestCase "git" $ requireCmd "git" "git --version >/dev/null"
	, TestCase "git version" getGitVersion
	, testCp "cp_a" "-a"
	, testCp "cp_p" "-p"
	, testCp "cp_preserve_timestamps" "--preserve=timestamps"
	, testCp "cp_reflink_auto" "--reflink=auto"
	, TestCase "xargs -0" $ requireCmd "xargs_0" "xargs -0 </dev/null"
	, TestCase "rsync" $ requireCmd "rsync" "rsync --version >/dev/null"
	, TestCase "curl" $ testCmd "curl" "curl --version >/dev/null"
	, TestCase "wget" $ testCmd "wget" "wget --version >/dev/null"
	, TestCase "wget unclutter options" checkWgetUnclutter
	, TestCase "bup" $ testCmd "bup" "bup --version >/dev/null"
	, TestCase "nice" $ testCmd "nice" "nice true >/dev/null"
	, TestCase "ionice" $ testCmd "ionice" "ionice -c3 true >/dev/null"
	, TestCase "nocache" $ testCmd "nocache" "nocache true >/dev/null"
	, TestCase "gpg" $ maybeSelectCmd "gpg"
		[ ("gpg", "--version >/dev/null")
		, ("gpg2", "--version >/dev/null") ]
	, TestCase "lsof" $ findCmdPath "lsof" "lsof"
	, TestCase "git-remote-gcrypt" $ findCmdPath "gcrypt" "git-remote-gcrypt"
	, TestCase "ssh connection caching" getSshConnectionCaching
	] ++ shaTestCases
	[ (1, "da39a3ee5e6b4b0d3255bfef95601890afd80709")
	, (256, "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
	, (512, "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e")
	, (224, "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f")
	, (384, "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b")
	]

{- shaNsum are the program names used by coreutils. Some systems
 - install these with 'g' prefixes.
 -
 - On some systems, shaN is used instead, but on other
 - systems, it might be "hashalot", which does not produce
 - usable checksums. Only accept programs that produce
 - known-good hashes when run on files. -}
shaTestCases :: [(Int, String)] -> [TestCase]
shaTestCases l = map make l
  where
	make (n, knowngood) = TestCase key $ 
		Config key . MaybeStringConfig <$> search (shacmds n)
	  where
		key = "sha" ++ show n
		search [] = return Nothing
		search (c:cmds) = do
			sha <- externalSHA c n "/dev/null"
			if sha == Right knowngood
				then return $ Just c
				else search cmds
	
	shacmds n = concatMap (\x -> [x, 'g':x]) $
		map (\x -> "sha" ++ show n ++ x) ["sum", ""]

tmpDir :: String
tmpDir = "tmp"

testFile :: String
testFile = tmpDir ++ "/testfile"

testCp :: ConfigKey -> String -> TestCase
testCp k option = TestCase cmd $ testCmd k cmdline
  where
	cmd = "cp " ++ option
	cmdline = cmd ++ " " ++ testFile ++ " " ++ testFile ++ ".new"

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
		let oldestallowed = Git.Version.normalize "1.7.1.0"
		when (v < oldestallowed) $
			error $ "installed git version " ++ show v ++ " is too old! (Need " ++ show oldestallowed ++ " or newer)"
		return $ Config "gitversion" $ StringConfig $ show v

checkWgetUnclutter :: Test
checkWgetUnclutter = Config "wgetunclutter" . BoolConfig
	. maybe False (>= normalize "1.16")
	<$> getWgetVersion 

getWgetVersion :: IO (Maybe DottedVersion)
getWgetVersion = catchDefaultIO Nothing $
	extract <$> readProcess "wget" ["--version"]
  where
	extract s = case lines s of
		[] -> Nothing
		(l:_) -> normalize <$> headMaybe (drop 2 $ words l)

getSshConnectionCaching :: Test
getSshConnectionCaching = Config "sshconnectioncaching" . BoolConfig <$>
	boolSystem "sh" [Param "-c", Param "ssh -o ControlPersist=yes -V >/dev/null 2>/dev/null"]

setup :: IO ()
setup = do
	createDirectoryIfMissing True tmpDir
	writeFile testFile "test file contents"

cleanup :: IO ()
cleanup = removeDirectoryRecursive tmpDir

run :: [TestCase] -> IO ()
run ts = do
	setup
	config <- runTests ts
	v <- getEnv "CROSS_COMPILE"
	case v of
		Just "Android" -> writeSysConfig $ androidConfig config
		_ -> writeSysConfig config
	cleanup
	whenM isReleaseBuild $
		cabalSetup "git-annex.cabal"

{- Hard codes some settings to cross-compile for Android. -}
androidConfig :: [Config] -> [Config]
androidConfig c = overrides ++ filter (not . overridden) c
  where
	overrides = 
		[ Config "cp_reflink_auto" $ BoolConfig False
		, Config "curl" $ BoolConfig False
		, Config "sha224" $ MaybeStringConfig Nothing
		, Config "sha384" $ MaybeStringConfig Nothing
		]
	overridden (Config k _) = k `elem` overridekeys
	overridekeys = map (\(Config k _) -> k) overrides
