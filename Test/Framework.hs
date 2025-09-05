{- git-annex test suite framework
 -
 - Copyright 2010-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, CPP #-}

module Test.Framework where

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.Options
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.Ingredients.ConsoleReporter
import qualified Test.Tasty.Patterns.Types as TP
import Options.Applicative.Types
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import System.Environment (getArgs)
import System.Console.Concurrent
import System.Console.ANSI
import Data.Time.Clock
import GHC.Conc
import System.IO.Unsafe (unsafePerformIO)
import System.PosixCompat.Files (isSymbolicLink, isRegularFile, fileMode, unionFileModes, ownerWriteMode)

import Common
import Types.Test
import Types.Concurrency
import qualified Utility.RawFilePath as R

import qualified Annex
import qualified Annex.UUID
import qualified Types.RepoVersion
import qualified Backend
import qualified Git.CurrentRepo
import qualified Git.Construct
import qualified Git.Types
import qualified Git.Branch
import qualified Git.Ref
import qualified Types.KeySource
import qualified Types.Backend
import qualified Types
import qualified Remote
import qualified Key
import qualified Types.Key
import qualified Types.Messages
import qualified Config
import qualified Annex.WorkTree
import qualified Annex.Link
import qualified Annex.Path
import qualified Annex.Action
import qualified Annex.AdjustedBranch
import qualified Annex.Init
import qualified Utility.Process
import qualified Utility.Process.Transcript
import qualified Utility.Env
import qualified Utility.Env.Set
import qualified Utility.Exception
import qualified Utility.ThreadScheduler
import qualified Utility.Tmp.Dir
import qualified Utility.Metered
import qualified Utility.HumanTime
import qualified Command.Uninit
import qualified Utility.OsString as OS
#ifndef mingw32_HOST_OS
import qualified Utility.Gpg
#endif

-- Run a process. The output and stderr is captured, and is only
-- displayed if the process does not return the expected value.
--
-- In debug mode, the output is allowed to pass through.
-- So the output does not get checked in debug mode.
testProcess :: String -> [String] -> Maybe [(String, String)] -> (Bool -> Bool) -> (String -> Bool) -> String -> Assertion
testProcess command params environ expectedret expectedtranscript faildesc =
	void $ testProcess' command params environ expectedret expectedtranscript faildesc

testProcess' :: String -> [String] -> Maybe [(String, String)] -> (Bool -> Bool) -> (String -> Bool) -> String -> IO String
testProcess' command params environ expectedret expectedtranscript faildesc = do
	let p = (proc command params) { env = environ }
	debug <- testDebug . testOptions <$> getTestMode
	if debug
		then do
			ret <- withCreateProcess p $ \_ _ _ pid ->
				waitForProcess pid
			(expectedret (ret == ExitSuccess)) @? (faildesc ++ " failed with unexpected exit code")
			return ""
		else do
			(transcript, ret) <- Utility.Process.Transcript.processTranscript' p Nothing
			(expectedret ret) @? (faildesc ++ " failed with unexpected exit code (transcript follows)\n" ++ transcript)
			(expectedtranscript transcript) @? (faildesc ++ " failed with unexpected output (transcript follows)\n" ++ transcript)
			return transcript

-- Run git. (Do not use to run git-annex as the one being tested
-- may not be in path.)
git :: String -> [String] -> String -> Assertion
git command params = testProcess "git" (command:params) Nothing (== True) (const True)

-- For when git is expected to fail.
git_shouldfail :: String -> [String] -> String -> Assertion
git_shouldfail command params = testProcess "git" (command:params) Nothing (== False) (const True)

-- Run git-annex.
git_annex :: String -> [String] -> String -> Assertion
git_annex command params faildesc = git_annex' command params Nothing faildesc

-- Runs git-annex with some environment.
git_annex' :: String -> [String] -> Maybe [(String, String)] -> String -> Assertion
git_annex' = git_annex'' (== True) (const True)

-- For when git-annex is expected to fail.
git_annex_shouldfail :: String -> [String] -> String -> Assertion
git_annex_shouldfail command params faildesc = git_annex_shouldfail' command params Nothing faildesc

git_annex_shouldfail' :: String -> [String] -> Maybe [(String, String)] -> String -> Assertion
git_annex_shouldfail' = git_annex'' (== False) (const True)

git_annex'' :: (Bool -> Bool) -> (String -> Bool) -> String -> [String] -> Maybe [(String, String)] -> String -> Assertion
git_annex'' expectedret expectedtranscript command params environ faildesc = do
	pp <- Annex.Path.programPath
	debug <- testDebug . testOptions <$> getTestMode
	let params' = if debug
		then "--debug":params
		else params
	testProcess (fromOsPath pp) (command:params') environ
		expectedret expectedtranscript faildesc

{- Runs git-annex and returns its standard output. -}
git_annex_output :: String -> [String] -> IO String
git_annex_output command params = do
	pp <- Annex.Path.programPath
	Utility.Process.readProcess (fromOsPath pp) (command:params)

git_annex_expectoutput :: String -> [String] -> [String] -> Assertion
git_annex_expectoutput command params expected = do
	got <- lines <$> git_annex_output command params
	got == expected @? ("unexpected value running " ++ command ++ " " ++ show params ++ " -- got: " ++ show got ++ " expected: " ++ show expected)

-- Runs an action in the current annex. Note that shutdown actions
-- are not run; this should only be used for actions that query state.
annexeval :: Types.Annex a -> IO a
annexeval a = do
	s <- Annex.new =<< Git.CurrentRepo.get
	Annex.eval s $ do
		Annex.setOutput Types.Messages.QuietOutput
		a `finally` Annex.Action.stopCoProcesses

innewrepo :: IO () -> IO ()
innewrepo a = withgitrepo $ \r -> intopdir r a

inmainrepo :: IO a -> IO a
inmainrepo a = do
	d <- mainrepodir
	intopdir d a

with_ssh_origin :: (Assertion -> Assertion) -> (Assertion -> Assertion)
with_ssh_origin cloner a = cloner $ do
	let k = Git.Types.ConfigKey (encodeBS config)
	let v = Git.Types.ConfigValue (toRawFilePath "/dev/null")
	origindir <- absPath . Git.Types.fromConfigValue
		=<< annexeval (Config.getConfig k v)
	let originurl = "localhost:" ++ fromOsPath origindir
	git "config" [config, originurl] "git config failed"
	a
  where
	config = "remote.origin.url"

intmpclonerepo :: Assertion -> Assertion
intmpclonerepo a = withtmpclonerepo $ \r -> intopdir r a

checkRepo :: Types.Annex a -> FilePath -> IO a
checkRepo getval d = do
	s <- Annex.new =<< Git.Construct.fromPath (toOsPath d)
	Annex.eval s $
		getval `finally` Annex.Action.stopCoProcesses

intmpbareclonerepo :: Assertion -> Assertion
intmpbareclonerepo a = withtmpclonerepo' (newCloneRepoConfig { bareClone = True } ) $
	\r -> intopdir r a

intmpsharedclonerepo :: Assertion -> Assertion
intmpsharedclonerepo a = withtmpclonerepo' (newCloneRepoConfig { sharedClone = True } ) $
	\r -> intopdir r a

withtmpclonerepo :: (FilePath -> Assertion) -> Assertion
withtmpclonerepo = withtmpclonerepo' newCloneRepoConfig

withtmpclonerepo' :: CloneRepoConfig -> (FilePath -> Assertion) -> Assertion
withtmpclonerepo' cfg a = do
	dir <- tmprepodir
	maindir <- mainrepodir
	clone <- clonerepo maindir dir cfg
	r <- tryNonAsync (a clone)
	case r of
		Right () -> return ()
		Left e -> do
			whenM (keepFailuresOption . testOptions <$> getTestMode) $ do
				topdir <- Utility.Env.getEnvDefault "TOPDIR" ""
				putStrLn $ "** Preserving repo for failure analysis in " ++ 
					fromOsPath (toOsPath topdir </> toOsPath clone)
			throwM e

disconnectOrigin :: Assertion
disconnectOrigin = git "remote" ["rm", "origin"] "remote rm"

withgitrepo :: (FilePath -> Assertion) -> Assertion
withgitrepo a = do
	maindir <- mainrepodir
	bracket (setuprepo maindir) return a

intopdir :: FilePath -> IO a -> IO a
intopdir dir a = do
	topdir <- Utility.Env.getEnvDefault "TOPDIR" (error "TOPDIR not set")
	inpath (topdir ++ "/" ++ dir) a

inpath :: FilePath -> IO a -> IO a
inpath path a = do
	currdir <- getCurrentDirectory
	-- Assertion failures throw non-IO errors; catch
	-- any type of error and change back to currdir before
	-- rethrowing.
	r <- bracket_
		(setCurrentDirectory (toOsPath path))
		(setCurrentDirectory currdir)
		(tryNonAsync a)
	case r of
		Right v -> return v
		Left e -> throwM e

setuprepo :: FilePath -> IO FilePath
setuprepo dir = do
	cleanup dir
	git "init" ["-q", dir] "git init"
	configrepo dir
	return dir

data CloneRepoConfig = CloneRepoConfig
	{ bareClone :: Bool
	, sharedClone :: Bool
	}

newCloneRepoConfig :: CloneRepoConfig
newCloneRepoConfig = CloneRepoConfig
	{ bareClone = False
	, sharedClone = False
	}

-- clones are always done as local clones; we cannot test ssh clones
clonerepo :: FilePath -> FilePath -> CloneRepoConfig -> IO FilePath
clonerepo old new cfg = do
	cleanup new
	let cloneparams = catMaybes
		[ Just "-q"
		, if bareClone cfg then Just "--bare" else Nothing
		, if sharedClone cfg then Just "--shared" else Nothing
		, Just old
		, Just new
		]
	git "clone" cloneparams "git clone"
	configrepo new
	intopdir new $ do
		ver <- annexVersion <$> getTestMode
		git_annex "init" 
			[ "-q"
			, new, "--version"
			, show (Types.RepoVersion.fromRepoVersion ver)
			]
			"git annex init"
	unless (bareClone cfg) $
		intopdir new $
			setupTestMode
	return new

configrepo :: FilePath -> IO ()
configrepo dir = intopdir dir $ do
	-- ensure git is set up to let commits happen
	git "config" ["user.name", "Test User"]
		"git config"
	git "config" ["user.email", "test@example.com"]
		"git config"
	-- avoid signed commits by test suite
	git "config" ["commit.gpgsign", "false"]
		"git config"
	-- tell git-annex to not annex the ingitfile
	git "config" ["annex.largefiles", "exclude=" ++ ingitfile]
		"git config annex.largefiles"
	-- set any additional git configs the user wants to test with
	gc <- testGitConfig . testOptions <$> getTestMode
	forM_ gc $ \case
		(Git.Types.ConfigKey k, Git.Types.ConfigValue v) -> 
			git "config" [decodeBS k, decodeBS v]
				"git config from test options"
		(Git.Types.ConfigKey _, Git.Types.NoConfigValue) -> noop

ensuredir :: FilePath -> IO ()
ensuredir d = do
	let d' = toOsPath d
	e <- doesDirectoryExist d'
	unless e $
		createDirectory d'

{- This is the only place in the test suite that can use setEnv.
 - Using it elsewhere can conflict with tasty's use of getEnv, which can
 - happen concurrently with a test case running, and would be a problem
 - since setEnv is not thread safe. This is run before tasty. -}
setTestEnv :: IO a -> IO a
setTestEnv a = Utility.Tmp.Dir.withTmpDir (literalOsPath "testhome") $ \tmphome -> do
	tmphomeabs <- fromOsPath <$> absPath tmphome
	{- Prevent global git configs from affecting the test suite. -}
	Utility.Env.Set.setEnv "HOME" tmphomeabs True
	Utility.Env.Set.setEnv "XDG_CONFIG_HOME" tmphomeabs True
	Utility.Env.Set.setEnv "GIT_CONFIG_NOSYSTEM" "1" True
	
	-- Ensure that the same git-annex binary that is running
	-- git-annex test is at the front of the PATH.
	pp <- Annex.Path.programPath
	p <- Utility.Env.getEnvDefault "PATH" ""
	let p' = fromOsPath $
		takeDirectory pp <> OS.singleton searchPathSeparator <> toOsPath p
	Utility.Env.Set.setEnv "PATH" p' True
	
	-- Avoid git complaining if it cannot determine the user's
	-- email address, or exploding if it doesn't know the user's name.
	Utility.Env.Set.setEnv "GIT_AUTHOR_EMAIL" "test@example.com" True
	Utility.Env.Set.setEnv "GIT_AUTHOR_NAME" "git-annex test" True
	Utility.Env.Set.setEnv "GIT_COMMITTER_EMAIL" "test@example.com" True
	Utility.Env.Set.setEnv "GIT_COMMITTER_NAME" "git-annex test" True
	-- force gpg into batch mode for the tests
	Utility.Env.Set.setEnv "GPG_BATCH" "1" True
	-- Make git and git-annex access ssh remotes on the local
	-- filesystem, without using ssh at all.
	Utility.Env.Set.setEnv "GIT_SSH_COMMAND" "git-annex test --fakessh --" True
	Utility.Env.Set.setEnv "GIT_ANNEX_USE_GIT_SSH" "1" True

	-- Record top directory.
	currdir <- getCurrentDirectory
	Utility.Env.Set.setEnv "TOPDIR" (fromOsPath currdir) True
	
	a

removeDirectoryForCleanup :: FilePath -> IO ()
removeDirectoryForCleanup = removePathForcibly . toOsPath

cleanup :: FilePath -> IO ()
cleanup dir = whenM (doesDirectoryExist (toOsPath dir)) $ do
	Command.Uninit.prepareRemoveAnnexDir' (toOsPath dir)
	-- This can fail if files in the directory are still open by a
	-- subprocess.
	void $ tryIO $ removeDirectoryForCleanup dir

finalCleanup :: IO ()
finalCleanup = whenM (doesDirectoryExist (toOsPath tmpdir)) $ do
	Command.Uninit.prepareRemoveAnnexDir' (toOsPath tmpdir)
	catchIO (removeDirectoryForCleanup tmpdir) $ \e -> do
		print e
		putStrLn "sleeping 10 seconds and will retry directory cleanup"
		Utility.ThreadScheduler.threadDelaySeconds $
			Utility.ThreadScheduler.Seconds 10
		whenM (doesDirectoryExist (toOsPath tmpdir)) $
			removeDirectoryForCleanup tmpdir

checklink :: FilePath -> Assertion
checklink f = ifM (annexeval Config.crippledFileSystem)
	( (isJust <$> annexeval (Annex.Link.getAnnexLinkTarget (toOsPath f)))
		@? f ++ " is not a (crippled) symlink"
	, do
		s <- R.getSymbolicLinkStatus (toRawFilePath f)
		isSymbolicLink s @? f ++ " is not a symlink"
	)

checkregularfile :: FilePath -> Assertion
checkregularfile f = do
	s <- R.getSymbolicLinkStatus (toRawFilePath f)
	isRegularFile s @? f ++ " is not a normal file"
	return ()

checkdoesnotexist :: FilePath -> Assertion
checkdoesnotexist f = 
	(either (const True) (const False) <$> Utility.Exception.tryIO (R.getSymbolicLinkStatus (toRawFilePath f)))
		@? f ++ " exists unexpectedly"

checkexists :: FilePath -> Assertion
checkexists f = 
	(either (const False) (const True) <$> Utility.Exception.tryIO (R.getSymbolicLinkStatus (toRawFilePath f)))
		@? f ++ " does not exist"

checkcontent :: FilePath -> Assertion
checkcontent f = do
	c <- Utility.Exception.catchDefaultIO "could not read file" $ readFileString (toOsPath f)
	assertEqual ("checkcontent " ++ f) (content f) c

checkunwritable :: FilePath -> Assertion
checkunwritable f = do
	-- Look at permissions bits rather than trying to write or
	-- using fileAccess because if run as root, any file can be
	-- modified despite permissions.
	s <- R.getFileStatus (toRawFilePath f)
	let mode = fileMode s
	when (mode == mode `unionFileModes` ownerWriteMode) $
		assertFailure $ "able to modify annexed file's " ++ f ++ " content"

checkwritable :: FilePath -> Assertion
checkwritable f = do
	s <- R.getFileStatus (toRawFilePath f)
	let mode = fileMode s
	unless (mode == mode `unionFileModes` ownerWriteMode) $
		assertFailure $ "unable to modify " ++ f

checkdangling :: FilePath -> Assertion
checkdangling f = ifM (annexeval Config.crippledFileSystem)
	( return () -- probably no real symlinks to test
	, do
		r <- tryIO $ readFileString (toOsPath f)
		case r of
			Left _ -> return () -- expected; dangling link
			Right _ -> assertFailure $ f ++ " was not a dangling link as expected"
	)

checklocationlog :: FilePath -> Bool -> Assertion
checklocationlog f expected = do
	thisuuid <- annexeval Annex.UUID.getUUID
	r <- annexeval $ Annex.WorkTree.lookupKey (toOsPath f)
	case r of
		Just k -> do
			uuids <- annexeval $ Remote.keyLocations k
			assertEqual ("bad content in location log for " ++ f ++ " key " ++ Key.serializeKey k ++ " uuid " ++ show thisuuid)
				expected (thisuuid `elem` uuids)
		_ -> assertFailure $ f ++ " failed to look up key"

checkbackend :: FilePath -> Types.Backend -> Assertion
checkbackend file expected = do
	let file' = toOsPath file
	b <- annexeval $ maybe (return Nothing) (Backend.getBackend file')
		=<< Annex.WorkTree.lookupKey file'
	assertEqual ("backend for " ++ file) (Just expected) b

checkispointerfile :: FilePath -> Assertion
checkispointerfile f = unlessM (isJust <$> Annex.Link.isPointerFile (toOsPath f)) $
	assertFailure $ f ++ " is not a pointer file"

inlocationlog :: FilePath -> Assertion
inlocationlog f = checklocationlog f True

notinlocationlog :: FilePath -> Assertion
notinlocationlog f = checklocationlog f False

runchecks :: [FilePath -> Assertion] -> FilePath -> Assertion
runchecks [] _ = return ()
runchecks (a:as) f = do
	a f
	runchecks as f

annexed_notpresent :: FilePath -> Assertion
annexed_notpresent f = ifM (hasUnlockedFiles <$> getTestMode)
	( annexed_notpresent_unlocked f
	, annexed_notpresent_locked f
	)

annexed_notpresent_locked :: FilePath -> Assertion
annexed_notpresent_locked = runchecks [checklink, checkdangling, notinlocationlog]

annexed_notpresent_unlocked :: FilePath -> Assertion
annexed_notpresent_unlocked = runchecks [checkregularfile, checkispointerfile, notinlocationlog]

annexed_present :: FilePath -> Assertion
annexed_present f = ifM (hasUnlockedFiles <$> getTestMode)
	( annexed_present_unlocked f
	, annexed_present_locked f
	)

annexed_present_locked :: FilePath -> Assertion
annexed_present_locked f = ifM (annexeval Config.crippledFileSystem)
	( runchecks [checklink, inlocationlog] f
	, runchecks [checklink, checkcontent, checkunwritable, inlocationlog] f
	)

annexed_present_unlocked :: FilePath -> Assertion
annexed_present_unlocked = runchecks
	[checkregularfile, checkcontent, checkwritable, inlocationlog]
	
annexed_present_imported :: FilePath -> Assertion
annexed_present_imported f = ifM (annexeval Config.crippledFileSystem)
	( annexed_present_unlocked f
	, ifM (adjustedUnlockedBranch <$> getTestMode) 
		( annexed_present_unlocked f
		, annexed_present_locked f
		)
	)

annexed_notpresent_imported :: FilePath -> Assertion
annexed_notpresent_imported f = ifM (annexeval Config.crippledFileSystem)
	( annexed_notpresent_unlocked f
	, ifM (adjustedUnlockedBranch <$> getTestMode)
		( annexed_notpresent_unlocked f
		, annexed_notpresent_locked f
		)
	)

unannexed :: FilePath -> Assertion
unannexed = runchecks [checkregularfile, checkcontent, checkwritable]

-- Check that a file is unannexed, but also that what's recorded in git
-- is not an annexed file.
unannexed_in_git :: FilePath -> Assertion
unannexed_in_git f = do
	unannexed f
	r <- annexeval $ Annex.WorkTree.lookupKey (toOsPath f)
	case r of
		Just _k -> assertFailure $ f ++ " is annexed in git"
		Nothing -> return ()

add_annex :: FilePath -> String -> Assertion
add_annex f faildesc = ifM (unlockedFiles <$> getTestMode)
	( git "add" [f] faildesc
	, git_annex "add" [f] faildesc
	)

#ifndef mingw32_HOST_OS
test_with_gpg :: (Utility.Gpg.GpgCmd -> [(String, String)] -> Assertion) -> Assertion
test_with_gpg a = Utility.Tmp.Dir.withTmpDir (literalOsPath "gpgtmp") $ \gpgtmp -> do
	-- Use the system temp directory as gpg temp directory because 
	-- it needs to be able to store the agent socket there,
	-- which can be problematic when testing some filesystems.
	absgpgtmp <- absPath gpgtmp
	res <- go absgpgtmp
	-- gpg may still be running and would prevent
	-- removeDirectoryRecursive from succeeding, so
	-- force removal of the temp directory.
	liftIO $ removeDirectoryForCleanup (fromOsPath gpgtmp)
	return res
  where
	gpgcmd = Utility.Gpg.mkGpgCmd Nothing
	go absgpgtmp = do
		-- Since gpg uses a unix socket, which is limited to a
		-- short path, use whichever is shorter of absolute
		-- or relative path.
		relgpgtmp <- relPathCwdToFile absgpgtmp
		let gpgtmp = if OS.length relgpgtmp < OS.length absgpgtmp
			then relgpgtmp 
			else absgpgtmp
		void $ Utility.Gpg.testHarness (fromOsPath gpgtmp) gpgcmd $ \environ ->
			a gpgcmd environ
#endif

data TestMode = TestMode
	{ unlockedFiles :: Bool
	, adjustedUnlockedBranch :: Bool
	, annexVersion :: Types.RepoVersion.RepoVersion
	, testOptions :: TestOptions
	}

testMode :: TestOptions -> Types.RepoVersion.RepoVersion -> TestMode
testMode opts v = TestMode
	{ unlockedFiles = False
	, adjustedUnlockedBranch = False
	, annexVersion = v
	, testOptions = opts
	}

hasUnlockedFiles :: TestMode -> Bool
hasUnlockedFiles m = unlockedFiles m || adjustedUnlockedBranch m

onAdjustedBranch :: IO Bool
onAdjustedBranch = adjustedUnlockedBranch <$> getTestMode

withTestMode :: TestMode -> TestTree -> TestTree
withTestMode testmode = withResource prepare release . const
  where
	prepare = setTestMode testmode
	release _ = noop

{- The current test mode is stored here while a test is running.
 -
 - Only one test can be running at a time by a process; running a
 - test also involves chdir into a test repository.
 -}
{-# NOINLINE currentTestMode #-}
currentTestMode :: TMVar TestMode
currentTestMode = unsafePerformIO newEmptyTMVarIO

currentMainRepoDir :: TMVar FilePath
currentMainRepoDir = unsafePerformIO newEmptyTMVarIO

setTestMode :: TestMode -> IO ()
setTestMode testmode = do
	atomically $ do
		_ <- tryTakeTMVar currentTestMode
		putTMVar currentTestMode testmode
	setmainrepodir =<< newmainrepodir

getTestMode :: IO TestMode
getTestMode = atomically (tryReadTMVar currentTestMode) >>= \case
	Just tm -> return tm
	Nothing -> error "getTestMode without setTestMode"

setupTestMode :: IO ()
setupTestMode = do
	testmode <- getTestMode
	when (adjustedUnlockedBranch testmode) $ do
		git "commit" ["--allow-empty", "-m", "empty"] "git commit failed"
		git_annex "adjust" ["--unlock"] "git annex adjust failed"

tmpdir :: String
tmpdir = ".t"

setmainrepodir :: FilePath -> IO ()
setmainrepodir mrd = atomically $ do
	_ <- tryTakeTMVar currentMainRepoDir
	putTMVar currentMainRepoDir mrd

mainrepodir :: IO FilePath
mainrepodir = atomically (tryReadTMVar currentMainRepoDir) >>= \case
	Just tm -> return tm
	Nothing -> error "mainrepodir without setmainrepodir"

newmainrepodir :: IO FilePath
newmainrepodir = go (0 :: Int)
  where
	go n = do
		let d = "main" ++ show n
		ifM (doesDirectoryExist (toOsPath d))
			( go $ n + 1
			, do
				createDirectory (toOsPath d)
				return d
			)

tmprepodir :: IO FilePath
tmprepodir = go (0 :: Int)
  where
	go n = do
		let d = "tmprepo" ++ show n
		ifM (doesDirectoryExist (toOsPath d))
			( go $ n + 1
			, return d
			)

annexedfile :: String
annexedfile = "foo"

annexedfiledup :: String
annexedfiledup = "foodup"

wormannexedfile :: String
wormannexedfile = "apple"

sha1annexedfile :: String
sha1annexedfile = "sha1foo"

sha1annexedfiledup :: String
sha1annexedfiledup = "sha1foodup"

ingitfile :: String
ingitfile = "bar.c"

content :: FilePath -> String		
content f
	| f == annexedfile = "annexed file content"
	| f == ingitfile = "normal file content"
	| f == sha1annexedfile ="sha1 annexed file content"
	| f == annexedfiledup = content annexedfile
	| f == sha1annexedfiledup = content sha1annexedfile
	| f == wormannexedfile = "worm annexed file content"
	| "import" `isPrefixOf` f = "imported content"
	| otherwise = "unknown file " ++ f

-- Writes new content to a file, and makes sure that it has a different
-- mtime than it did before
writecontent :: FilePath -> String -> IO ()
writecontent f c = go (10000000 :: Integer)
  where
	go ticsleft = do
		let f' = toOsPath f
		oldmtime <- catchMaybeIO $ getModificationTime f'
		writeFileString f' c
		newmtime <- getModificationTime f'
		if Just newmtime == oldmtime
			then do
				threadDelay 100000
				let ticsleft' = ticsleft - 100000
				if ticsleft' > 0
					then go ticsleft'
					else do
						hPutStrLn stderr "file mtimes do not seem to be changing (tried for 10 seconds)"
						hFlush stderr
						return ()
			else return ()

changecontent :: FilePath -> IO ()
changecontent f = writecontent f $ changedcontent f

changedcontent :: FilePath -> String
changedcontent f = content f ++ " (modified)"

backendSHA1 :: Types.Backend
backendSHA1 = backend_ "SHA1"

backendSHA256 :: Types.Backend
backendSHA256 = backend_ "SHA256"

backendSHA256E :: Types.Backend
backendSHA256E = backend_ "SHA256E"

backendWORM :: Types.Backend
backendWORM = backend_ "WORM"

backend_ :: String -> Types.Backend
backend_ = Backend.lookupBuiltinBackendVariety . Types.Key.parseKeyVariety . encodeBS

getKey :: Types.Backend -> FilePath -> IO Types.Key
getKey b f = case Types.Backend.genKey b of
	Just a -> annexeval $ a ks Utility.Metered.nullMeterUpdate
	Nothing -> error "internal"
  where
	ks = Types.KeySource.KeySource
		{ Types.KeySource.keyFilename = toOsPath f
		, Types.KeySource.contentLocation = toOsPath f
		, Types.KeySource.inodeCache = Nothing
		}

{- Get the name of the original branch, eg the current branch, or
 - if in an adjusted branch, the parent branch. -}
origBranch :: Types.Annex String
origBranch = maybe "foo"
	(Git.Types.fromRef . Git.Ref.base . Annex.AdjustedBranch.fromAdjustedBranch)
	<$> Annex.inRepo Git.Branch.current

{- Set up repos as remotes of each other. -}
pair :: FilePath -> FilePath -> Assertion
pair r1 r2 = forM_ [r1, r2] $ \r -> intopdir r $ do
	when (r /= r1) $
		git "remote" ["add", "r1", "../" ++ r1] "remote add"
	when (r /= r2) $
		git "remote" ["add", "r2", "../" ++ r2] "remote add"


{- Runs a query in the current repository, but first makes the repository
 - read-only. The write bit is added back at the end, so when possible, 
 - include multiple tests within a single call for efficiency. -}
readonly_query :: Assertion -> Assertion
readonly_query = bracket_ (make_readonly ".") (make_writeable ".")
	
{- Not guaranteed to do anything: 
 - chmod may fail, or not be available, or the filesystem not support
 - permissions. -}
make_readonly :: FilePath -> IO ()
make_readonly d = void $
	Utility.Process.Transcript.processTranscript
		"chmod" ["-R", "-w", d] Nothing

{- The write bit is added back for the current user, but not for other
 - users, even though make_readonly removes any other user's write bits. -}
make_writeable :: FilePath -> IO ()
make_writeable d = void $
	Utility.Process.Transcript.processTranscript
		"chmod" ["-R", "u+w", d] Nothing

runFakeSsh :: [String] -> IO ()
runFakeSsh ("-n":ps) = runFakeSsh ps
runFakeSsh (_host:cmd:[]) =
	withCreateProcess (shell cmd) $
		\_ _ _ pid -> exitWith =<< waitForProcess pid
runFakeSsh ps = error $ "fake ssh option parse error: " ++ show ps

{- Tests each TestTree in parallel, and exits with success/failure.
 -
 - Tasty supports parallel tests, but this does not use it, because
 - many tests need to be run in test repos, and chdir would not be 
 - thread safe. Instead, this starts one child process for each TestTree.
 -
 - An added benefit of using child processes is that any files they may
 - leave open are closed before finalCleanup is run at the end. This
 - prevents some failures to clean up after the test suite.
 -}
parallelTestRunner :: TestOptions -> (Int -> Bool -> TestOptions -> [TestTree]) -> IO ()
parallelTestRunner opts mkts = do
	numjobs <- case concurrentJobs opts of
		Just NonConcurrent -> pure 1
		Just (Concurrent n) -> pure n
		Just ConcurrentPerCpu -> getNumProcessors
		Nothing -> getNumProcessors
	parallelTestRunner' numjobs opts mkts

parallelTestRunner' :: Int -> TestOptions -> (Int -> Bool -> TestOptions -> [TestTree]) -> IO ()
parallelTestRunner' numjobs opts mkts
	| fakeSsh opts = runFakeSsh (internalData opts)
	| otherwise = go =<< Utility.Env.getEnv subenv
  where
	subenv = "GIT_ANNEX_TEST_SUBPROCESS"

	-- Make more parts than there are jobs, because some parts
	-- are larger, and this allows the smaller parts to be packed
	-- in more efficiently, speeding up the test suite overall.
	--
	-- When there is a pattern, splitting into parts will cause
	-- extra work.
	numparts = if haspattern
		then 1
		else numjobs * 2

	worker rs nvar a = do
		(n, m) <- atomically $ do
			(n, m) <- readTVar nvar
			writeTVar nvar (n+1, m)
			return (n, m)
		if n > m
			then return rs
			else do
				r <- a n
				worker (r:rs) nvar a
	
	summarizeresults a = do
		starttime <- getCurrentTime
		(numts, exitcodes) <- a
		duration <- Utility.HumanTime.durationSince starttime
		case nub (filter (/= ExitSuccess) (concat exitcodes)) of
			[] -> do
				putStrLn ""
				putStrLn $ "All tests succeeded. (Ran "
					++ show numts
					++ " test groups in " 
					++ Utility.HumanTime.fromDuration duration
					++ ")"
				exitSuccess
			[ExitFailure 1] -> do
				putStrLn "  (Failures above could be due to a bug in git-annex, or an incompatibility"
				putStrLn "   with utilities, such as git, installed on this system.)"
				exitFailure
			_ -> do
				putStrLn $ "  Test subprocesses exited with unexpected exit codes: " ++ show (concat exitcodes)
				exitFailure

	go Nothing = summarizeresults $ withConcurrentOutput $ do
		ensuredir tmpdir
		crippledfilesystem <- fromMaybe False . fst 
			<$> Annex.Init.probeCrippledFileSystem'
				(toOsPath tmpdir)
				Nothing Nothing False
		let ts = mkts numparts crippledfilesystem opts
		let warnings = fst (tastyParser ts)
		unless (null warnings) $ do
			hPutStrLn stderr "warnings from tasty:"
			mapM_ (hPutStrLn stderr) warnings
		environ <- Utility.Env.getEnvironment
		args <- getArgs
		pp <- fromOsPath <$> Annex.Path.programPath
		termcolor <- hSupportsANSIColor stdout
		let ps = if useColor (lookupOption tastyopts) termcolor
			then "--color=always":args
			else "--color=never":args
		let runone n = do
			let subdir = fromOsPath $ toOsPath tmpdir </> toOsPath (show n)
			ensuredir subdir
			let p = (proc pp ps)
				{ env = Just ((subenv, show (n, crippledfilesystem)):environ)
				, cwd = Just subdir
				}
			(_, _, _, pid) <- createProcessConcurrent p
			waitForProcess pid
		nvar <- newTVarIO (1, length ts)
		exitcodes <- forConcurrently [1..numjobs] $ \_ -> 
			worker [] nvar runone
		unless (keepFailuresOption opts) finalCleanup
		return (length ts, exitcodes)
	go (Just subenvval) = case readish subenvval of
		Nothing -> error ("Bad " ++ subenv)
		Just (n, crippledfilesystem) -> setTestEnv $ do
			let ts = mkts numparts crippledfilesystem opts
			let t = topLevelTestGroup [ ts !! (n - 1) ]
			case tryIngredients ingredients tastyopts t of
				Nothing -> error "No tests found!?"
				Just act -> ifM act
					( exitSuccess
					, exitFailure
					)
	
	(haspattern, tastyopts) = case lookupOption (tastyOptionSet opts) of
		-- Work around limitation of tasty; when tests to run
		-- are limited to a pattern, it does not include their
		-- dependencies. So, add another pattern including the
		-- init tests, which are a dependency of most tests.
		TestPattern (Just p) -> 
			(True, setOption (TestPattern (Just (TP.Or p (TP.ERE initTestsName))))
				(tastyOptionSet opts))
		TestPattern Nothing ->
			(False, tastyOptionSet opts)

topLevelTestGroup :: [TestTree] -> TestTree
topLevelTestGroup = testGroup "Tests"

initTestsName :: String
initTestsName = "Init Tests"

tastyParser :: [TestTree] -> ([String], Parser Test.Tasty.Options.OptionSet)
tastyParser ts = suiteOptionParser ingredients (topLevelTestGroup ts)

ingredients :: [Ingredient]
ingredients =
	[ listingTests
	, rerunningTests [consoleTestReporter]
	]

