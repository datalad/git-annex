{- git-annex test suite
 -
 - Copyright 2010-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Test where

import Types.Test
import Types.RepoVersion
import Types.Concurrency
import Test.Framework

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Options.Applicative.Types
import Options.Applicative (switch, long, short, help, internal, maybeReader, option, metavar)

import qualified Data.Map as M
import qualified Data.ByteString.Lazy.UTF8 as BU8
import Control.Concurrent.STM hiding (check)

import Common
import CmdLine.GitAnnex.Options
import qualified Utility.RawFilePath as R

import qualified Utility.ShellEscape
import qualified Annex
import qualified Git.Filename
import qualified Git.Types
import qualified Git.Ref
import qualified Git.LsTree
import qualified Git.FilePath
import qualified Annex.Locations
#ifndef mingw32_HOST_OS
import qualified Types.GitConfig
#endif
import qualified Types.TrustLevel
import qualified Types
import qualified Logs.MapLog
import qualified Logs.Trust
import qualified Logs.Remote
import qualified Logs.Unused
import qualified Logs.Transfer
import qualified Logs.Presence
import qualified Logs.ContentIdentifier
import qualified Logs.PreferredContent
import qualified Types.MetaData
import qualified Remote
import qualified Key
import qualified Config
import qualified Config.Cost
import qualified Crypto
import qualified Database.Keys
import qualified Annex.WorkTree
import qualified Annex.CatFile
import qualified Annex.VectorClock
import qualified Annex.VariantFile
import qualified Annex.View
import qualified Annex.View.ViewedFile
import qualified Logs.View
import qualified Command.TestRemote
import qualified Utility.Path.Tests
import qualified Utility.FileMode
import qualified BuildInfo
import qualified Utility.Format
import qualified Utility.Verifiable
import qualified Utility.Process
import qualified Utility.Misc
import qualified Utility.InodeCache
import qualified Utility.Matcher
import qualified Utility.Hash
import qualified Utility.Scheduled
import qualified Utility.Scheduled.QuickCheck
import qualified Utility.HumanTime
import qualified Utility.Base64
import qualified Utility.Tmp.Dir
import qualified Utility.FileSystemEncoding
import qualified Utility.Aeson
import qualified Utility.CopyFile
import qualified Utility.MoveFile
import qualified Types.Remote
#ifndef mingw32_HOST_OS
import qualified Remote.Helper.Encryptable
import qualified Types.Crypto
import qualified Utility.Gpg
#endif

optParser :: Parser TestOptions
optParser = TestOptions
	<$> snd (tastyParser (tests 1 False True (TestOptions mempty False False Nothing mempty False mempty)))
	<*> switch
		( long "keep-failures"
		<> help "preserve repositories on test failure"
		)
	<*> switch
		( long "fakessh"
		<> internal
		)
	<*> optional (option (maybeReader parseConcurrency)
		( long "jobs"
		<> short 'J'
		<> help "number of concurrent jobs"
		))
	<*> many (option (maybeReader parseconfigvalue)
		( long "test-git-config"
		<> help "run tests with a git config set"
		<> metavar "NAME=VALUE"
		))
	<*> switch
		( long "test-debug"
		<> help "show debug messages for commands run by test suite"
		)
	<*> cmdParams "non-options are for internal use only"
  where
	parseconfigvalue s = case break (== '=') s of
		(_, []) -> Nothing
		(k, v) -> Just 
			( Git.Types.ConfigKey (encodeBS k)
			, Git.Types.ConfigValue (encodeBS (drop 1 v))
			)

runner :: TestOptions -> IO ()
runner opts = parallelTestRunner opts tests

tests :: Int -> Bool -> Bool -> TestOptions -> [TestTree]
tests n crippledfilesystem adjustedbranchok opts = 
	properties 
		: withTestMode remotetestmode testRemotes
		: concatMap mkrepotests testmodes
  where
	testmodes = catMaybes
		[ canadjust ("v10 adjusted unlocked branch", (testMode opts (RepoVersion 10)) { adjustedUnlockedBranch = True })
		, unlesscrippled ("v10 unlocked", (testMode opts (RepoVersion 10)) { unlockedFiles = True })
		, unlesscrippled ("v10 locked", testMode opts (RepoVersion 10))
		]
	remotetestmode = testMode opts (RepoVersion 8)
	unlesscrippled v
		| crippledfilesystem = Nothing
		| otherwise = Just v
	canadjust v
		| adjustedbranchok = Just v
		| otherwise = Nothing
	mkrepotests (d, te) = map 
		(\uts -> withTestMode te uts)
		(repoTests d n)

properties :: TestTree
properties = localOption (QuickCheckTests 1000) $ testGroup "QuickCheck" $
	[ testProperty "prop_encode_decode_roundtrip" Git.Filename.prop_encode_decode_roundtrip
	, testProperty "prop_encode_c_decode_c_roundtrip" Utility.Format.prop_encode_c_decode_c_roundtrip
	, testProperty "prop_isomorphic_key_encode" Key.prop_isomorphic_key_encode
	, testProperty "prop_isomorphic_shellEscape" Utility.ShellEscape.prop_isomorphic_shellEscape
	, testProperty "prop_isomorphic_shellEscape_multiword" Utility.ShellEscape.prop_isomorphic_shellEscape_multiword
	, testProperty "prop_isomorphic_configEscape" Logs.Remote.prop_isomorphic_configEscape
	, testProperty "prop_parse_show_Config" Logs.Remote.prop_parse_show_Config
	, testProperty "prop_upFrom_basics" Utility.Path.Tests.prop_upFrom_basics
	, testProperty "prop_relPathDirToFileAbs_basics" Utility.Path.Tests.prop_relPathDirToFileAbs_basics
	, testProperty "prop_relPathDirToFileAbs_regressionTest" Utility.Path.Tests.prop_relPathDirToFileAbs_regressionTest
	, testProperty "prop_dirContains_regressionTest" Utility.Path.Tests.prop_dirContains_regressionTest
	, testProperty "prop_cost_sane" Config.Cost.prop_cost_sane
	, testProperty "prop_matcher_sane" Utility.Matcher.prop_matcher_sane
	, testProperty "prop_HmacSha1WithCipher_sane" Crypto.prop_HmacSha1WithCipher_sane
	, testProperty "prop_VectorClock_sane" Annex.VectorClock.prop_VectorClock_sane
	, testProperty "prop_addMapLog_sane" Logs.MapLog.prop_addMapLog_sane
	, testProperty "prop_verifiable_sane" Utility.Verifiable.prop_verifiable_sane
	, testProperty "prop_segment_regressionTest" Utility.Misc.prop_segment_regressionTest
	, testProperty "prop_read_write_transferinfo" Logs.Transfer.prop_read_write_transferinfo
	, testProperty "prop_read_show_inodecache" Utility.InodeCache.prop_read_show_inodecache
	, testProperty "prop_parse_build_presence_log" Logs.Presence.prop_parse_build_presence_log
	, testProperty "prop_parse_build_contentidentifier_log" Logs.ContentIdentifier.prop_parse_build_contentidentifier_log
	, testProperty "prop_read_show_TrustLevel" Types.TrustLevel.prop_read_show_TrustLevel
	, testProperty "prop_parse_build_TrustLevelLog" Logs.Trust.prop_parse_build_TrustLevelLog
	, testProperty "prop_schedule_roundtrips" Utility.Scheduled.QuickCheck.prop_schedule_roundtrips
	, testProperty "prop_past_sane" Utility.Scheduled.prop_past_sane
	, testProperty "prop_duration_roundtrips" Utility.HumanTime.prop_duration_roundtrips
	, testProperty "prop_metadata_sane" Types.MetaData.prop_metadata_sane
	, testProperty "prop_metadata_serialize" Types.MetaData.prop_metadata_serialize
	, testProperty "prop_branchView_legal" Logs.View.prop_branchView_legal
	, testProperty "prop_viewPath_roundtrips" Annex.View.prop_viewPath_roundtrips
	, testProperty "prop_view_roundtrips" Annex.View.prop_view_roundtrips
	, testProperty "prop_viewedFile_rountrips" Annex.View.ViewedFile.prop_viewedFile_roundtrips
	, testProperty "prop_b64_roundtrips" Utility.Base64.prop_b64_roundtrips
	, testProperty "prop_standardGroups_parse" Logs.PreferredContent.prop_standardGroups_parse
	] ++ map (uncurry testProperty) combos
  where
	combos = concat
		[ Utility.Hash.props_hashes_stable
		, Utility.Hash.props_macs_stable
		]

testRemotes :: TestTree
testRemotes = testGroup "Remote Tests" $
	-- These tests are failing in really strange ways on Windows,
	-- apparently not due to an actual problem with the remotes being
	-- tested, so are disabled there.
#ifdef mingw32_HOST_OS
	filter (\_ -> False)
#endif
	[ testGitRemote
	, testDirectoryRemote
	]

testGitRemote :: TestTree
testGitRemote = testRemote False "git" $ \remotename -> do
	git "clone" [".", "remotedir"] "git clone"
	git "remote" ["add", remotename, "remotedir"] "git remote add"

testDirectoryRemote :: TestTree
testDirectoryRemote = testRemote True "directory" $ \remotename -> do
	createDirectory "remotedir"
	git_annex "initremote"
		[ remotename
		, "type=directory"
		, "--quiet"
		, "directory=remotedir"
		, "encryption=none"
		] "init"

testRemote :: Bool -> String -> (String -> IO ()) -> TestTree
testRemote testvariants remotetype setupremote = 
	withResource newEmptyTMVarIO (const noop) $ \getv -> 
		testGroup ("testremote type " ++ remotetype) $ concat
			[ [testCase "init" (prep getv)]
			, go getv
			]
  where
	reponame = "test repo"
	remotename = "testremote"
	basesz = 1024 * 1024
	keysizes = Command.TestRemote.keySizes basesz False
	prep getv = do
		d <- newmainrepodir
		setmainrepodir d
		innewrepo $ do
			git_annex "init" [reponame, "--quiet"] "init"
			setupremote remotename
			r <- annexeval $ either error return 
				=<< Remote.byName' remotename
			cache <- Command.TestRemote.newRemoteVariantCache
			unavailr <- annexeval $ Types.Remote.mkUnavailable r
			exportr <- annexeval $ Command.TestRemote.exportTreeVariant cache r
			ks <- annexeval $ mapM Command.TestRemote.randKey keysizes
			v <- getv
			cv <- annexeval cache
			liftIO $ atomically $ putTMVar v
				(r, (unavailr, (exportr, (ks, cv))))
	go getv = Command.TestRemote.mkTestTrees runannex mkrs mkunavailr mkexportr mkks
	  where
		runannex = inmainrepo . annexeval
		mkrs = if testvariants
			then Command.TestRemote.remoteVariants cache mkr basesz False
			else [fmap (fmap Just) mkr]
		mkr = descas (remotetype ++ " remote") (fst <$> v)
		mkunavailr = fst . snd <$> v
		mkexportr = fst . snd . snd <$> v
		mkks = map (\(sz, n) -> desckeysize sz (getk n))
			(zip keysizes [0..])
		getk n = fmap (!! n) (fst . snd . snd . snd <$> v)
		cache = snd . snd . snd . snd <$> v
		v = liftIO $ atomically . readTMVar =<< getv
		descas = Command.TestRemote.Described
		desckeysize sz = descas ("key size " ++ show sz)

{- These tests set up the test environment, but also test some basic parts
 - of git-annex. They are always run before the repoTests. -}
initTests :: TestTree
initTests = testGroup initTestsName
	[ testCase "init" test_init
	, testCase "add" test_add
	]

repoTests :: String -> Int -> [TestTree]
repoTests note numparts = map mk $ sep
	[ testCase "add dup" test_add_dup
	, testCase "add extras" test_add_extras
	, testCase "add moved link" test_add_moved
	, testCase "readonly remote" test_readonly_remote
	, testCase "ignore deleted files" test_ignore_deleted_files
	, testCase "metadata" test_metadata
	, testCase "export_import" test_export_import
	, testCase "export_import_subdir" test_export_import_subdir
	, testCase "shared clone" test_shared_clone
	, testCase "log" test_log
	, testCase "view" test_view
	, testCase "magic" test_magic
	, testCase "import" test_import
	, testCase "reinject" test_reinject
	, testCase "unannex (no copy)" test_unannex_nocopy
	, testCase "unannex (with copy)" test_unannex_withcopy
	, testCase "drop (no remote)" test_drop_noremote
	, testCase "drop (with remote)" test_drop_withremote
	, testCase "drop (untrusted remote)" test_drop_untrustedremote
	, testCase "get" test_get
	, testCase "get (ssh remote)" test_get_ssh_remote
	, testCase "move" test_move
	, testCase "move (ssh remote)" test_move_ssh_remote
	, testCase "move (numcopies)" test_move_numcopies
	, testCase "copy" test_copy
	, testCase "lock" test_lock
	, testCase "lock --force" test_lock_force
	, testCase "edit (no pre-commit)" test_edit
	, testCase "edit (pre-commit)" test_edit_precommit
	, testCase "partial commit" test_partial_commit
	, testCase "fix" test_fix
	, testCase "trust" test_trust
	, testCase "fsck (basics)" test_fsck_basic
	, testCase "fsck (bare)" test_fsck_bare
	, testCase "fsck (local untrusted)" test_fsck_localuntrusted
	, testCase "fsck (remote untrusted)" test_fsck_remoteuntrusted
	, testCase "fsck --from remote" test_fsck_fromremote
	, testCase "conversion git to annexed" test_conversion_git_to_annexed
	, testCase "conversion annexed to git" test_conversion_annexed_to_git
	, testCase "migrate" test_migrate
	, testCase "migrate (via gitattributes)" test_migrate_via_gitattributes
	, testCase "unused" test_unused
	, testCase "describe" test_describe
	, testCase "find" test_find
	, testCase "merge" test_merge
	, testCase "info" test_info
	, testCase "version" test_version
	, testCase "sync" test_sync
	, testCase "concurrent get of dup key regression" test_concurrent_get_of_dup_key_regression
	, testCase "union merge regression" test_union_merge_regression
	, testCase "adjusted branch merge regression" test_adjusted_branch_merge_regression
	, testCase "adjusted branch subtree regression" test_adjusted_branch_subtree_regression
	, testCase "transition propagation" test_transition_propagation_reversion
	, testCase "conflict resolution" test_conflict_resolution
	, testCase "conflict resolution (adjusted branch)" test_conflict_resolution_adjusted_branch
	, testCase "conflict resolution movein regression" test_conflict_resolution_movein_regression
	, testCase "conflict resolution (mixed directory and file)" test_mixed_conflict_resolution
	, testCase "conflict resolution symlink bit" test_conflict_resolution_symlink_bit
	, testCase "conflict resolution (uncommitted local file)" test_uncommitted_conflict_resolution
	, testCase "conflict resolution (removed file)" test_remove_conflict_resolution
	, testCase "conflict resolution (nonannexed file)" test_nonannexed_file_conflict_resolution
	, testCase "conflict resolution (nonannexed symlink)" test_nonannexed_symlink_conflict_resolution
	, testCase "conflict resolution (mixed locked and unlocked file)" test_mixed_lock_conflict_resolution
	, testCase "map" test_map
	, testCase "uninit" test_uninit
	, testCase "uninit (in git-annex branch)" test_uninit_inbranch
	, testCase "upgrade" test_upgrade
	, testCase "whereis" test_whereis
	, testCase "hook remote" test_hook_remote
	, testCase "directory remote" test_directory_remote
	, testCase "rsync remote" test_rsync_remote
	, testCase "bup remote" test_bup_remote
	, testCase "borg remote" test_borg_remote
	, testCase "crypto" test_crypto
	, testCase "preferred content" test_preferred_content
	, testCase "required_content" test_required_content
	, testCase "add subdirs" test_add_subdirs
	, testCase "addurl" test_addurl
	]
  where
	mk l = testGroup groupname (initTests : map adddep l)
	adddep = Test.Tasty.after AllSucceed (groupname ++ "." ++ initTestsName)
	groupname = "Repo Tests " ++ note
	sep l = 
		-- Avoid separating into parts that contain less than
		-- 5 tests each. Since the tests depend on the
		-- initTests, this avoids spending too much work running
		-- the initTests once per part.
		let numparts' = if length l `div` numparts > 5
			then numparts
			else length l `div` 5
		in sep' (replicate numparts' []) l
	sep' (p:ps) (l:ls) = sep' (ps++[l:p]) ls
	sep' ps [] = ps
	sep' [] _ = []

-- this test case creates the main repo
test_init :: Assertion
test_init = innewrepo $ do
	ver <- annexVersion <$> getTestMode
	git_annex "init" [reponame, "--version", show (fromRepoVersion ver)] "init"
	setupTestMode
  where
	reponame = "test repo"

-- this test case runs in the main repo, to set up a basic
-- annexed file that later tests will use
test_add :: Assertion
test_add = inmainrepo $ do
	writecontent annexedfile $ content annexedfile
	add_annex annexedfile "add"
	annexed_present annexedfile
	writecontent sha1annexedfile $ content sha1annexedfile
	git_annex "add" [sha1annexedfile, "--backend=SHA1"] "add with SHA1"
	whenM (unlockedFiles <$> getTestMode) $
		git_annex "unlock" [sha1annexedfile] "unlock"
	annexed_present sha1annexedfile
	checkbackend sha1annexedfile backendSHA1
	writecontent ingitfile $ content ingitfile
	git "add" [ingitfile] "git add"
	git "commit" ["-q", "-m", "commit"] "git commit"
	git_annex "add" [ingitfile] "add ingitfile should be no-op"
	unannexed ingitfile

test_add_dup :: Assertion
test_add_dup = intmpclonerepo $ do
	writecontent annexedfiledup $ content annexedfiledup
	add_annex annexedfiledup "add of second file with same content failed"
	annexed_present annexedfiledup
	annexed_present annexedfile

test_add_extras :: Assertion
test_add_extras = intmpclonerepo $ do
	writecontent wormannexedfile $ content wormannexedfile
	git_annex "add" [wormannexedfile, "--backend=WORM"] "add with WORM"
	whenM (unlockedFiles <$> getTestMode) $
		git_annex "unlock" [wormannexedfile] "unlock"
	annexed_present wormannexedfile
	checkbackend wormannexedfile backendWORM

test_add_moved :: Assertion
test_add_moved = intmpclonerepo $ do
	git_annex "get" [annexedfile] "get failed"
	annexed_present annexedfile
	createDirectory subdir
	Utility.MoveFile.moveFile (toRawFilePath annexedfile) (toRawFilePath subfile)
	git_annex "add" [subdir] "add of moved annexed file"
	git "mv" [sha1annexedfile, sha1annexedfile ++ ".renamed"] "git mv"
	git_annex "add" [] "add does not fail on deleted file after move"
  where
	subdir = "subdir"
	subfile = subdir </> "file"

test_readonly_remote :: Assertion
test_readonly_remote =
#ifndef mingw32_HOST_OS
	withtmpclonerepo $ \r1 ->
		withtmpclonerepo $ \r2 -> do
			pair r1 r2
			indir r1 $ do
				git_annex "get" [annexedfile] "get failed in first repo"
			make_readonly r1
			indir r2 $ do
				git_annex "sync" ["r1", "--no-push"] "sync with readonly repo"
				git_annex "get" [annexedfile, "--from", "r1"] "get from readonly repo"
				git "remote" ["rm", "origin"] "remote rm"
				git_annex "drop" [annexedfile] "drop vs readonly repo"
#else
	-- This test does not work on Windows, and it doesn't make sense to
	-- try to support this on Windows anyway.
	return ()
#endif

test_ignore_deleted_files :: Assertion
test_ignore_deleted_files = intmpclonerepo $ do
	git_annex "get" [annexedfile] "get"
	git_annex_expectoutput "find" [] [annexedfile]
	removeWhenExistsWith R.removeLink (toRawFilePath annexedfile)
	-- A file that has been deleted, but the deletion not staged,
	-- is a special case; make sure git-annex skips these.
	git_annex_expectoutput "find" [] []

test_metadata :: Assertion
test_metadata = intmpclonerepo $ do
	git_annex "metadata" ["-s", "foo=bar", annexedfile] "set metadata"
	git_annex_expectoutput "find" ["--metadata", "foo=bar"] [annexedfile]
	git_annex_expectoutput "find" ["--metadata", "foo=other"] []
	readonly_query $ do
		git_annex_expectoutput "find" ["--metadata", "foo=bar"] [annexedfile]
		git_annex_expectoutput "find" ["--metadata", "foo=other"] []
	writecontent annexedfiledup $ content annexedfiledup
	add_annex annexedfiledup "add of second file with same content failed"
	annexed_present annexedfiledup
	git_annex_expectoutput "find" ["--metadata", "foo=bar"]
		[annexedfile, annexedfiledup]
	git_annex_shouldfail "metadata" ["--remove", "foo", "."]
		"removing metadata from dir with multiple files not allowed"
	git_annex "metadata" ["--remove", "foo", annexedfile]
		"removing metadata"
	git_annex_expectoutput "find" ["--metadata", "foo=bar"] []
	git_annex "metadata" ["--force", "-s", "foo=bar", "."]
		"recursively set metadata force"

test_shared_clone :: Assertion
test_shared_clone = intmpsharedclonerepo $ do
	v <- catchMaybeIO $ Utility.Process.readProcess "git" 
		[ "config"
		, "--bool"
		, "--get"
		, "annex.hardlink"
		]
	v == Just "true\n"
		@? "shared clone of repo did not get annex.hardlink set"

test_log :: Assertion
test_log = intmpclonerepo $ do
	git_annex "log" [annexedfile] "log"

test_view :: Assertion
test_view = intmpclonerepo $ do
	git_annex "metadata" ["-s", "test=test1", annexedfile]  "metadata"
	git_annex "metadata" ["-s", "test=test2", sha1annexedfile] "metadata"
	git_annex "view" ["test=test1"] "entering view"
	checkexists annexedfile
	checkdoesnotexist sha1annexedfile

test_magic :: Assertion
test_magic = intmpclonerepo $ do
#ifdef WITH_MAGICMIME
	git "config" ["annex.largefiles", "mimeencoding=binary"]
		"git config annex.largefiles"
	writeFile "binary" "\127"
	writeFile "text" "test\n" 
	git_annex "add" ["binary", "text"]
		"git-annex add with mimeencoding in largefiles"
	git_annex "sync" []
		"git-annex sync"
	(isJust <$> annexeval (Annex.CatFile.catKeyFile (encodeBS "binary")))
		@? "binary file not added to annex despite mimeencoding config"
	(isNothing <$> annexeval (Annex.CatFile.catKeyFile (encodeBS "text")))
		@? "non-binary file got added to annex despite mimeencoding config"
#else
	return ()
#endif

test_import :: Assertion
test_import = intmpclonerepo $ Utility.Tmp.Dir.withTmpDir "importtest" $ \importdir -> do
	(toimport1, importf1, imported1) <- mktoimport importdir "import1"
	git_annex "import" [toimport1] "import"
	annexed_present_imported imported1
	checkdoesnotexist importf1

	(toimport2, importf2, imported2) <- mktoimport importdir "import2"
	git_annex "import" [toimport2] "import of duplicate"
	annexed_present_imported imported2
	checkdoesnotexist importf2

	(toimport3, importf3, imported3) <- mktoimport importdir "import3"
	git_annex "import" ["--skip-duplicates", toimport3]
		"import of duplicate with --skip-duplicates"
	checkdoesnotexist imported3
	checkexists importf3
	git_annex "import" ["--clean-duplicates", toimport3]
		"import of duplicate with --clean-duplicates"
	checkdoesnotexist imported3
	checkdoesnotexist importf3
	
	(toimport4, importf4, imported4) <- mktoimport importdir "import4"
	git_annex "import" ["--deduplicate", toimport4] "import --deduplicate"
	checkdoesnotexist imported4
	checkdoesnotexist importf4
	
	(toimport5, importf5, imported5) <- mktoimport importdir "import5"
	git_annex "import" ["--duplicate", toimport5] "import --duplicate"
	annexed_present_imported imported5
	checkexists importf5
	
	git_annex "drop" ["--force", imported1, imported2, imported5] "drop"
	annexed_notpresent_imported imported2
	(toimportdup, importfdup, importeddup) <- mktoimport importdir "importdup"
	git_annex_shouldfail "import" ["--clean-duplicates", toimportdup] 
		"import of missing duplicate with --clean-duplicates not allowed"
	checkdoesnotexist importeddup
	checkexists importfdup
  where
	mktoimport importdir subdir = do
		createDirectory (importdir </> subdir)
		let importf = subdir </> "f"
		writecontent (importdir </> importf) (content importf)
		return (importdir </> subdir, importdir </> importf, importf)

test_reinject :: Assertion
test_reinject = intmpclonerepo $ do
	git_annex "drop" ["--force", sha1annexedfile] "drop"
	annexed_notpresent sha1annexedfile
	writecontent tmp $ content sha1annexedfile
	key <- Key.serializeKey <$> getKey backendSHA1 tmp
	git_annex "reinject" [tmp, sha1annexedfile] "reinject"
	annexed_present sha1annexedfile
	git_annex "fromkey" [key, sha1annexedfiledup] "fromkey for dup"
	whenM (unlockedFiles <$> getTestMode) $
		git_annex "unlock" [sha1annexedfiledup] "unlock"
	annexed_present sha1annexedfiledup
  where
	tmp = "tmpfile"

test_unannex_nocopy :: Assertion
test_unannex_nocopy = intmpclonerepo $ do
	annexed_notpresent annexedfile
	git_annex "unannex" [annexedfile] "unannex with no copy"
	annexed_notpresent annexedfile

test_unannex_withcopy :: Assertion
test_unannex_withcopy = intmpclonerepo $ do
	git_annex "get" [annexedfile] "get"
	annexed_present annexedfile
	git_annex "unannex" [annexedfile, sha1annexedfile] "unannex"
	unannexed annexedfile
	git_annex "unannex" [ingitfile] "unannex ingitfile should be no-op"
	unannexed ingitfile

test_drop_noremote :: Assertion
test_drop_noremote = intmpclonerepo $ do
	git_annex "get" [annexedfile] "get"
	git "remote" ["rm", "origin"] "git remote rm origin"
	git_annex_shouldfail "drop" [annexedfile] "drop with no known copy of file not allowed"
	annexed_present annexedfile
	git_annex "drop" ["--force", annexedfile] "drop --force"
	annexed_notpresent annexedfile
	git_annex "drop" [annexedfile] "drop of dropped file"
	git_annex "drop" [ingitfile] "drop ingitfile should be no-op"
	unannexed ingitfile

test_drop_withremote :: Assertion
test_drop_withremote = intmpclonerepo $ do
	git_annex "get" [annexedfile] "get"
	annexed_present annexedfile
	git_annex "numcopies" ["2"] "numcopies config"
	git_annex_shouldfail "drop" [annexedfile] "drop with numcopies not satisfied is not allowed"
	git_annex "numcopies" ["1"] "numcopies config"
	git_annex "drop" [annexedfile] "drop when origin has copy"
	annexed_notpresent annexedfile
	-- make sure that the correct symlink is staged for the file
	-- after drop
	git_annex_expectoutput "status" [] []
	inmainrepo $ annexed_present annexedfile

test_drop_untrustedremote :: Assertion
test_drop_untrustedremote = intmpclonerepo $ do
	git_annex "untrust" ["origin"] "untrust of origin"
	git_annex "get" [annexedfile] "get"
	annexed_present annexedfile
	git_annex_shouldfail "drop" [annexedfile] "drop with only an untrusted copy of the file should fail"
	annexed_present annexedfile
	inmainrepo $ annexed_present annexedfile

test_get :: Assertion
test_get = test_get' intmpclonerepo

test_get_ssh_remote :: Assertion
test_get_ssh_remote = 
#ifndef mingw32_HOST_OS
	test_get' (with_ssh_origin intmpclonerepo)
#else
	noop
#endif

test_get' :: (Assertion -> Assertion) -> Assertion
test_get' setup = setup $ do
	inmainrepo $ annexed_present annexedfile
	annexed_notpresent annexedfile
	git_annex "get" [annexedfile] "get of file"
	inmainrepo $ annexed_present annexedfile
	annexed_present annexedfile
	git_annex "get" [annexedfile] "get of file already here"
	inmainrepo $ annexed_present annexedfile
	annexed_present annexedfile
	inmainrepo $ unannexed ingitfile
	unannexed ingitfile
	git_annex "get" [ingitfile] "get ingitfile should be no-op"
	inmainrepo $ unannexed ingitfile
	unannexed ingitfile

test_move :: Assertion
test_move = test_move' intmpclonerepo

test_move_ssh_remote :: Assertion
test_move_ssh_remote =
#ifndef mingw32_HOST_OS
	test_move' (with_ssh_origin intmpclonerepo)
#else
	noop
#endif

test_move' :: (Assertion -> Assertion) -> Assertion
test_move' setup = setup $ do
	annexed_notpresent annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "move" ["--from", "origin", annexedfile] "move --from of file"
	annexed_present annexedfile
	inmainrepo $ annexed_notpresent annexedfile
	git_annex "move" ["--from", "origin", annexedfile] "move --from of file already here"
	annexed_present annexedfile
	inmainrepo $ annexed_notpresent annexedfile
	git_annex "move" ["--to", "origin", annexedfile] "move --to of file"
	inmainrepo $ annexed_present annexedfile
	annexed_notpresent annexedfile
	git_annex "move" ["--to", "origin", annexedfile] "move --to of file already there"
	inmainrepo $ annexed_present annexedfile
	annexed_notpresent annexedfile
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile
	git_annex "move" ["--to", "origin", ingitfile] "move of ingitfile should be no-op"
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile
	git_annex "move" ["--from", "origin", ingitfile] "move of ingitfile should be no-op"
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile

test_move_numcopies :: Assertion
test_move_numcopies = intmpclonerepo $ do
	inmainrepo $ annexed_present annexedfile
	git_annex "numcopies" ["2"] "setting numcopies"
	git_annex "move" ["--from", "origin", annexedfile] "move of file --from remote with numcopies unsatisfied but not made worse"
	annexed_present annexedfile
	inmainrepo $ annexed_notpresent annexedfile
	git_annex "move" ["--to", "origin", annexedfile] "move of file --to remote with numcopies unsatisfied but not made worse"
	annexed_notpresent annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "get" [annexedfile] "get of file"
	git_annex_shouldfail "move" ["--from", "origin", annexedfile] "move of file --from remote that violates numcopies setting not allowed"
	git_annex_shouldfail "move" ["--to", "origin", annexedfile] "move of file --to remote that violates numcopies setting not allowed"

test_copy :: Assertion
test_copy = intmpclonerepo $ do
	annexed_notpresent annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "copy" ["--from", "origin", annexedfile] "copy --from of file"
	annexed_present annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "copy" ["--from", "origin", annexedfile] "copy --from of file already here"
	annexed_present annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "copy" ["--to", "origin", annexedfile] "copy --to of file already there"
	annexed_present annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "move" ["--to", "origin", annexedfile] "move --to of file already there"
	annexed_notpresent annexedfile
	inmainrepo $ annexed_present annexedfile
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile
	git_annex "copy" ["--to", "origin", ingitfile] "copy of ingitfile should be no-op"
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile
	git_annex "copy" ["--from", "origin", ingitfile] "copy of ingitfile should be no-op"
	checkregularfile ingitfile
	checkcontent ingitfile

test_preferred_content :: Assertion
test_preferred_content = intmpclonerepo $ do
	annexed_notpresent annexedfile
	-- get/copy --auto looks only at numcopies when preferred content is not
	-- set, and with 1 copy existing, does not get the file.
	git_annex "get" ["--auto", annexedfile] "get --auto of file with default preferred content"
	annexed_notpresent annexedfile
	git_annex "copy" ["--from", "origin", "--auto", annexedfile] "copy --auto --from of file with default preferred content"
	annexed_notpresent annexedfile

	git_annex "wanted" [".", "standard"] "set expression to standard"
	git_annex "group" [".", "client"] "set group to standard"
	git_annex "get" ["--auto", annexedfile] "get --auto of file for client"
	annexed_present annexedfile
	git_annex "drop" [annexedfile] "drop of file"
	git_annex "copy" ["--from", "origin", "--auto", annexedfile] "copy --auto --from of file for client"
	annexed_present annexedfile
	git_annex "ungroup" [".", "client"] "ungroup"

	git_annex "wanted" [".", "standard"] "set expression to standard"
	git_annex "group" [".", "manual"] "set group to manual"
	-- drop --auto with manual leaves the file where it is
	git_annex "drop" ["--auto", annexedfile] "drop --auto of file with manual preferred content"
	annexed_present annexedfile
	git_annex "drop" [annexedfile] "drop of file"
	annexed_notpresent annexedfile
	-- copy/get --auto with manual does not get the file
	git_annex "get" ["--auto", annexedfile] "get --auto of file with manual preferred content"
	annexed_notpresent annexedfile
	git_annex "copy" ["--from", "origin", "--auto", annexedfile] "copy --auto --from of file with manual preferred content"
	annexed_notpresent annexedfile
	git_annex "ungroup" [".", "client"] "ungroup"
	
	git_annex "wanted" [".", "exclude=*"] "set expression to exclude=*"
	git_annex "get" [annexedfile] "get of file"
	annexed_present annexedfile
	git_annex "drop" ["--auto", annexedfile] "drop --auto of file with exclude=*"
	annexed_notpresent annexedfile
	git_annex "get" ["--auto", annexedfile] "get --auto of file with exclude=*"
	annexed_notpresent annexedfile

test_required_content :: Assertion
test_required_content = intmpclonerepo $ do
	git_annex "get" [annexedfile] "get"
	annexed_present annexedfile
	git_annex "required" [".", "include=" ++ annexedfile] "annexedfile required"

	git_annex_shouldfail "drop" [annexedfile] "drop of required content should fail"
	annexed_present annexedfile

	git_annex "drop" ["--auto", annexedfile] "drop --auto of required content skips it"
	annexed_present annexedfile
	
	writecontent annexedfiledup $ content annexedfiledup
	add_annex annexedfiledup "add of second file with same content failed"
	annexed_present annexedfiledup
	annexed_present annexedfile

	git_annex_shouldfail "drop" [annexedfiledup] "drop of file sharing required content should fail"
	annexed_present annexedfiledup
	annexed_present annexedfile

test_lock :: Assertion
test_lock = intmpclonerepo $ do
	annexed_notpresent annexedfile

	-- regression test: unlock of newly added, not committed file
	-- should not fail.
	writecontent "newfile" "foo"
	git_annex "add" ["newfile"] "add new file"
	git_annex "unlock" ["newfile"] "unlock on newly added, never committed file"

	git_annex "get" [annexedfile] "get of file"
	annexed_present annexedfile
	git_annex "unlock" [annexedfile] "unlock"
	unannexed annexedfile
	-- write different content, to verify that lock
	-- throws it away
	changecontent annexedfile
	writecontent annexedfile $ content annexedfile ++ "foo"
	git_annex_shouldfail "lock" [annexedfile] "lock without --force should not be allowed"
	git_annex "lock" ["--force", annexedfile] "lock --force"
	-- The original content of an unlocked file is not always
	-- preserved after modification, so re-get it.
	git_annex "get" [annexedfile] "get of file after lock --force"
	annexed_present_locked annexedfile
	git_annex "unlock" [annexedfile] "unlock"
	unannexed annexedfile
	changecontent annexedfile
	git "add" [annexedfile] "add of modified file"
	runchecks [checkregularfile, checkwritable] annexedfile
	c <- readFile annexedfile
	assertEqual "content of modified file" c (changedcontent annexedfile)
	git_annex_shouldfail "drop" [annexedfile]
		"drop with no known copy of modified file should not be allowed"

-- Regression test: lock --force when work tree file
-- was modified lost the (unmodified) annex object.
-- (Only occurred when the keys database was out of sync.)
test_lock_force :: Assertion
test_lock_force = intmpclonerepo $ do
	git_annex "upgrade" [] "upgrade"
	git_annex "get" [annexedfile] "get of file"
	git_annex "unlock" [annexedfile] "unlock"
	annexeval $ do
		Just k <- Annex.WorkTree.lookupKey (toRawFilePath annexedfile)
		Database.Keys.removeInodeCaches k
		Database.Keys.closeDb
		liftIO . removeWhenExistsWith R.removeLink
			=<< Annex.calcRepo' Annex.Locations.gitAnnexKeysDbIndexCache
	writecontent annexedfile "test_lock_force content"
	git_annex_shouldfail "lock" [annexedfile] "lock of modified file should not be allowed"
	git_annex "lock" ["--force", annexedfile] "lock --force of modified file"
	annexed_present_locked annexedfile

test_edit :: Assertion
test_edit = test_edit' False

test_edit_precommit :: Assertion
test_edit_precommit = test_edit' True

test_edit' :: Bool -> Assertion
test_edit' precommit = intmpclonerepo $ do
	git_annex "get" [annexedfile] "get of file"
	annexed_present annexedfile
	git_annex "edit" [annexedfile] "edit"
	unannexed annexedfile
	changecontent annexedfile
	git "add" [annexedfile] "git add of edited file"
	if precommit
		then git_annex "pre-commit" [] "pre-commit"
		else git "commit" ["-q", "-m", "contentchanged"] "git commit of edited file"
	runchecks [checkregularfile, checkwritable] annexedfile
	c <- readFile annexedfile
	assertEqual "content of modified file" c (changedcontent annexedfile)
	git_annex_shouldfail "drop" [annexedfile] "drop no known copy of modified file should not be allowed"

test_partial_commit :: Assertion
test_partial_commit = intmpclonerepo $ do
	git_annex "get" [annexedfile] "get of file"
	annexed_present annexedfile
	git_annex "unlock" [annexedfile] "unlock"
	changecontent annexedfile
	git "commit" ["-q", "-m", "test", annexedfile]
		"partial commit of unlocked file should be allowed"

test_fix :: Assertion
test_fix = intmpclonerepo $ unlessM (hasUnlockedFiles <$> getTestMode) $ do
	annexed_notpresent annexedfile
	git_annex "fix" [annexedfile] "fix of not present"
	annexed_notpresent annexedfile
	git_annex "get" [annexedfile] "get of file"
	annexed_present annexedfile
	git_annex "fix" [annexedfile] "fix of present file"
	annexed_present annexedfile
	createDirectory subdir
	git "mv" [annexedfile, subdir] "git mv"
	git_annex "fix" [newfile] "fix of moved file"
	runchecks [checklink, checkunwritable] newfile
	c <- readFile newfile
	assertEqual "content of moved file" c (content annexedfile)
  where
	subdir = "s"
	newfile = subdir ++ "/" ++ annexedfile

test_trust :: Assertion
test_trust = intmpclonerepo $ do
	git_annex "trust" ["--force", repo] "trust"
	trustcheck Logs.Trust.Trusted "trusted 1"
	git_annex "trust" ["--force", repo] "trust of trusted"
	trustcheck Logs.Trust.Trusted "trusted 2"
	git_annex "untrust" [repo] "untrust"
	trustcheck Logs.Trust.UnTrusted "untrusted 1"
	git_annex "untrust" [repo] "untrust of untrusted"
	trustcheck Logs.Trust.UnTrusted "untrusted 2"
	git_annex "dead" [repo] "dead"
	trustcheck Logs.Trust.DeadTrusted "deadtrusted 1"
	git_annex "dead" [repo] "dead of dead"
	trustcheck Logs.Trust.DeadTrusted "deadtrusted 2"
	git_annex "semitrust" [repo] "semitrust"
	trustcheck Logs.Trust.SemiTrusted "semitrusted 1"
	git_annex "semitrust" [repo] "semitrust of semitrusted"
	trustcheck Logs.Trust.SemiTrusted "semitrusted 2"
  where
	repo = "origin"
	trustcheck expected msg = do
		present <- annexeval $ do
			l <- Logs.Trust.trustGet expected
			u <- Remote.nameToUUID repo
			return $ u `elem` l
		assertBool msg present

test_fsck_basic :: Assertion
test_fsck_basic = intmpclonerepo $ do
	git_annex "fsck" [] "fsck"
	git_annex "numcopies" ["2"] "numcopies config"
	fsck_should_fail "numcopies unsatisfied"
	git_annex "numcopies" ["1"] "numcopies config"
	corrupt annexedfile
	corrupt sha1annexedfile
  where
	corrupt f = do
		git_annex "get" [f] "get of file"
		Utility.FileMode.allowWrite (toRawFilePath f)
		writecontent f (changedcontent f)
		ifM (hasUnlockedFiles <$> getTestMode)
			( git_annex "fsck" []"fsck on unlocked file with changed file content"
			, git_annex_shouldfail "fsck" [] "fsck with corrupted file content should error"
			)
		git_annex "fsck" [] "second fsck, after first fsck should have fixed problem"

test_fsck_bare :: Assertion
test_fsck_bare = intmpbareclonerepo $
	git_annex "fsck" [] "fsck"

test_fsck_localuntrusted :: Assertion
test_fsck_localuntrusted = intmpclonerepo $ do
	git_annex "get" [annexedfile] "get"
	git_annex "untrust" ["origin"] "untrust of origin repo"
	git_annex "untrust" ["."] "untrust of current repo"
	fsck_should_fail "content only available in untrusted (current) repository"
	git_annex "trust" ["--force", "."] "trust of current repo"
	git_annex "fsck" [annexedfile] "fsck on file present in trusted repo"

test_fsck_remoteuntrusted :: Assertion
test_fsck_remoteuntrusted = intmpclonerepo $ do
	git_annex "numcopies" ["2"] "numcopies config"
	git_annex "get" [annexedfile] "get"
	git_annex "get" [sha1annexedfile] "get"
	git_annex "fsck" [] "fsck with numcopies=2 and 2 copies"
	git_annex "untrust" ["origin"] "untrust of origin"
	fsck_should_fail "content not replicated to enough non-untrusted repositories"

test_fsck_fromremote :: Assertion
test_fsck_fromremote = intmpclonerepo $ do
	git_annex "fsck" ["--from", "origin"] "fsck --from origin"

fsck_should_fail :: String -> Assertion
fsck_should_fail m = git_annex_shouldfail "fsck" []
	("fsck should not succeed with " ++ m)

-- Make sure that the "converting git to annexed" recipe in
-- doc/tips/largefiles.mdwn continues to work.
test_conversion_git_to_annexed :: Assertion
test_conversion_git_to_annexed = intmpclonerepo $ do
	git "rm" ["--cached", ingitfile] "git rm --cached"
	git_annex "add" ["--force-large", ingitfile] "add --force-large"
	git "commit" ["-q", "-m", "commit", ingitfile] "git commit"
	whenM (unlockedFiles <$> getTestMode) $
		git_annex "unlock" [ingitfile] "unlock"
	annexed_present ingitfile

-- Make sure that the "converting annexed to git" recipe in
-- doc/tips/largefiles.mdwn continues to work.
test_conversion_annexed_to_git :: Assertion
test_conversion_annexed_to_git = intmpclonerepo $ do	
	git_annex "get" [annexedfile] "get"
	git_annex "unlock" [annexedfile] "unlock"
	git "rm" ["--cached", annexedfile] "git rm --cached"
	git_annex "add" ["--force-small", annexedfile] "add --force-small"
	git "commit" ["-q", "-m", "commit", annexedfile] "git commit"
	unannexed_in_git annexedfile

test_migrate :: Assertion
test_migrate = test_migrate' False

test_migrate_via_gitattributes :: Assertion
test_migrate_via_gitattributes = test_migrate' True

test_migrate' :: Bool -> Assertion
test_migrate' usegitattributes = intmpclonerepo $ do
	annexed_notpresent annexedfile
	annexed_notpresent sha1annexedfile
	git_annex "migrate" [annexedfile] "migrate of not present"
	git_annex "migrate" [sha1annexedfile] "migrate of not present"
	git_annex "get" [annexedfile] "get of file"
	git_annex "get" [sha1annexedfile] "get of file"
	annexed_present annexedfile
	annexed_present sha1annexedfile
	if usegitattributes
		then do
			writeFile ".gitattributes" "* annex.backend=SHA1"
			git_annex "migrate" [sha1annexedfile]
				"migrate sha1annexedfile"
			git_annex "migrate" [annexedfile]
				"migrate annexedfile"
		else do
			git_annex "migrate" [sha1annexedfile, "--backend", "SHA1"]
				"migrate sha1annexedfile"
			git_annex "migrate" [annexedfile, "--backend", "SHA1"]
				"migrate annexedfile"
	annexed_present annexedfile
	annexed_present sha1annexedfile
	checkbackend annexedfile backendSHA1
	checkbackend sha1annexedfile backendSHA1

	-- check that reversing a migration works
	writeFile ".gitattributes" "* annex.backend=SHA256"
	git_annex "migrate" [sha1annexedfile] "migrate sha1annexedfile"
	git_annex "migrate" [annexedfile] "migrate annexedfile"
	annexed_present annexedfile
	annexed_present sha1annexedfile
	checkbackend annexedfile backendSHA256
	checkbackend sha1annexedfile backendSHA256

test_unused :: Assertion
test_unused = intmpclonerepo $ do
	checkunused [] "in new clone"
	git_annex "get" [annexedfile] "get of file"
	git_annex "get" [sha1annexedfile] "get of file"
	annexedfilekey <- getKey backendSHA256E annexedfile
	sha1annexedfilekey <- getKey backendSHA1 sha1annexedfile
	checkunused [] "after get"
	git "rm" ["-fq", annexedfile] "git rm"
	checkunused [] "after rm"
	-- commit the rm, and when on an adjusted branch, sync it back to
	-- the master branch
	git_annex "sync" ["--no-push", "--no-pull"] "git-annex sync"
	checkunused [] "after commit"
	-- unused checks origin/master; once it's gone it is really unused
	git "remote" ["rm", "origin"] "git remote rm origin"
	checkunused [annexedfilekey] "after origin branches are gone"
	git "rm" ["-fq", sha1annexedfile] "git rm"
	git_annex "sync" ["--no-push", "--no-pull"] "git-annex sync"
	checkunused [annexedfilekey, sha1annexedfilekey] "after rm sha1annexedfile"

	-- good opportunity to test dropkey also
	git_annex "dropkey" ["--force", Key.serializeKey annexedfilekey] "dropkey"
	checkunused [sha1annexedfilekey] ("after dropkey --force " ++ Key.serializeKey annexedfilekey)

	git_annex_shouldfail "dropunused" ["1"] "dropunused should not be allowed without --force"
	git_annex "dropunused" ["--force", "1"] "dropunused"
	checkunused [] "after dropunused"
	git_annex_shouldfail "dropunused" ["--force", "10", "501"] "dropunused on bogus numbers"

	-- Unused used to miss renamed symlinks that were not staged
	-- and pointed at annexed content, and think that content was unused.
	-- This is only relevant when using locked files; if the file is
	-- unlocked, the work tree file has the content, and there's no way
	-- to associate it with the key.
	unlessM (hasUnlockedFiles <$> getTestMode) $ do
		writecontent "unusedfile" "unusedcontent"
		git_annex "add" ["unusedfile"] "add of unusedfile"
		unusedfilekey <- getKey backendSHA256E "unusedfile"
		renameFile "unusedfile" "unusedunstagedfile"
		git "rm" ["-qf", "unusedfile"] "git rm"
		checkunused [] "with unstaged link"
		removeFile "unusedunstagedfile"
		checkunused [unusedfilekey] "with renamed link deleted"

	-- unused used to miss symlinks that were deleted or modified
	-- manually
	writecontent "unusedfile" "unusedcontent"
	git_annex "add" ["unusedfile"] "add of unusedfile"
	git "add" ["unusedfile"] "git add"
	unusedfilekey' <- getKey backendSHA256E "unusedfile"
	checkunused [] "with staged deleted link"
	git "rm" ["-qf", "unusedfile"] "git rm"
	checkunused [unusedfilekey'] "with staged link deleted"

	-- unused used to false positive on symlinks that were
	-- deleted or modified manually, but not staged as such
	writecontent "unusedfile" "unusedcontent"
	git_annex "add" ["unusedfile"] "add of unusedfile"
	git "add" ["unusedfile"] "git add"
	checkunused [] "with staged file"
	removeFile "unusedfile"
	checkunused [] "with staged deleted file"

	-- When an unlocked file is modified, git diff will cause git-annex
	-- to add its content to the repository. Make sure that's not
	-- found as unused.
	whenM (hasUnlockedFiles <$> getTestMode) $ do
		let f = "unlockedfile"
		writecontent f "unlockedcontent1"
		git "add" ["unlockedfile"] "git add"
		checkunused [] "with unlocked file before modification"
		writecontent f "unlockedcontent2"
		checkunused [] "with unlocked file after modification"
		git_shouldfail "diff" ["--quiet", f] "git diff should exit nonzero when unlocked file is modified"
		-- still nothing unused because one version is in the index
		-- and the other is in the work tree
		checkunused [] "with unlocked file after git diff"
  where
	checkunused expectedkeys desc = do
		git_annex "unused" [] "unused"
		unusedmap <- annexeval $ Logs.Unused.readUnusedMap mempty
		let unusedkeys = M.elems unusedmap
		assertEqual ("unused keys differ " ++ desc)
			(sort expectedkeys) (sort unusedkeys)

test_describe :: Assertion
test_describe = intmpclonerepo $ do
	git_annex "describe" [".", "this repo"] "describe 1"
	git_annex "describe" ["origin", "origin repo"] "describe 2"

test_find :: Assertion
test_find = intmpclonerepo $ do
	annexed_notpresent annexedfile
	git_annex_expectoutput "find" [] []
	readonly_query $ git_annex_expectoutput "find" [] []
	git_annex "get" [annexedfile] "get"
	annexed_present annexedfile
	annexed_notpresent sha1annexedfile
	git_annex_expectoutput "find" [] [annexedfile]
	readonly_query $ git_annex_expectoutput "find" [] [annexedfile]
	git_annex_expectoutput "find" ["--exclude", annexedfile, "--and", "--exclude", sha1annexedfile] []
	git_annex_expectoutput "find" ["--include", annexedfile] [annexedfile]
	git_annex_expectoutput "find" ["--not", "--in", "origin"] []
	git_annex_expectoutput "find" ["--copies", "1", "--and", "--not", "--copies", "2"] [sha1annexedfile]
	git_annex_expectoutput "find" ["--inbackend", "SHA1"] [sha1annexedfile]
	git_annex_expectoutput "find" ["--inbackend", "WORM"] []

	{- --include=* should match files in subdirectories too,
	 - and --exclude=* should exclude them. -}
	createDirectory "dir"
	writecontent "dir/subfile" "subfile"
	git_annex "add" ["dir"] "add of subdir"
	git_annex_expectoutput "find" ["--include", "*", "--exclude", annexedfile, "--exclude", sha1annexedfile] ["dir/subfile"]
	git_annex_expectoutput "find" ["--exclude", "*"] []

test_merge :: Assertion
test_merge = intmpclonerepo $
	git_annex "merge" [] "merge"

test_info :: Assertion
test_info = intmpclonerepo $ do
	isjson
	readonly_query isjson
	-- When presented with an input that it does not support,
	-- info does not stop but processess subsequent inputs too.
	git_annex'' (const True)
		(sha1annexedfile `isInfixOf`)
		"info"
		[annexedfile, "dnefile", sha1annexedfile] Nothing "info"
  where
	isjson = do
		json <- BU8.fromString <$> git_annex_output "info" ["--json"]
		case Utility.Aeson.eitherDecode json :: Either String Utility.Aeson.Value of
			Right _ -> return ()
			Left e -> assertFailure e

test_version :: Assertion
test_version = intmpclonerepo $ do
	git_annex "version" [] "version"
	readonly_query $ git_annex "version" [] "readonly version"

test_sync :: Assertion
test_sync = intmpclonerepo $ do
	git_annex "sync" [] "sync"
	{- Regression test for bug fixed in
	 - 039e83ed5d1a11fd562cce55b8429c840d72443e, where a present
	 - wanted file was dropped. -}
	git_annex "get" [annexedfile] "get"
	git_annex_expectoutput "find" ["--in", "."] [annexedfile]
	git_annex "wanted" [".", "present"] "wanted"
	git_annex "sync" ["--content"] "sync"
	git_annex_expectoutput "find" ["--in", "."] [annexedfile]
	git_annex "drop" [annexedfile] "drop"
	git_annex_expectoutput "find" ["--in", "."] []
	git_annex "sync" ["--content"] "sync"
	git_annex_expectoutput "find" ["--in", "."] []

{- Regression test for the concurrency bug fixed in
 - 667d38a8f11c1ee8f256cdbd80e225c2bae06595 -}
test_concurrent_get_of_dup_key_regression :: Assertion
test_concurrent_get_of_dup_key_regression = intmpclonerepo $ do
	makedup dupfile
	-- This was sufficient currency to trigger the bug.
	git_annex "get" ["-J1", annexedfile, dupfile]
		"concurrent get -J1 with dup"
	git_annex "drop" ["-J1"]
		"drop with dup"
	-- With -J2, one more dup file was needed to trigger the bug.
	makedup dupfile2
	git_annex "get" ["-J2", annexedfile, dupfile, dupfile2]
		"concurrent get -J2 with dup"
	git_annex "drop" ["-J2"]
		"drop with dup"
  where
	dupfile = annexedfile ++ "2"
	dupfile2 = annexedfile ++ "3"
	makedup f = do
		Utility.CopyFile.copyFileExternal Utility.CopyFile.CopyAllMetaData annexedfile f
			@? "copying annexed file failed"
		git "add" [f] "git add"

{- Regression test for union merge bug fixed in
 - 0214e0fb175a608a49b812d81b4632c081f63027 -}
test_union_merge_regression :: Assertion
test_union_merge_regression =
	{- We need 3 repos to see this bug. -}
	withtmpclonerepo $ \r1 ->
		withtmpclonerepo $ \r2 ->
			withtmpclonerepo $ \r3 -> do
				forM_ [r1, r2, r3] $ \r -> indir r $ do
					when (r /= r1) $
						git "remote" ["add", "r1", "../" ++ r1] "remote add"
					when (r /= r2) $
						git "remote" ["add", "r2", "../" ++ r2] "remote add"
					when (r /= r3) $
						git "remote" ["add", "r3", "../" ++ r3] "remote add"
					git_annex "get" [annexedfile] "get"
					git "remote" ["rm", "origin"] "remote rm"
				forM_ [r3, r2, r1] $ \r -> indir r $
					git_annex "sync" [] ("sync in " ++ r)
				forM_ [r3, r2] $ \r -> indir r $
					git_annex "drop" ["--force", annexedfile] ("drop in " ++ r)
				indir r1 $ do
					git_annex "sync" [] "sync in r1"
					git_annex_expectoutput "find" ["--in", "r3"] []
					{- This was the bug. The sync
					 - mangled location log data and it
					 - thought the file was still in r2 -}
					git_annex_expectoutput "find" ["--in", "r2"] []

{- Regression test for the automatic conflict resolution bug fixed
 - in f4ba19f2b8a76a1676da7bb5850baa40d9c388e2. -}
test_conflict_resolution_movein_regression :: Assertion
test_conflict_resolution_movein_regression = withtmpclonerepo $ \r1 -> 
	withtmpclonerepo $ \r2 -> do
		let rname r = if r == r1 then "r1" else "r2"
		forM_ [r1, r2] $ \r -> indir r $ do
			{- Get all files, see check below. -}
			git_annex "get" [] "get"
			disconnectOrigin
		pair r1 r2
		forM_ [r1, r2] $ \r -> indir r $ do
			{- Set up a conflict. -}
			let newcontent = content annexedfile ++ rname r
			git_annex "unlock" [annexedfile] "unlock"
			writecontent annexedfile newcontent
		{- Sync twice in r1 so it gets the conflict resolution
		 - update from r2 -}
		forM_ [r1, r2, r1] $ \r -> indir r $
			git_annex "sync" ["--force"] ("sync in " ++ rname r)
		{- After the sync, it should be possible to get all
		 - files. This includes both sides of the conflict,
		 - although the filenames are not easily predictable.
		 -
		 - The bug caused one repo to be missing the content
		 - of the file that had been put in it. -}
		forM_ [r1, r2] $ \r -> indir r $ do
			git_annex "get" [] ("get all files after merge conflict resolution in " ++ rname r)

{- Simple case of conflict resolution; 2 different versions of annexed
 - file. -}
test_conflict_resolution :: Assertion
test_conflict_resolution = 
	withtmpclonerepo $ \r1 ->
		withtmpclonerepo $ \r2 -> do
			indir r1 $ do
				disconnectOrigin
				writecontent conflictor "conflictor1"
				add_annex conflictor "add conflicter"
				git_annex "sync" [] "sync in r1"
			indir r2 $ do
				disconnectOrigin
				writecontent conflictor "conflictor2"
				add_annex conflictor "add conflicter"
				git_annex "sync" [] "sync in r2"
			pair r1 r2
			forM_ [r1,r2,r1] $ \r -> indir r $
				git_annex "sync" [] "sync"
			checkmerge "r1" r1
			checkmerge "r2" r2
  where
	conflictor = "conflictor"
	variantprefix = conflictor ++ ".variant"
	checkmerge what d = do
		l <- getDirectoryContents d
		let v = filter (variantprefix `isPrefixOf`) l
		length v == 2
			@? (what ++ " not exactly 2 variant files in: " ++ show l)
		conflictor `notElem` l @? ("conflictor still present after conflict resolution")
		indir d $ do
			git_annex "get" v "get"
			git_annex_expectoutput "find" v v

{- Conflict resolution while in an adjusted branch. -}
test_conflict_resolution_adjusted_branch :: Assertion
test_conflict_resolution_adjusted_branch =
	withtmpclonerepo $ \r1 ->
		withtmpclonerepo $ \r2 -> whenM (adjustedbranchsupported r2) $ do
			indir r1 $ do
				disconnectOrigin
				writecontent conflictor "conflictor1"
				add_annex conflictor "add conflicter"
				git_annex "sync" [] "sync in r1"
			indir r2 $ do
				disconnectOrigin
				writecontent conflictor "conflictor2"
				add_annex conflictor "add conflicter"
				git_annex "sync" [] "sync in r2"
				-- We might be in an adjusted branch
				-- already, when eg on a crippled
				-- filesystem. So, --force it.
				git_annex "adjust" ["--unlock", "--force"] "adjust"
			pair r1 r2
			forM_ [r1,r2,r1] $ \r -> indir r $
				git_annex "sync" [] "sync"
			checkmerge "r1" r1
			checkmerge "r2" r2
  where
	conflictor = "conflictor"
	variantprefix = conflictor ++ ".variant"
	checkmerge what d = do
		l <- getDirectoryContents d
		let v = filter (variantprefix `isPrefixOf`) l
		length v == 2
			@? (what ++ " not exactly 2 variant files in: " ++ show l)
		conflictor `notElem` l @? ("conflictor still present after conflict resolution")
		indir d $ do
			git_annex "get" v "get"
			git_annex_expectoutput "find" v v

{- Check merge conflict resolution when one side is an annexed
 - file, and the other is a directory. -}
test_mixed_conflict_resolution :: Assertion
test_mixed_conflict_resolution = do
	check True
	check False
  where
	check inr1 = withtmpclonerepo $ \r1 ->
		withtmpclonerepo $ \r2 -> do
			indir r1 $ do
				disconnectOrigin
				writecontent conflictor "conflictor"
				add_annex conflictor "add conflicter"
				git_annex "sync" [] "sync in r1"
			indir r2 $ do
				disconnectOrigin
				createDirectory conflictor
				writecontent subfile "subfile"
				add_annex conflictor "add conflicter"
				git_annex "sync" [] "sync in r2"
			pair r1 r2
			let l = if inr1 then [r1, r2] else [r2, r1]
			forM_ l $ \r -> indir r $
				git_annex "sync" [] "sync in mixed conflict"
			checkmerge "r1" r1
			checkmerge "r2" r2
	conflictor = "conflictor"
	subfile = conflictor </> "subfile"
	checkmerge what d = do
		doesDirectoryExist (d </> conflictor) 
			@? (d ++ " conflictor directory missing")
		l <- getDirectoryContents d
		let v = filter (Annex.VariantFile.variantMarker `isInfixOf`) l
		not (null v)
			@? (what ++ " conflictor variant file missing in: " ++ show l )
		length v == 1
			@? (what ++ " too many variant files in: " ++ show v)
		indir d $ do
			git_annex "get" (conflictor:v) ("get  in " ++ what)
			git_annex_expectoutput "find" [conflictor] [fromRawFilePath (Git.FilePath.toInternalGitPath (toRawFilePath subfile))]
			git_annex_expectoutput "find" v v

{- Check merge conflict resolution when both repos start with an annexed
 - file; one modifies it, and the other deletes it. -}
test_remove_conflict_resolution :: Assertion
test_remove_conflict_resolution = do
	check True
	check False
  where
	check inr1 = withtmpclonerepo $ \r1 ->
		withtmpclonerepo $ \r2 -> do
			indir r1 $ do
				disconnectOrigin
				writecontent conflictor "conflictor"
				add_annex conflictor "add conflicter"
				git_annex "sync" [] "sync in r1"
			indir r2 $
				disconnectOrigin
			pair r1 r2
			indir r2 $ do
				git_annex "sync" [] "sync in r2"
				git_annex "get" [conflictor] "get conflictor"
				git_annex "unlock" [conflictor] "unlock conflictor"
				writecontent conflictor "newconflictor"
			indir r1 $
				removeWhenExistsWith R.removeLink (toRawFilePath conflictor)
			let l = if inr1 then [r1, r2, r1] else [r2, r1, r2]
			forM_ l $ \r -> indir r $
				git_annex "sync" [] "sync"
			checkmerge "r1" r1
			checkmerge "r2" r2
	conflictor = "conflictor"
	variantprefix = conflictor ++ ".variant"
	checkmerge what d = do
		l <- getDirectoryContents d
		let v = filter (variantprefix `isPrefixOf`) l
		not (null v)
			@? (what ++ " conflictor variant file missing in: " ++ show l )
		length v == 1
			@? (what ++ " too many variant files in: " ++ show v)

{- Check merge confalict resolution when a file is annexed in one repo,
 - and checked directly into git in the other repo.
 -}
test_nonannexed_file_conflict_resolution :: Assertion
test_nonannexed_file_conflict_resolution = do
	check True
	check False
  where
	check inr1 = withtmpclonerepo $ \r1 ->
		withtmpclonerepo $ \r2 -> do
			indir r1 $ do
				disconnectOrigin
				writecontent conflictor "conflictor"
				add_annex conflictor "add conflicter"
				git_annex "sync" [] "sync in r1"
			indir r2 $ do
				disconnectOrigin
				writecontent conflictor nonannexed_content
				git "config"
					[ "annex.largefiles"
					, "exclude=" ++ ingitfile ++ " and exclude=" ++ conflictor
					] "git config annex.largefiles"
				git "add" [conflictor] "git add conflictor"
				git_annex "sync" [] "sync in r2"
			pair r1 r2
			let l = if inr1 then [r1, r2] else [r2, r1]
			forM_ l $ \r -> indir r $
				git_annex "sync" [] "sync"
			checkmerge "r1" r1
			checkmerge "r2" r2
	conflictor = "conflictor"
	nonannexed_content = "nonannexed"
	variantprefix = conflictor ++ ".variant"
	checkmerge what d = do
		l <- getDirectoryContents d
		let v = filter (variantprefix `isPrefixOf`) l
		not (null v)
			@? (what ++ " conflictor variant file missing in: " ++ show l )
		length v == 1
			@? (what ++ " too many variant files in: " ++ show v)
		conflictor `elem` l @? (what ++ " conflictor file missing in: " ++ show l)
		s <- catchMaybeIO (readFile (d </> conflictor))
		s == Just nonannexed_content
			@? (what ++ " wrong content for nonannexed file: " ++ show s)


{- Check merge conflict resolution when a file is annexed in one repo,
 - and is a non-git-annex symlink in the other repo.
 -
 - Test can only run when coreSymlinks is supported, because git needs to
 - be able to check out the non-git-annex symlink.
 -}
test_nonannexed_symlink_conflict_resolution :: Assertion
test_nonannexed_symlink_conflict_resolution = do
	check True
	check False
  where
	check inr1 = withtmpclonerepo $ \r1 ->
		withtmpclonerepo $ \r2 ->
			whenM (checkRepo (Types.coreSymlinks <$> Annex.getGitConfig) r1) $ do
				indir r1 $ do
					disconnectOrigin
					writecontent conflictor "conflictor"
					add_annex conflictor "add conflicter"
					git_annex "sync" [] "sync in r1"
				indir r2 $ do
					disconnectOrigin
					R.createSymbolicLink (toRawFilePath symlinktarget) (toRawFilePath "conflictor")
					git "add" [conflictor] "git add conflictor"
					git_annex "sync" [] "sync in r2"
				pair r1 r2
				let l = if inr1 then [r1, r2] else [r2, r1]
				forM_ l $ \r -> indir r $
					git_annex "sync" [] "sync"
				checkmerge "r1" r1
				checkmerge "r2" r2
	conflictor = "conflictor"
	symlinktarget = "dummy-target"
	variantprefix = conflictor ++ ".variant"
	checkmerge what d = do
		l <- getDirectoryContents d
		let v = filter (variantprefix `isPrefixOf`) l
		not (null v)
			@? (what ++ " conflictor variant file missing in: " ++ show l )
		length v == 1
			@? (what ++ " too many variant files in: " ++ show v)
		conflictor `elem` l @? (what ++ " conflictor file missing in: " ++ show l)
		s <- catchMaybeIO (R.readSymbolicLink (toRawFilePath (d </> conflictor)))
		s == Just (toRawFilePath symlinktarget)
			@? (what ++ " wrong target for nonannexed symlink: " ++ show s)

{- Check merge conflict resolution when there is a local file,
 - that is not staged or committed, that conflicts with what's being added
 - from the remmote.
 -
 - Case 1: Remote adds file named conflictor; local has a file named
 - conflictor.
 -
 - Case 2: Remote adds conflictor/file; local has a file named conflictor.
 -}
test_uncommitted_conflict_resolution :: Assertion
test_uncommitted_conflict_resolution = do
	check conflictor
	check (conflictor </> "file")
  where
	check remoteconflictor = withtmpclonerepo $ \r1 ->
		withtmpclonerepo $ \r2 -> do
			indir r1 $ do
				disconnectOrigin
				createDirectoryIfMissing True (fromRawFilePath (parentDir (toRawFilePath remoteconflictor)))
				writecontent remoteconflictor annexedcontent
				add_annex conflictor "add remoteconflicter"
				git_annex "sync" [] "sync in r1"
			indir r2 $ do
				disconnectOrigin
				writecontent conflictor localcontent
			pair r1 r2
			-- this case is intentionally not handled
			-- since the user can recover on their own easily
			indir r2 $ git_annex_shouldfail "sync" [] "sync should not succeed"
	conflictor = "conflictor"
	localcontent = "local"
	annexedcontent = "annexed"

{- On Windows/FAT, repeated conflict resolution sometimes 
 - lost track of whether a file was a symlink. 
 -}
test_conflict_resolution_symlink_bit :: Assertion
test_conflict_resolution_symlink_bit = unlessM (hasUnlockedFiles <$> getTestMode) $
	withtmpclonerepo $ \r1 ->
		withtmpclonerepo $ \r2 ->
			withtmpclonerepo $ \r3 -> do
				indir r1 $ do
					writecontent conflictor "conflictor"
					git_annex "add" [conflictor] "add conflicter"
					git_annex "sync" [] "sync in r1"
					check_is_link conflictor "r1"
				indir r2 $ do
					createDirectory conflictor
					writecontent (conflictor </> "subfile") "subfile"
					git_annex "add" [conflictor] "add conflicter"
					git_annex "sync" [] "sync in r2"
					check_is_link (conflictor </> "subfile") "r2"
				indir r3 $ do
					writecontent conflictor "conflictor"
					git_annex "add" [conflictor] "add conflicter"
					git_annex "sync" [] "sync in r1"
					check_is_link (conflictor </> "subfile") "r3"
  where
	conflictor = "conflictor"
	check_is_link f what = do
		git_annex_expectoutput "find" ["--include=*", f] [fromRawFilePath (Git.FilePath.toInternalGitPath (toRawFilePath f))]
		l <- annexeval $ Annex.inRepo $ Git.LsTree.lsTreeFiles (Git.LsTree.LsTreeLong False) Git.Ref.headRef [f]
		all (\i -> Git.Types.toTreeItemType (Git.LsTree.mode i) == Just Git.Types.TreeSymlink) l
			@? (what ++ " " ++ f ++ " lost symlink bit after merge: " ++ show l)

{- An unlocked file that conflicts with a locked file should be resolved
 - in favor of the unlocked file, with no variant files, as long as they
 - both point to the same key. -}
test_mixed_lock_conflict_resolution :: Assertion
test_mixed_lock_conflict_resolution = 
	withtmpclonerepo $ \r1 ->
		withtmpclonerepo $ \r2 -> do
			indir r1 $ do
				disconnectOrigin
				writecontent conflictor "conflictor"
				git_annex "add" [conflictor] "add conflicter"
				git_annex "sync" [] "sync in r1"
			indir r2 $ do
				disconnectOrigin
				writecontent conflictor "conflictor"
				git_annex "add" [conflictor] "add conflicter"
				git_annex "unlock" [conflictor] "unlock conflicter"
				git_annex "sync" [] "sync in r2"
			pair r1 r2
			forM_ [r1,r2,r1] $ \r -> indir r $
				git_annex "sync" [] "sync"
			checkmerge "r1" r1
			checkmerge "r2" r2
  where
	conflictor = "conflictor"
	variantprefix = conflictor ++ ".variant"
	checkmerge what d = indir d $ do
		l <- getDirectoryContents "."
		let v = filter (variantprefix `isPrefixOf`) l
		length v == 0
			@? (what ++ " not exactly 0 variant files in: " ++ show l)
		conflictor `elem` l @? ("conflictor not present after conflict resolution")
		git_annex "get" [conflictor] "get"
		git_annex_expectoutput "find" [conflictor] [conflictor]
		-- regular file because it's unlocked
		checkregularfile conflictor

{- Regression test for a bad merge between two adjusted branch repos,
 - where the same file is added to both independently. The bad merge
 - emptied the whole tree. -}
test_adjusted_branch_merge_regression :: Assertion
test_adjusted_branch_merge_regression = do
	withtmpclonerepo $ \r1 ->
		withtmpclonerepo $ \r2 -> whenM (adjustedbranchsupported r1) $ do
			pair r1 r2
			setup r1
			setup r2
			checkmerge "r1" r1
			checkmerge "r2" r2
  where
	conflictor = "conflictor"
	setup r = indir r $ whensupported $ do
		disconnectOrigin
		git_annex "upgrade" [] "upgrade"
		git_annex "adjust" ["--unlock", "--force"] "adjust"
		writecontent conflictor "conflictor"
		git_annex "add" [conflictor] "add conflicter"
		git_annex "sync" [] "sync"
	checkmerge what d = indir d $ whensupported $ do
		git_annex "sync" [] ("sync should not work in " ++ what)
		l <- getDirectoryContents "."
		conflictor `elem` l
			@? ("conflictor not present after merge in " ++ what)
	-- Currently this fails on FAT, for unknown reasons not to
	-- do with what it's intended to test.
	whensupported = unlessM (annexeval Config.crippledFileSystem)

{- Regression test for a bug in adjusted branch syncing code, where adding
 - a subtree to an existing tree lost files. -}
test_adjusted_branch_subtree_regression :: Assertion
test_adjusted_branch_subtree_regression = 
	withtmpclonerepo $ \r -> whenM (adjustedbranchsupported r) $ do
		indir r $ do
			disconnectOrigin
			origbranch <- annexeval origBranch
			git_annex "upgrade" [] "upgrade"
			git_annex "adjust" ["--unlock", "--force"] "adjust"
			createDirectoryIfMissing True "a/b/c"
			writecontent "a/b/c/d" "foo"
			git_annex "add" ["a/b/c"] "add a/b/c"
			git_annex "sync" [] "sync"
			createDirectoryIfMissing True "a/b/x"
			writecontent "a/b/x/y" "foo"
			git_annex "add" ["a/b/x"] "add a/b/x"
			git_annex "sync" [] "sync"
			git "checkout" [origbranch] "git checkout"
			doesFileExist "a/b/x/y" @? ("a/b/x/y missing from master after adjusted branch sync")

test_map :: Assertion
test_map = intmpclonerepo $ do
	-- set descriptions, that will be looked for in the map
	git_annex "describe" [".", "this repo"] "describe 1"
	git_annex "describe" ["origin", "origin repo"] "describe 2"
	-- --fast avoids it running graphviz, not a build dependency
	git_annex "map" ["--fast"] "map"

test_uninit :: Assertion
test_uninit = intmpclonerepo $ do
	git_annex "get" [] "get"
	annexed_present annexedfile
	-- any exit status is accepted; does abnormal exit
	git_annex'' (const True) (const True) "uninit" [] Nothing "uninit"
	checkregularfile annexedfile
	doesDirectoryExist ".git" @? ".git vanished in uninit"

test_uninit_inbranch :: Assertion
test_uninit_inbranch = intmpclonerepo $ do
	git "checkout" ["git-annex"] "git checkout git-annex"
	git_annex_shouldfail "uninit" [] "uninit should not succeed when git-annex branch is checked out"

test_upgrade :: Assertion
test_upgrade = intmpclonerepo $
	git_annex "upgrade" [] "upgrade"

test_whereis :: Assertion
test_whereis = intmpclonerepo $ do
	annexed_notpresent annexedfile
	git_annex "whereis" [annexedfile] "whereis on non-present file"
	readonly_query $ 
		git_annex "whereis" [annexedfile] "readonly whereis on non-present file"
	git_annex "untrust" ["origin"] "untrust"
	git_annex_shouldfail "whereis" [annexedfile] "whereis should exit nonzero on non-present file only present in untrusted repo"
	git_annex "get" [annexedfile] "get"
	annexed_present annexedfile
	git_annex "whereis" [annexedfile] "whereis on present file"
	readonly_query $
		git_annex "whereis" [annexedfile] "readonly whereis on present file"

test_hook_remote :: Assertion
test_hook_remote = intmpclonerepo $ do
#ifndef mingw32_HOST_OS
	git_annex "initremote" (words "foo type=hook encryption=none hooktype=foo") "initremote"
	createDirectory dir
	git_config "annex.foo-store-hook" $
		"cp $ANNEX_FILE " ++ loc
	git_config "annex.foo-retrieve-hook" $
		"cp " ++ loc ++ " $ANNEX_FILE"
	git_config "annex.foo-remove-hook" $
		"rm -f " ++ loc
	git_config "annex.foo-checkpresent-hook" $
		"if [ -e " ++ loc ++ " ]; then echo $ANNEX_KEY; fi"
	git_annex "get" [annexedfile] "get of file"
	annexed_present annexedfile
	git_annex "copy" [annexedfile, "--to", "foo"] "copy --to hook remote"
	annexed_present annexedfile
	git_annex "drop" [annexedfile, "--numcopies=2"] "drop"
	annexed_notpresent annexedfile
	git_annex "move" [annexedfile, "--from", "foo"] "move --from hook remote"
	annexed_present annexedfile
	git_annex_shouldfail "drop" [annexedfile, "--numcopies=2"] "drop should not be allowed with numcopies=2"
	annexed_present annexedfile
  where
	dir = "dir"
	loc = dir ++ "/$ANNEX_KEY"
	git_config k v = git "config" [k, v] "git config"
#else
	-- this test doesn't work in Windows TODO
	noop
#endif

test_directory_remote :: Assertion
test_directory_remote = intmpclonerepo $ do
	createDirectory "dir"
	git_annex "initremote" (words "foo type=directory encryption=none directory=dir") "initremote"
	git_annex "get" [annexedfile] "get of file"
	annexed_present annexedfile
	git_annex "copy" [annexedfile, "--to", "foo"] "copy --to directory remote"
	annexed_present annexedfile
	git_annex "drop" [annexedfile, "--numcopies=2"] "drop"
	annexed_notpresent annexedfile
	git_annex "move" [annexedfile, "--from", "foo"] "move --from directory remote"
	annexed_present annexedfile
	git_annex_shouldfail "drop" [annexedfile, "--numcopies=2"] "drop should not be allowed with numcopies=2"
	annexed_present annexedfile

test_rsync_remote :: Assertion
test_rsync_remote = intmpclonerepo $ do
#ifndef mingw32_HOST_OS
	createDirectory "dir"
	git_annex "initremote" (words "foo type=rsync encryption=none rsyncurl=dir") "initremote"
	git_annex "get" [annexedfile] "get of file"
	annexed_present annexedfile
	git_annex "copy" [annexedfile, "--to", "foo"] "copy --to rsync remote"
	annexed_present annexedfile
	git_annex "drop" [annexedfile, "--numcopies=2"] "drop"
	annexed_notpresent annexedfile
	git_annex "move" [annexedfile, "--from", "foo"] "move --from rsync remote"
	annexed_present annexedfile
	git_annex_shouldfail "drop" [annexedfile, "--numcopies=2"] "drop should not be allowed with numcopies=2"
	annexed_present annexedfile
#else
	noop
#endif

test_bup_remote :: Assertion
test_bup_remote = intmpclonerepo $ when BuildInfo.bup $ do
	-- bup special remote needs an absolute path
	dir <- fromRawFilePath <$> absPath (toRawFilePath "dir")
	createDirectory dir
	git_annex "initremote" (words $ "foo type=bup encryption=none buprepo="++dir) "initremote"
	git_annex "get" [annexedfile] "get of file"
	annexed_present annexedfile
	git_annex "copy" [annexedfile, "--to", "foo"] "copy --to bup remote"
	annexed_present annexedfile
	git_annex "drop" [annexedfile, "--numcopies=2"] "drop"
	annexed_notpresent annexedfile
	git_annex "copy" [annexedfile, "--from", "foo"] "copy --from bup remote"
	annexed_present annexedfile
	git_annex "move" [annexedfile, "--from", "foo"] "move --from bup remote"
	annexed_present annexedfile

test_borg_remote :: Assertion
test_borg_remote = when BuildInfo.borg $ do
	borgdirparent <- fromRawFilePath <$> (absPath . toRawFilePath =<< tmprepodir)
	let borgdir = borgdirparent </> "borgrepo"
	intmpclonerepo $ do
		testProcess "borg" ["init", borgdir, "-e", "none"] Nothing (== True) (const True) "borg init"
		testProcess "borg" ["create", borgdir++"::backup1", "."] Nothing (== True) (const True) "borg create"

		git_annex "initremote" (words $ "borg type=borg borgrepo="++borgdir) "initremote"
		git_annex "sync" ["borg"] "sync borg"
		git_annex_expectoutput "find" ["--in=borg"] []

		git_annex "get" [annexedfile] "get of file"
		annexed_present annexedfile
		git_annex_expectoutput "find" ["--in=borg"] []
		
		testProcess "borg" ["create", borgdir++"::backup2", "."] Nothing (== True) (const True) "borg create"
		git_annex "sync" ["borg"] "sync borg after getting file"
		git_annex_expectoutput "find" ["--in=borg"] [annexedfile]

		git "remote" ["rm", "origin"] "remote rm"
		git_annex_shouldfail "drop" [annexedfile]
			"drop from borg succeeded, but it should be untrusted by default"
		git_annex "enableremote" ["borg", "appendonly=yes"] "enableremote appendonly"		
		git_annex "drop" [annexedfile] "drop from borg (appendonly)"
		git_annex "get" [annexedfile, "--from=borg"] "get from borg"

-- gpg is not a build dependency, so only test when it's available
test_crypto :: Assertion
#ifndef mingw32_HOST_OS
test_crypto = do
	testscheme "shared"
	testscheme "hybrid"
	testscheme "pubkey"
  where
	gpgcmd = Utility.Gpg.mkGpgCmd Nothing
	testscheme scheme = Utility.Tmp.Dir.withTmpDir "gpgtmp" $ \gpgtmp -> do
		-- Use the system temp directory as gpg temp directory because 
		-- it needs to be able to store the agent socket there,
		-- which can be problimatic when testing some filesystems.
		absgpgtmp <- fromRawFilePath <$> absPath (toRawFilePath gpgtmp)
		res <- testscheme' scheme absgpgtmp
		-- gpg may still be running and would prevent
		-- removeDirectoryRecursive from succeeding, so
		-- force removal of the temp directory.
		liftIO $ removeDirectoryForCleanup gpgtmp
		return res
	testscheme' scheme absgpgtmp = intmpclonerepo $ do
		-- Since gpg uses a unix socket, which is limited to a
		-- short path, use whichever is shorter of absolute
		-- or relative path.
		relgpgtmp <- fromRawFilePath <$> relPathCwdToFile (toRawFilePath absgpgtmp)
		let gpgtmp = if length relgpgtmp < length absgpgtmp
			then relgpgtmp 
			else absgpgtmp
		void $ Utility.Gpg.testHarness gpgtmp gpgcmd $ \environ -> do
			createDirectory "dir"
			let initps =
				[ "foo"
				, "type=directory"
				, "encryption=" ++ scheme
				, "directory=dir"
				, "highRandomQuality=false"
				] ++ if scheme `elem` ["hybrid","pubkey"]
					then ["keyid=" ++ Utility.Gpg.testKeyId]
					else []
			git_annex' "initremote" initps (Just environ) "initremote"
			git_annex_shouldfail' "initremote" initps (Just environ) "initremote should not work when run twice in a row"
			git_annex' "enableremote" initps (Just environ) "enableremote"
			git_annex' "enableremote" initps (Just environ) "enableremote when run twice in a row"
			git_annex' "get" [annexedfile] (Just environ) "get of file"
			annexed_present annexedfile
			git_annex' "copy" [annexedfile, "--to", "foo"] (Just environ) "copy --to encrypted remote"
			(c,k) <- annexeval $ do
				uuid <- Remote.nameToUUID "foo"
				rs <- Logs.Remote.readRemoteLog
				Just k <- Annex.WorkTree.lookupKey (toRawFilePath annexedfile)
				return (fromJust $ M.lookup uuid rs, k)
			let key = if scheme `elem` ["hybrid","pubkey"]
					then Just $ Utility.Gpg.KeyIds [Utility.Gpg.testKeyId]
					else Nothing
			testEncryptedRemote environ scheme key c [k] @? "invalid crypto setup"
	
			annexed_present annexedfile
			git_annex' "drop" [annexedfile, "--numcopies=2"] (Just environ) "drop"
			annexed_notpresent annexedfile
			git_annex' "move" [annexedfile, "--from", "foo"] (Just environ) "move --from encrypted remote"
			annexed_present annexedfile
			git_annex_shouldfail' "drop" [annexedfile, "--numcopies=2"] (Just environ) "drop should not be allowed with numcopies=2"
			annexed_present annexedfile
	{- Ensure the configuration complies with the encryption scheme, and
	 - that all keys are encrypted properly for the given directory remote. -}
	testEncryptedRemote environ scheme ks c keys = case Remote.Helper.Encryptable.extractCipher pc of
		Just cip@Crypto.SharedCipher{} | scheme == "shared" && isNothing ks ->
			checkKeys cip Nothing
		Just cip@(Crypto.EncryptedCipher encipher v ks')
			| checkScheme v && keysMatch ks' ->
				checkKeys cip (Just v) <&&> checkCipher encipher ks'
		_ -> return False
	  where
		pc = either (const (Types.Remote.ParsedRemoteConfig mempty mempty)) id $
			Remote.Helper.Encryptable.parseEncryptionConfig c
		keysMatch (Utility.Gpg.KeyIds ks') =
			maybe False (\(Utility.Gpg.KeyIds ks2) ->
					sort (nub ks2) == sort (nub ks')) ks
		checkCipher encipher = Utility.Gpg.checkEncryptionStream gpgcmd (Just environ) encipher . Just
		checkScheme Types.Crypto.Hybrid = scheme == "hybrid"
		checkScheme Types.Crypto.PubKey = scheme == "pubkey"
		checkKeys cip mvariant = do
			dummycfg <- Types.GitConfig.dummyRemoteGitConfig
			let encparams = (Types.Remote.ParsedRemoteConfig mempty mempty, dummycfg)
			cipher <- Crypto.decryptCipher' gpgcmd (Just environ) encparams cip
			files <- filterM doesFileExist $
				map ("dir" </>) $ concatMap (serializeKeys cipher) keys
			return (not $ null files) <&&> allM (checkFile mvariant) files
		checkFile mvariant filename =
			Utility.Gpg.checkEncryptionFile gpgcmd (Just environ) filename $
				if mvariant == Just Types.Crypto.PubKey then ks else Nothing
		serializeKeys cipher = map fromRawFilePath . 
			Annex.Locations.keyPaths .
			Crypto.encryptKey Types.Crypto.HmacSha1 cipher
#else
test_crypto = putStrLn "gpg testing not implemented on Windows"
#endif

test_add_subdirs :: Assertion
test_add_subdirs = intmpclonerepo $ do
	createDirectory "dir"
	writecontent ("dir" </> "foo") $ "dir/" ++ content annexedfile
	git_annex "add" ["dir"] "add of subdir"

	{- Regression test for Windows bug where symlinks were not
	 - calculated correctly for files in subdirs. -}
	unlessM (hasUnlockedFiles <$> getTestMode) $ do
		git_annex "sync" [] "sync"
		l <- annexeval $ Utility.FileSystemEncoding.decodeBL
			<$> Annex.CatFile.catObject (Git.Types.Ref (encodeBS "HEAD:dir/foo"))
		"../.git/annex/" `isPrefixOf` l @? ("symlink from subdir to .git/annex is wrong: " ++ l)

	createDirectory "dir2"
	writecontent ("dir2" </> "foo") $ content annexedfile
	setCurrentDirectory "dir"
	git_annex "add" [".." </> "dir2"] "add of ../subdir"

test_addurl :: Assertion
test_addurl = intmpclonerepo $ do
	-- file:// only; this test suite should not hit the network
	let filecmd c ps = git_annex c ("-cannex.security.allowed-url-schemes=file" : ps)
	f <- fromRawFilePath <$> absPath (toRawFilePath "myurl")
	let url = replace "\\" "/" ("file:///" ++ dropDrive f)
	writecontent f "foo"
	git_annex_shouldfail "addurl" [url] "addurl should not work on file url"
	filecmd "addurl" [url] ("addurl on " ++ url)
	let dest = "addurlurldest"
	filecmd "addurl" ["--file", dest, url] ("addurl on " ++ url ++ "  with --file")
	doesFileExist dest @? (dest ++ " missing after addurl --file")

test_export_import :: Assertion
test_export_import = intmpclonerepo $ do
	createDirectory "dir"
	git_annex "initremote" (words "foo type=directory encryption=none directory=dir exporttree=yes importtree=yes") "initremote"
	git_annex "get" [] "get of files"
	annexed_present annexedfile

	-- Nothing to commit, but this makes sure the master branch
	-- is in sync with the adjusted branch, which it may not be
	-- depending on how the repository was set up.
	commitchanges
	origbranch <- annexeval origBranch
	git_annex "export" [origbranch, "--to", "foo"] "export to dir"
	dircontains annexedfile (content annexedfile)

	writedir "import" (content "import")
	git_annex "import" [origbranch, "--from", "foo"] "import from dir"
	git_annex "merge" ["foo/" ++ origbranch] "git annex merge"
	annexed_present_imported "import"

	removeWhenExistsWith R.removeLink (toRawFilePath "import")
	writecontent "import" (content "newimport1")
	git_annex "add" ["import"] "add of import"
	commitchanges
	git_annex "export" [origbranch, "--to", "foo"] "export modified file to dir"
	dircontains "import" (content "newimport1")

	-- verify that export refuses to overwrite modified file
	writedir "import" (content "newimport2")
	removeWhenExistsWith R.removeLink (toRawFilePath "import")
	writecontent "import" (content "newimport3")
	git_annex "add" ["import"] "add of import"
	commitchanges
	git_annex_shouldfail "export" [origbranch, "--to", "foo"] "export should not work in conflict"
	dircontains "import" (content "newimport2")

	-- resolving import conflict
	git_annex "import" [origbranch, "--from", "foo"] "import from dir"
	git_shouldfail "merge" ["foo/master", "-mmerge"] "git merge of conflict should exit nonzero"
	removeWhenExistsWith R.removeLink (toRawFilePath "import")
	writecontent "import" (content "newimport3")
	git_annex "add" ["import"] "add of import"
	commitchanges
	git_annex "export" [origbranch, "--to", "foo"] "export after import conflict"
	dircontains "import" (content "newimport3")
  where
	dircontains f v = 
		((v==) <$> readFile ("dir" </> f))
			@? ("did not find expected content of " ++ "dir" </> f)
	writedir f = writecontent ("dir" </> f)
	-- When on an adjusted branch, this updates the master branch
	-- to match it, which is necessary since the master branch is going
	-- to be exported.
	commitchanges = git_annex "sync" ["--no-pull", "--no-push"] "sync"

test_export_import_subdir :: Assertion
test_export_import_subdir = intmpclonerepo $ do
	createDirectory "dir"
	git_annex "initremote" (words "foo type=directory encryption=none directory=dir exporttree=yes importtree=yes") "initremote"
	git_annex "get" [] "get of files"
	annexed_present annexedfile

	createDirectory subdir
	git "mv" [annexedfile, subannexedfile] "git mv"
	git "commit" ["-m", "moved"] "git commit"
	
	-- When on an adjusted branch, this updates the master branch
	-- to match it, which is necessary since the master branch is going
	-- to be exported.
	git_annex "sync" ["--no-pull", "--no-push"] "sync"

	-- Run three times because there was a bug that took a couple
	-- of runs to lead to the wrong tree being written to the remote
	-- tracking branch.
	testimport
	testexport
	testimport
	testexport
	testimport
	testexport
  where
	dircontains f v = 
		((v==) <$> readFile ("dir" </> f))
			@? ("did not find expected content of " ++ "dir" </> f)
	
	subdir = "subdir"
	subannexedfile = "subdir" </> annexedfile
	
	testexport = do
		origbranch <- annexeval origBranch
		git_annex "export" [origbranch++":"++subdir, "--to", "foo"] "export of subdir"
		dircontains annexedfile (content annexedfile)
	
	testimport = do
		origbranch <- annexeval origBranch
		git_annex "import" [origbranch++":"++subdir, "--from", "foo"] "import of subdir"
		git_annex "merge" ["foo/master", "--allow-unrelated-histories"] "git annex merge foo/master"

		-- Make sure that import did not import the file to the top
		-- of the repo.
		checkdoesnotexist annexedfile

test_transition_propagation_reversion :: Assertion
test_transition_propagation_reversion =
	withtmpclonerepo $ \r1 ->
		withtmpclonerepo $ \r2 -> do
			pair r1 r2
			indir r1 $ do
				disconnectOrigin
				writecontent wormannexedfile $ content wormannexedfile
				git_annex "add" [wormannexedfile, "--backend=WORM"] "add"
				git_annex "sync" [] "sync"
			indir r2 $ do
				disconnectOrigin
				git_annex "sync" [] "sync"
			indir r1 $ do
				git_annex "sync" [] "sync"
			indir r2 $ do
				git_annex "get" [wormannexedfile] "get"
				git_annex "drop" [wormannexedfile] "drop"
				git_annex "get" [wormannexedfile] "get"
				git_annex "drop" [wormannexedfile] "drop"
			indir r1 $ do
				git_annex "drop" ["--force", wormannexedfile] "drop"
				git_annex "sync" [] "sync"
				git_annex "forget" ["--force"] "forget"
				git_annex "sync" [] "sync"
				emptylog
			indir r2 $ do
				git_annex "sync" [] "sync"
				emptylog
			indir r1 $ do
				git_annex "sync" [] "sync"
				emptylog
  where
	emptylog = git_annex_expectoutput "log" [wormannexedfile] []
