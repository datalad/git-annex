{- git-annex test suite
 -
 - Copyright 2010-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Test where

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Ingredients.Rerun
import Data.Monoid

import Options.Applicative hiding (command)
import Control.Exception.Extensible
import qualified Data.Map as M
import System.IO.HVFS (SystemFS(..))
import qualified Text.JSON
import System.Path

import Common

import qualified Utility.SafeCommand
import qualified Annex
import qualified Annex.UUID
import qualified Backend
import qualified Git.CurrentRepo
import qualified Git.Filename
import qualified Locations
import qualified Types.KeySource
import qualified Types.Backend
import qualified Types.TrustLevel
import qualified Types
import qualified Logs
import qualified Logs.UUIDBased
import qualified Logs.Trust
import qualified Logs.Remote
import qualified Logs.Unused
import qualified Logs.Transfer
import qualified Logs.Presence
import qualified Remote
import qualified Types.Key
import qualified Types.Messages
import qualified Config
import qualified Config.Cost
import qualified Crypto
import qualified Annex.Init
import qualified Utility.Path
import qualified Utility.FileMode
import qualified Build.SysConfig
import qualified Utility.Format
import qualified Utility.Verifiable
import qualified Utility.Process
import qualified Utility.Misc
import qualified Utility.InodeCache
import qualified Utility.Env
import qualified Utility.Matcher
import qualified Utility.Exception
import qualified Utility.Hash
import qualified Utility.Scheduled
import qualified Utility.HumanTime
#ifndef mingw32_HOST_OS
import qualified CmdLine.GitAnnex as GitAnnex
import qualified Remote.Helper.Encryptable
import qualified Types.Crypto
import qualified Utility.Gpg
#endif

type TestEnv = M.Map String String

main :: [String] -> IO ()
main ps = do
	let tests = testGroup "Tests"
		-- Test both direct and indirect mode.
		-- Windows is only going to use direct mode,
		-- so don't test twice.
		[ properties
#ifndef mingw32_HOST_OS
		, withTestEnv True $ unitTests "(direct)"
		, withTestEnv False $ unitTests "(indirect)"
#else
		, withTestEnv False $ unitTests ""
#endif
		]

	-- Can't use tasty's defaultMain because one of the command line
	-- parameters is "test".
	let pinfo = info (helper <*> suiteOptionParser ingredients tests)
		( fullDesc <> header "Builtin test suite" )
	opts <- either (\f -> error =<< errMessage f "git-annex test") return $
		execParserPure (prefs idm) pinfo ps
	case tryIngredients ingredients opts tests of
		Nothing -> error "No tests found!?"
		Just act -> ifM act
			( exitSuccess
			, do
				putStrLn "  (This could be due to a bug in git-annex, or an incompatability"
				putStrLn "   with utilities, such as git, installed on this system.)"
				exitFailure
			)

ingredients :: [Ingredient]
ingredients =
	[ rerunningTests [consoleTestReporter]
	, listingTests
	]

properties :: TestTree
properties = localOption (QuickCheckTests 1000) $ testGroup "QuickCheck"
	[ testProperty "prop_idempotent_deencode_git" Git.Filename.prop_idempotent_deencode
	, testProperty "prop_idempotent_deencode" Utility.Format.prop_idempotent_deencode
	, testProperty "prop_idempotent_fileKey" Locations.prop_idempotent_fileKey
	, testProperty "prop_idempotent_key_encode" Types.Key.prop_idempotent_key_encode
	, testProperty "prop_idempotent_key_decode" Types.Key.prop_idempotent_key_decode
	, testProperty "prop_idempotent_shellEscape" Utility.SafeCommand.prop_idempotent_shellEscape
	, testProperty "prop_idempotent_shellEscape_multiword" Utility.SafeCommand.prop_idempotent_shellEscape_multiword
	, testProperty "prop_logs_sane" Logs.prop_logs_sane
	, testProperty "prop_idempotent_configEscape" Logs.Remote.prop_idempotent_configEscape
	, testProperty "prop_parse_show_Config" Logs.Remote.prop_parse_show_Config
	, testProperty "prop_parentDir_basics" Utility.Path.prop_parentDir_basics
	, testProperty "prop_relPathDirToFile_basics" Utility.Path.prop_relPathDirToFile_basics
	, testProperty "prop_relPathDirToFile_regressionTest" Utility.Path.prop_relPathDirToFile_regressionTest
	, testProperty "prop_cost_sane" Config.Cost.prop_cost_sane
	, testProperty "prop_matcher_sane" Utility.Matcher.prop_matcher_sane
	, testProperty "prop_HmacSha1WithCipher_sane" Crypto.prop_HmacSha1WithCipher_sane
	, testProperty "prop_TimeStamp_sane" Logs.UUIDBased.prop_TimeStamp_sane
	, testProperty "prop_addLog_sane" Logs.UUIDBased.prop_addLog_sane
	, testProperty "prop_verifiable_sane" Utility.Verifiable.prop_verifiable_sane
	, testProperty "prop_segment_regressionTest" Utility.Misc.prop_segment_regressionTest
	, testProperty "prop_read_write_transferinfo" Logs.Transfer.prop_read_write_transferinfo
	, testProperty "prop_read_show_inodecache" Utility.InodeCache.prop_read_show_inodecache
	, testProperty "prop_parse_show_log" Logs.Presence.prop_parse_show_log
	, testProperty "prop_read_show_TrustLevel" Types.TrustLevel.prop_read_show_TrustLevel
	, testProperty "prop_parse_show_TrustLog" Logs.Trust.prop_parse_show_TrustLog
	, testProperty "prop_hashes_stable" Utility.Hash.prop_hashes_stable
	, testProperty "prop_schedule_roundtrips" Utility.Scheduled.prop_schedule_roundtrips
	, testProperty "prop_duration_roundtrips" Utility.HumanTime.prop_duration_roundtrips
	]

{- These tests set up the test environment, but also test some basic parts
 - of git-annex. They are always run before the unitTests. -}
initTests :: TestEnv -> TestTree
initTests env = testGroup ("Init Tests")
	[ check "init" test_init
	, check "add" test_add
	]
  where
	check desc t = testCase desc (t env)

unitTests :: String -> IO TestEnv -> TestTree
unitTests note getenv = testGroup ("Unit Tests " ++ note)
	[ check "add sha1dup" test_add_sha1dup
	, check "add extras" test_add_extras
	, check "add subdirs" test_add_subdirs
	, check "reinject" test_reinject
	, check "unannex (no copy)" test_unannex_nocopy
	, check "unannex (with copy)" test_unannex_withcopy
	, check "drop (no remote)" test_drop_noremote
	, check "drop (with remote)" test_drop_withremote
	, check "drop (untrusted remote)" test_drop_untrustedremote
	, check "get" test_get
	, check "move" test_move
	, check "copy" test_copy
	, check "lock" test_lock
	, check "edit (no pre-commit)" test_edit
	, check "edit (pre-commit)" test_edit_precommit
	, check "fix" test_fix
	, check "trust" test_trust
	, check "fsck (basics)" test_fsck_basic
	, check "fsck (bare)" test_fsck_bare
	, check "fsck (local untrusted)" test_fsck_localuntrusted
	, check "fsck (remote untrusted)" test_fsck_remoteuntrusted
	, check "migrate" test_migrate
	, check "migrate (via gitattributes)" test_migrate_via_gitattributes
	, check" unused" test_unused
	, check "describe" test_describe
	, check "find" test_find
	, check "merge" test_merge
	, check "info" test_info
	, check "version" test_version
	, check "sync" test_sync
	, check "union merge regression" test_union_merge_regression
	, check "conflict resolution" test_conflict_resolution_movein_bug
	, check "conflict_resolution (mixed directory and file)" test_mixed_conflict_resolution
	, check "map" test_map
	, check "uninit" test_uninit
	, check "uninit (in git-annex branch)" test_uninit_inbranch
	, check "upgrade" test_upgrade
	, check "whereis" test_whereis
	, check "hook remote" test_hook_remote
	, check "directory remote" test_directory_remote
	, check "rsync remote" test_rsync_remote
	, check "bup remote" test_bup_remote
	, check "crypto" test_crypto
	, check "preferred content" test_preferred_content
	]
  where
	check desc t = testCase desc (getenv >>= t)

-- this test case create the main repo
test_init :: TestEnv -> Assertion
test_init env = innewrepo env $ do
	git_annex env "init" [reponame] @? "init failed"
	handleforcedirect env
  where
	reponame = "test repo"

-- this test case runs in the main repo, to set up a basic
-- annexed file that later tests will use
test_add :: TestEnv -> Assertion
test_add env = inmainrepo env $ do
	writeFile annexedfile $ content annexedfile
	git_annex env "add" [annexedfile] @? "add failed"
	annexed_present annexedfile
	writeFile sha1annexedfile $ content sha1annexedfile
	git_annex env "add" [sha1annexedfile, "--backend=SHA1"] @? "add with SHA1 failed"
	annexed_present sha1annexedfile
	checkbackend sha1annexedfile backendSHA1
	ifM (annexeval Config.isDirect)
		( do
			writeFile ingitfile $ content ingitfile
			not <$> boolSystem "git" [Param "add", File ingitfile] @? "git add failed to fail in direct mode"
			boolSystem "rm" [Params "-f", File ingitfile] @? "rm failed"
			git_annex env "sync" [] @? "sync failed"
		, do
			writeFile ingitfile $ content ingitfile
			boolSystem "git" [Param "add", File ingitfile] @? "git add failed"
			boolSystem "git" [Params "commit -q -m commit"] @? "git commit failed"
			git_annex env "add" [ingitfile] @? "add ingitfile should be no-op"
			unannexed ingitfile
		)

test_add_sha1dup :: TestEnv -> Assertion
test_add_sha1dup env = intmpclonerepo env $ do
	writeFile sha1annexedfiledup $ content sha1annexedfiledup
	git_annex env "add" [sha1annexedfiledup, "--backend=SHA1"] @? "add of second file with same SHA1 failed"
	annexed_present sha1annexedfiledup
	annexed_present sha1annexedfile

test_add_extras :: TestEnv -> Assertion
test_add_extras env = intmpclonerepo env $ do
	writeFile wormannexedfile $ content wormannexedfile
	git_annex env "add" [wormannexedfile, "--backend=WORM"] @? "add with WORM failed"
	annexed_present wormannexedfile
	checkbackend wormannexedfile backendWORM

test_add_subdirs :: TestEnv -> Assertion
test_add_subdirs env = intmpclonerepo env $ do
	createDirectory "dir"
	writeFile ("dir" </> "foo") $ content annexedfile
	git_annex env "add" ["dir"] @? "add of subdir failed"
	createDirectory "dir2"
	writeFile ("dir2" </> "foo") $ content annexedfile
#ifndef mingw32_HOST_OS
	{- This does not work on Windows, for whatever reason. -}
	setCurrentDirectory "dir"
	git_annex env "add" [".." </> "dir2"] @? "add of ../subdir failed"
#endif

test_reinject :: TestEnv -> Assertion
test_reinject env = intmpclonerepoInDirect env $ do
	git_annex env "drop" ["--force", sha1annexedfile] @? "drop failed"
	writeFile tmp $ content sha1annexedfile
	r <- annexeval $ Types.Backend.getKey backendSHA1 $
		Types.KeySource.KeySource { Types.KeySource.keyFilename = tmp, Types.KeySource.contentLocation = tmp, Types.KeySource.inodeCache = Nothing }
	let key = Types.Key.key2file $ fromJust r
	git_annex env "reinject" [tmp, sha1annexedfile] @? "reinject failed"
	git_annex env "fromkey" [key, sha1annexedfiledup] @? "fromkey failed for dup"
	annexed_present sha1annexedfiledup
  where
	tmp = "tmpfile"

test_unannex_nocopy :: TestEnv -> Assertion
test_unannex_nocopy env = intmpclonerepo env $ do
	annexed_notpresent annexedfile
	git_annex env "unannex" [annexedfile] @? "unannex failed with no copy"
	annexed_notpresent annexedfile

test_unannex_withcopy :: TestEnv -> Assertion
test_unannex_withcopy env = intmpclonerepo env $ do
	git_annex env "get" [annexedfile] @? "get failed"
	annexed_present annexedfile
	git_annex env "unannex" [annexedfile, sha1annexedfile] @? "unannex failed"
	unannexed annexedfile
	git_annex env "unannex" [annexedfile] @? "unannex failed on non-annexed file"
	unannexed annexedfile
	unlessM (annexeval Config.isDirect) $ do
		git_annex env "unannex" [ingitfile] @? "unannex ingitfile should be no-op"
		unannexed ingitfile

test_drop_noremote :: TestEnv -> Assertion
test_drop_noremote env = intmpclonerepo env $ do
	git_annex env "get" [annexedfile] @? "get failed"
	boolSystem "git" [Params "remote rm origin"]
		@? "git remote rm origin failed"
	not <$> git_annex env "drop" [annexedfile] @? "drop wrongly succeeded with no known copy of file"
	annexed_present annexedfile
	git_annex env "drop" ["--force", annexedfile] @? "drop --force failed"
	annexed_notpresent annexedfile
	git_annex env "drop" [annexedfile] @? "drop of dropped file failed"
	unlessM (annexeval Config.isDirect) $ do
		git_annex env "drop" [ingitfile] @? "drop ingitfile should be no-op"
		unannexed ingitfile

test_drop_withremote :: TestEnv -> Assertion
test_drop_withremote env = intmpclonerepo env $ do
	git_annex env "get" [annexedfile] @? "get failed"
	annexed_present annexedfile
	git_annex env "numcopies" ["2"] @? "numcopies config failed"
	not <$> git_annex env "drop" [annexedfile] @? "drop succeeded although numcopies is not satisfied"
	git_annex env "numcopies" ["1"] @? "numcopies config failed"
	git_annex env "drop" [annexedfile] @? "drop failed though origin has copy"
	annexed_notpresent annexedfile
	inmainrepo env $ annexed_present annexedfile

test_drop_untrustedremote :: TestEnv -> Assertion
test_drop_untrustedremote env = intmpclonerepo env $ do
	git_annex env "untrust" ["origin"] @? "untrust of origin failed"
	git_annex env "get" [annexedfile] @? "get failed"
	annexed_present annexedfile
	not <$> git_annex env "drop" [annexedfile] @? "drop wrongly suceeded with only an untrusted copy of the file"
	annexed_present annexedfile
	inmainrepo env $ annexed_present annexedfile

test_get :: TestEnv -> Assertion
test_get env = intmpclonerepo env $ do
	inmainrepo env $ annexed_present annexedfile
	annexed_notpresent annexedfile
	git_annex env "get" [annexedfile] @? "get of file failed"
	inmainrepo env $ annexed_present annexedfile
	annexed_present annexedfile
	git_annex env "get" [annexedfile] @? "get of file already here failed"
	inmainrepo env $ annexed_present annexedfile
	annexed_present annexedfile
	unlessM (annexeval Config.isDirect) $ do
		inmainrepo env $ unannexed ingitfile
		unannexed ingitfile
		git_annex env "get" [ingitfile] @? "get ingitfile should be no-op"
		inmainrepo env $ unannexed ingitfile
		unannexed ingitfile

test_move :: TestEnv -> Assertion
test_move env = intmpclonerepo env $ do
	annexed_notpresent annexedfile
	inmainrepo env $ annexed_present annexedfile
	git_annex env "move" ["--from", "origin", annexedfile] @? "move --from of file failed"
	annexed_present annexedfile
	inmainrepo env $ annexed_notpresent annexedfile
	git_annex env "move" ["--from", "origin", annexedfile] @? "move --from of file already here failed"
	annexed_present annexedfile
	inmainrepo env $ annexed_notpresent annexedfile
	git_annex env "move" ["--to", "origin", annexedfile] @? "move --to of file failed"
	inmainrepo env $ annexed_present annexedfile
	annexed_notpresent annexedfile
	git_annex env "move" ["--to", "origin", annexedfile] @? "move --to of file already there failed"
	inmainrepo env $ annexed_present annexedfile
	annexed_notpresent annexedfile
	unlessM (annexeval Config.isDirect) $ do
		unannexed ingitfile
		inmainrepo env $ unannexed ingitfile
		git_annex env "move" ["--to", "origin", ingitfile] @? "move of ingitfile should be no-op"
		unannexed ingitfile
		inmainrepo env $ unannexed ingitfile
		git_annex env "move" ["--from", "origin", ingitfile] @? "move of ingitfile should be no-op"
		unannexed ingitfile
		inmainrepo env $ unannexed ingitfile

test_copy :: TestEnv -> Assertion
test_copy env = intmpclonerepo env $ do
	annexed_notpresent annexedfile
	inmainrepo env $ annexed_present annexedfile
	git_annex env "copy" ["--from", "origin", annexedfile] @? "copy --from of file failed"
	annexed_present annexedfile
	inmainrepo env $ annexed_present annexedfile
	git_annex env "copy" ["--from", "origin", annexedfile] @? "copy --from of file already here failed"
	annexed_present annexedfile
	inmainrepo env $ annexed_present annexedfile
	git_annex env "copy" ["--to", "origin", annexedfile] @? "copy --to of file already there failed"
	annexed_present annexedfile
	inmainrepo env $ annexed_present annexedfile
	git_annex env "move" ["--to", "origin", annexedfile] @? "move --to of file already there failed"
	annexed_notpresent annexedfile
	inmainrepo env $ annexed_present annexedfile
	unlessM (annexeval Config.isDirect) $ do
		unannexed ingitfile
		inmainrepo env $ unannexed ingitfile
		git_annex env "copy" ["--to", "origin", ingitfile] @? "copy of ingitfile should be no-op"
		unannexed ingitfile
		inmainrepo env $ unannexed ingitfile
		git_annex env "copy" ["--from", "origin", ingitfile] @? "copy of ingitfile should be no-op"
		checkregularfile ingitfile
		checkcontent ingitfile

test_preferred_content :: TestEnv -> Assertion
test_preferred_content env = intmpclonerepo env $ do
	annexed_notpresent annexedfile
	-- get --auto only looks at numcopies when preferred content is not
	-- set, and with 1 copy existing, does not get the file.
	git_annex env "get" ["--auto", annexedfile] @? "get --auto of file failed with default preferred content"
	annexed_notpresent annexedfile

	git_annex env "wanted" [".", "standard"] @? "set expression to standard failed"
	git_annex env "group" [".", "client"] @? "set group to standard failed"
	git_annex env "get" ["--auto", annexedfile] @? "get --auto of file failed for client"
	annexed_present annexedfile
	git_annex env "ungroup" [".", "client"] @? "ungroup failed"

	git_annex env "wanted" [".", "standard"] @? "set expression to standard failed"
	git_annex env "group" [".", "manual"] @? "set group to manual failed"
	-- drop --auto with manual leaves the file where it is
	git_annex env "drop" ["--auto", annexedfile] @? "drop --auto of file failed with manual preferred content"
	annexed_present annexedfile
	git_annex env "drop" [annexedfile] @? "drop of file failed"
	annexed_notpresent annexedfile
	-- get --auto with manual does not get the file
	git_annex env "get" ["--auto", annexedfile] @? "get --auto of file failed with manual preferred content"
	annexed_notpresent annexedfile
	git_annex env "ungroup" [".", "client"] @? "ungroup failed"
	
	git_annex env "wanted" [".", "exclude=*"] @? "set expression to exclude=* failed"
	git_annex env "get" [annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex env "drop" ["--auto", annexedfile] @? "drop --auto of file failed with exclude=*"
	annexed_notpresent annexedfile
	git_annex env "get" ["--auto", annexedfile] @? "get --auto of file failed with exclude=*"
	annexed_notpresent annexedfile

test_lock :: TestEnv -> Assertion
test_lock env = intmpclonerepoInDirect env $ do
	-- regression test: unlock of not present file should skip it
	annexed_notpresent annexedfile
	not <$> git_annex env "unlock" [annexedfile] @? "unlock failed to fail with not present file"
	annexed_notpresent annexedfile

	git_annex env "get" [annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex env "unlock" [annexedfile] @? "unlock failed"		
	unannexed annexedfile
	-- write different content, to verify that lock
	-- throws it away
	changecontent annexedfile
	writeFile annexedfile $ content annexedfile ++ "foo"
	not <$> git_annex env "lock" [annexedfile] @? "lock failed to fail without --force"
	git_annex env "lock" ["--force", annexedfile] @? "lock --force failed"
	annexed_present annexedfile
	git_annex env "unlock" [annexedfile] @? "unlock failed"		
	unannexed annexedfile
	changecontent annexedfile
	git_annex env "add" [annexedfile] @? "add of modified file failed"
	runchecks [checklink, checkunwritable] annexedfile
	c <- readFile annexedfile
	assertEqual "content of modified file" c (changedcontent annexedfile)
	r' <- git_annex env "drop" [annexedfile]
	not r' @? "drop wrongly succeeded with no known copy of modified file"

test_edit :: TestEnv -> Assertion
test_edit = test_edit' False

test_edit_precommit :: TestEnv -> Assertion
test_edit_precommit = test_edit' True

test_edit' :: Bool -> TestEnv -> Assertion
test_edit' precommit env = intmpclonerepoInDirect env $ do
	git_annex env "get" [annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex env "edit" [annexedfile] @? "edit failed"
	unannexed annexedfile
	changecontent annexedfile
	boolSystem "git" [Param "add", File annexedfile]
		@? "git add of edited file failed"
	if precommit
		then git_annex env "pre-commit" []
			@? "pre-commit failed"
		else boolSystem "git" [Params "commit -q -m contentchanged"]
			@? "git commit of edited file failed"
	runchecks [checklink, checkunwritable] annexedfile
	c <- readFile annexedfile
	assertEqual "content of modified file" c (changedcontent annexedfile)
	not <$> git_annex env "drop" [annexedfile] @? "drop wrongly succeeded with no known copy of modified file"

test_fix :: TestEnv -> Assertion
test_fix env = intmpclonerepoInDirect env $ do
	annexed_notpresent annexedfile
	git_annex env "fix" [annexedfile] @? "fix of not present failed"
	annexed_notpresent annexedfile
	git_annex env "get" [annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex env "fix" [annexedfile] @? "fix of present file failed"
	annexed_present annexedfile
	createDirectory subdir
	boolSystem "git" [Param "mv", File annexedfile, File subdir]
		@? "git mv failed"
	git_annex env "fix" [newfile] @? "fix of moved file failed"
	runchecks [checklink, checkunwritable] newfile
	c <- readFile newfile
	assertEqual "content of moved file" c (content annexedfile)
  where
	subdir = "s"
	newfile = subdir ++ "/" ++ annexedfile

test_trust :: TestEnv -> Assertion
test_trust env = intmpclonerepo env $ do
	git_annex env "trust" [repo] @? "trust failed"
	trustcheck Logs.Trust.Trusted "trusted 1"
	git_annex env "trust" [repo] @? "trust of trusted failed"
	trustcheck Logs.Trust.Trusted "trusted 2"
	git_annex env "untrust" [repo] @? "untrust failed"
	trustcheck Logs.Trust.UnTrusted "untrusted 1"
	git_annex env "untrust" [repo] @? "untrust of untrusted failed"
	trustcheck Logs.Trust.UnTrusted "untrusted 2"
	git_annex env "dead" [repo] @? "dead failed"
	trustcheck Logs.Trust.DeadTrusted "deadtrusted 1"
	git_annex env "dead" [repo] @? "dead of dead failed"
	trustcheck Logs.Trust.DeadTrusted "deadtrusted 2"
	git_annex env "semitrust" [repo] @? "semitrust failed"
	trustcheck Logs.Trust.SemiTrusted "semitrusted 1"
	git_annex env "semitrust" [repo] @? "semitrust of semitrusted failed"
	trustcheck Logs.Trust.SemiTrusted "semitrusted 2"
  where
	repo = "origin"
	trustcheck expected msg = do
		present <- annexeval $ do
			l <- Logs.Trust.trustGet expected
			u <- Remote.nameToUUID repo
			return $ u `elem` l
		assertBool msg present

test_fsck_basic :: TestEnv -> Assertion
test_fsck_basic env = intmpclonerepo env $ do
	git_annex env "fsck" [] @? "fsck failed"
	git_annex env "numcopies" ["2"] @? "numcopies config failed"
	fsck_should_fail env "numcopies unsatisfied"
	git_annex env "numcopies" ["1"] @? "numcopies config failed"
	corrupt annexedfile
	corrupt sha1annexedfile
  where
	corrupt f = do
		git_annex env "get" [f] @? "get of file failed"
		Utility.FileMode.allowWrite f
		writeFile f (changedcontent f)
		ifM (annexeval Config.isDirect)
			( git_annex env "fsck" [] @? "fsck failed in direct mode with changed file content"
			, not <$> git_annex env "fsck" [] @? "fsck failed to fail with corrupted file content"
			)
		git_annex env "fsck" [] @? "fsck unexpectedly failed again; previous one did not fix problem with " ++ f

test_fsck_bare :: TestEnv -> Assertion
test_fsck_bare env = intmpbareclonerepo env $ do
	git_annex env "fsck" [] @? "fsck failed"

test_fsck_localuntrusted :: TestEnv -> Assertion
test_fsck_localuntrusted env = intmpclonerepo env $ do
	git_annex env "get" [annexedfile] @? "get failed"
	git_annex env "untrust" ["origin"] @? "untrust of origin repo failed"
	git_annex env "untrust" ["."] @? "untrust of current repo failed"
	fsck_should_fail env "content only available in untrusted (current) repository"
	git_annex env "trust" ["."] @? "trust of current repo failed"
	git_annex env "fsck" [annexedfile] @? "fsck failed on file present in trusted repo"

test_fsck_remoteuntrusted :: TestEnv -> Assertion
test_fsck_remoteuntrusted env = intmpclonerepo env $ do
	git_annex env "numcopies" ["2"] @? "numcopies config failed"
	git_annex env "get" [annexedfile] @? "get failed"
	git_annex env "get" [sha1annexedfile] @? "get failed"
	git_annex env "fsck" [] @? "fsck failed with numcopies=2 and 2 copies"
	git_annex env "untrust" ["origin"] @? "untrust of origin failed"
	fsck_should_fail env "content not replicated to enough non-untrusted repositories"

fsck_should_fail :: TestEnv -> String -> Assertion
fsck_should_fail env m = not <$> git_annex env "fsck" []
	@? "fsck failed to fail with " ++ m

test_migrate :: TestEnv -> Assertion
test_migrate = test_migrate' False

test_migrate_via_gitattributes :: TestEnv -> Assertion
test_migrate_via_gitattributes = test_migrate' True

test_migrate' :: Bool -> TestEnv -> Assertion
test_migrate' usegitattributes env = intmpclonerepoInDirect env $ do
	annexed_notpresent annexedfile
	annexed_notpresent sha1annexedfile
	git_annex env "migrate" [annexedfile] @? "migrate of not present failed"
	git_annex env "migrate" [sha1annexedfile] @? "migrate of not present failed"
	git_annex env "get" [annexedfile] @? "get of file failed"
	git_annex env "get" [sha1annexedfile] @? "get of file failed"
	annexed_present annexedfile
	annexed_present sha1annexedfile
	if usegitattributes
		then do
			writeFile ".gitattributes" $ "* annex.backend=SHA1"
			git_annex env "migrate" [sha1annexedfile]
				@? "migrate sha1annexedfile failed"
			git_annex env "migrate" [annexedfile]
				@? "migrate annexedfile failed"
		else do
			git_annex env "migrate" [sha1annexedfile, "--backend", "SHA1"]
				@? "migrate sha1annexedfile failed"
			git_annex env "migrate" [annexedfile, "--backend", "SHA1"]
				@? "migrate annexedfile failed"
	annexed_present annexedfile
	annexed_present sha1annexedfile
	checkbackend annexedfile backendSHA1
	checkbackend sha1annexedfile backendSHA1

	-- check that reversing a migration works
	writeFile ".gitattributes" $ "* annex.backend=SHA256"
	git_annex env "migrate" [sha1annexedfile]
		@? "migrate sha1annexedfile failed"
	git_annex env "migrate" [annexedfile]
		@? "migrate annexedfile failed"
	annexed_present annexedfile
	annexed_present sha1annexedfile
	checkbackend annexedfile backendSHA256
	checkbackend sha1annexedfile backendSHA256

test_unused :: TestEnv -> Assertion
-- This test is broken in direct mode
test_unused env = intmpclonerepoInDirect env $ do
	-- keys have to be looked up before files are removed
	annexedfilekey <- annexeval $ findkey annexedfile
	sha1annexedfilekey <- annexeval $ findkey sha1annexedfile
	git_annex env "get" [annexedfile] @? "get of file failed"
	git_annex env "get" [sha1annexedfile] @? "get of file failed"
	checkunused [] "after get"
	boolSystem "git" [Params "rm -fq", File annexedfile] @? "git rm failed"
	checkunused [] "after rm"
	boolSystem "git" [Params "commit -q -m foo"] @? "git commit failed"
	checkunused [] "after commit"
	-- unused checks origin/master; once it's gone it is really unused
	boolSystem "git" [Params "remote rm origin"] @? "git remote rm origin failed"
	checkunused [annexedfilekey] "after origin branches are gone"
	boolSystem "git" [Params "rm -fq", File sha1annexedfile] @? "git rm failed"
	boolSystem "git" [Params "commit -q -m foo"] @? "git commit failed"
	checkunused [annexedfilekey, sha1annexedfilekey] "after rm sha1annexedfile"

	-- good opportunity to test dropkey also
	git_annex env "dropkey" ["--force", Types.Key.key2file annexedfilekey]
		@? "dropkey failed"
	checkunused [sha1annexedfilekey] ("after dropkey --force " ++ Types.Key.key2file annexedfilekey)

	not <$> git_annex env "dropunused" ["1"] @? "dropunused failed to fail without --force"
	git_annex env "dropunused" ["--force", "1"] @? "dropunused failed"
	checkunused [] "after dropunused"
	not <$> git_annex env "dropunused" ["--force", "10", "501"] @? "dropunused failed to fail on bogus numbers"

	-- unused used to miss symlinks that were not staged and pointed 
	-- at annexed content, and think that content was unused
	writeFile "unusedfile" "unusedcontent"
	git_annex env "add" ["unusedfile"] @? "add of unusedfile failed"
	unusedfilekey <- annexeval $ findkey "unusedfile"
	renameFile "unusedfile" "unusedunstagedfile"
	boolSystem "git" [Params "rm -qf", File "unusedfile"] @? "git rm failed"
	checkunused [] "with unstaged link"
	removeFile "unusedunstagedfile"
	checkunused [unusedfilekey] "with unstaged link deleted"

	-- unused used to miss symlinks that were deleted or modified
	-- manually, but commited as such.
	writeFile "unusedfile" "unusedcontent"
	git_annex env "add" ["unusedfile"] @? "add of unusedfile failed"
	boolSystem "git" [Param "add", File "unusedfile"] @? "git add failed"
	unusedfilekey' <- annexeval $ findkey "unusedfile"
	checkunused [] "with staged deleted link"
	boolSystem "git" [Params "rm -qf", File "unusedfile"] @? "git rm failed"
	checkunused [unusedfilekey'] "with staged link deleted"

	-- unused used to miss symlinks that were deleted or modified
	-- manually, but not staged as such.
	writeFile "unusedfile" "unusedcontent"
	git_annex env "add" ["unusedfile"] @? "add of unusedfile failed"
	boolSystem "git" [Param "add", File "unusedfile"] @? "git add failed"
	unusedfilekey'' <- annexeval $ findkey "unusedfile"
	checkunused [] "with unstaged deleted link"
	removeFile "unusedfile"
	checkunused [unusedfilekey''] "with unstaged link deleted"

  where
	checkunused expectedkeys desc = do
		git_annex env "unused" [] @? "unused failed"
		unusedmap <- annexeval $ Logs.Unused.readUnusedMap ""
		let unusedkeys = M.elems unusedmap
		assertEqual ("unused keys differ " ++ desc)
			(sort expectedkeys) (sort unusedkeys)
	findkey f = do
		r <- Backend.lookupFile f
		return $ fst $ fromJust r

test_describe :: TestEnv -> Assertion
test_describe env = intmpclonerepo env $ do
	git_annex env "describe" [".", "this repo"] @? "describe 1 failed"
	git_annex env "describe" ["origin", "origin repo"] @? "describe 2 failed"

test_find :: TestEnv -> Assertion
test_find env = intmpclonerepo env $ do
	annexed_notpresent annexedfile
	git_annex_expectoutput env "find" [] []
	git_annex env "get" [annexedfile] @? "get failed"
	annexed_present annexedfile
	annexed_notpresent sha1annexedfile
	git_annex_expectoutput env "find" [] [annexedfile]
	git_annex_expectoutput env "find" ["--exclude", annexedfile, "--and", "--exclude", sha1annexedfile] []
	git_annex_expectoutput env "find" ["--include", annexedfile] [annexedfile]
	git_annex_expectoutput env "find" ["--not", "--in", "origin"] []
	git_annex_expectoutput env "find" ["--copies", "1", "--and", "--not", "--copies", "2"] [sha1annexedfile]
	git_annex_expectoutput env "find" ["--inbackend", "SHA1"] [sha1annexedfile]
	git_annex_expectoutput env "find" ["--inbackend", "WORM"] []

	{- --include=* should match files in subdirectories too,
	 - and --exclude=* should exclude them. -}
	createDirectory "dir"
	writeFile "dir/subfile" "subfile"
	git_annex env "add" ["dir"] @? "add of subdir failed"
	git_annex_expectoutput env "find" ["--include", "*", "--exclude", annexedfile, "--exclude", sha1annexedfile] ["dir/subfile"]
	git_annex_expectoutput env "find" ["--exclude", "*"] []

test_merge :: TestEnv -> Assertion
test_merge env = intmpclonerepo env $ do
	git_annex env "merge" [] @? "merge failed"

test_info :: TestEnv -> Assertion
test_info env = intmpclonerepo env $ do
	json <- git_annex_output env "info" ["--json"]
	case Text.JSON.decodeStrict json :: Text.JSON.Result (Text.JSON.JSObject Text.JSON.JSValue) of
		Text.JSON.Ok _ -> return ()
		Text.JSON.Error e -> assertFailure e

test_version :: TestEnv -> Assertion
test_version env = intmpclonerepo env $ do
	git_annex env "version" [] @? "version failed"

test_sync :: TestEnv -> Assertion
test_sync env = intmpclonerepo env $ do
	git_annex env "sync" [] @? "sync failed"
	{- Regression test for bug fixed in 
	 - 7b0970b340d7faeb745c666146c7f701ec71808f, where in direct mode
	 - sync committed the symlink standin file to the annex. -}
	git_annex_expectoutput env "find" ["--in", "."] []

{- Regression test for union merge bug fixed in
 - 0214e0fb175a608a49b812d81b4632c081f63027 -}
test_union_merge_regression :: TestEnv -> Assertion
test_union_merge_regression env =
	{- We need 3 repos to see this bug. -}
	withtmpclonerepo env False $ \r1 -> do
		withtmpclonerepo env False $ \r2 -> do
			withtmpclonerepo env False $ \r3 -> do
				forM_ [r1, r2, r3] $ \r -> indir env r $ do
					when (r /= r1) $
						boolSystem "git" [Params "remote add r1", File ("../../" ++ r1)] @? "remote add"
					when (r /= r2) $
						boolSystem "git" [Params "remote add r2", File ("../../" ++ r2)] @? "remote add"
					when (r /= r3) $
						boolSystem "git" [Params "remote add r3", File ("../../" ++ r3)] @? "remote add"
					git_annex env "get" [annexedfile] @? "get failed"
					boolSystem "git" [Params "remote rm origin"] @? "remote rm"
				forM_ [r3, r2, r1] $ \r -> indir env r $
					git_annex env "sync" [] @? "sync failed"
				forM_ [r3, r2] $ \r -> indir env r $
					git_annex env "drop" ["--force", annexedfile] @? "drop failed"
				indir env r1 $ do
					git_annex env "sync" [] @? "sync failed in r1"
					git_annex_expectoutput env "find" ["--in", "r3"] []
					{- This was the bug. The sync
					 - mangled location log data and it
					 - thought the file was still in r2 -}
					git_annex_expectoutput env "find" ["--in", "r2"] []

{- Regression test for the automatic conflict resolution bug fixed
 - in f4ba19f2b8a76a1676da7bb5850baa40d9c388e2. -}
test_conflict_resolution_movein_bug :: TestEnv -> Assertion
test_conflict_resolution_movein_bug env = withtmpclonerepo env False $ \r1 -> do
	withtmpclonerepo env False $ \r2 -> do
		let rname r = if r == r1 then "r1" else "r2"
		forM_ [r1, r2] $ \r -> indir env r $ do
			{- Get all files, see check below. -}
			git_annex env "get" [] @? "get failed"
		pair env r1 r2
		forM_ [r1, r2] $ \r -> indir env r $ do
			{- Set up a conflict. -}
			let newcontent = content annexedfile ++ rname r
			ifM (annexeval Config.isDirect)
				( writeFile annexedfile newcontent
				, do
					git_annex env "unlock" [annexedfile] @? "unlock failed"		
					writeFile annexedfile newcontent
				)
		{- Sync twice in r1 so it gets the conflict resolution
		 - update from r2 -}
		forM_ [r1, r2, r1] $ \r -> indir env r $ do
			git_annex env "sync" ["--force"] @? "sync failed in " ++ rname r
		{- After the sync, it should be possible to get all
		 - files. This includes both sides of the conflict,
		 - although the filenames are not easily predictable.
		 -
		 - The bug caused, in direct mode, one repo to
		 - be missing the content of the file that had
		 - been put in it. -}
		forM_ [r1, r2] $ \r -> indir env r $ do
		 	git_annex env "get" [] @? "unable to get all files after merge conflict resolution in " ++ rname r

{- Check merge conflict resolution when one side is an annexed
 - file, and the other is a directory. -}
test_mixed_conflict_resolution :: TestEnv -> Assertion
test_mixed_conflict_resolution env = do
	check_mixed_conflict True
	check_mixed_conflict False
  where
	check_mixed_conflict inr1 = withtmpclonerepo env False $ \r1 ->
		withtmpclonerepo env False $ \r2 -> do
			indir env r1 $ do
				writeFile conflictor "conflictor"
				git_annex env "add" [conflictor] @? "add conflicter failed"
				git_annex env "sync" [] @? "sync failed"
			indir env r2 $ do
				createDirectory conflictor
				writeFile (conflictor </> "subfile") "subfile"
				git_annex env "add" [conflictor] @? "add conflicter failed"
				git_annex env "sync" [] @? "sync failed"
			pair env r1 r2
			let r = if inr1 then r1 else r2
			indir env r $ do
				git_annex env "sync" [] @? "sync failed in mixed conflict"
			checkmerge r1
			checkmerge r2
	  where
		conflictor = "conflictor"
		variantprefix = conflictor ++ ".variant"
		checkmerge d = do
			doesDirectoryExist (d </> conflictor) @? (d ++ " conflictor directory missing")
			(any (variantprefix `isPrefixOf`) 
				<$> getDirectoryContents d)
				@? (d ++ "conflictor file missing")

{- Set up repos as remotes of each other;
 - remove origin since we're going to sync
 - some changes to a file. -}
pair :: TestEnv -> FilePath -> FilePath -> Assertion
pair env r1 r2 = forM_ [r1, r2] $ \r -> indir env r $ do
	when (r /= r1) $
		boolSystem "git" [Params "remote add r1", File ("../../" ++ r1)] @? "remote add"
	when (r /= r2) $
		boolSystem "git" [Params "remote add r2", File ("../../" ++ r2)] @? "remote add"
	boolSystem "git" [Params "remote rm origin"] @? "remote rm"

test_map :: TestEnv -> Assertion
test_map env = intmpclonerepo env $ do
	-- set descriptions, that will be looked for in the map
	git_annex env "describe" [".", "this repo"] @? "describe 1 failed"
	git_annex env "describe" ["origin", "origin repo"] @? "describe 2 failed"
	-- --fast avoids it running graphviz, not a build dependency
	git_annex env "map" ["--fast"] @? "map failed"

test_uninit :: TestEnv -> Assertion
test_uninit env = intmpclonerepo env $ do
	git_annex env "get" [] @? "get failed"
	annexed_present annexedfile
	_ <- git_annex env "uninit" [] -- exit status not checked; does abnormal exit
	checkregularfile annexedfile
	doesDirectoryExist ".git" @? ".git vanished in uninit"

test_uninit_inbranch :: TestEnv -> Assertion
test_uninit_inbranch env = intmpclonerepoInDirect env $ do
	boolSystem "git" [Params "checkout git-annex"] @? "git checkout git-annex"
	not <$> git_annex env "uninit" [] @? "uninit failed to fail when git-annex branch was checked out"

test_upgrade :: TestEnv -> Assertion
test_upgrade env = intmpclonerepo env $ do
	git_annex env "upgrade" [] @? "upgrade from same version failed"

test_whereis :: TestEnv -> Assertion
test_whereis env = intmpclonerepo env $ do
	annexed_notpresent annexedfile
	git_annex env "whereis" [annexedfile] @? "whereis on non-present file failed"
	git_annex env "untrust" ["origin"] @? "untrust failed"
	not <$> git_annex env "whereis" [annexedfile] @? "whereis on non-present file only present in untrusted repo failed to fail"
	git_annex env "get" [annexedfile] @? "get failed"
	annexed_present annexedfile
	git_annex env "whereis" [annexedfile] @? "whereis on present file failed"

test_hook_remote :: TestEnv -> Assertion
test_hook_remote env = intmpclonerepo env $ do
#ifndef mingw32_HOST_OS
	git_annex env "initremote" (words "foo type=hook encryption=none hooktype=foo") @? "initremote failed"
	createDirectory dir
	git_config "annex.foo-store-hook" $
		"cp $ANNEX_FILE " ++ loc
	git_config "annex.foo-retrieve-hook" $
		"cp " ++ loc ++ " $ANNEX_FILE"
	git_config "annex.foo-remove-hook" $
		"rm -f " ++ loc
	git_config "annex.foo-checkpresent-hook" $
		"if [ -e " ++ loc ++ " ]; then echo $ANNEX_KEY; fi"
	git_annex env "get" [annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex env "copy" [annexedfile, "--to", "foo"] @? "copy --to hook remote failed"
	annexed_present annexedfile
	git_annex env "drop" [annexedfile, "--numcopies=2"] @? "drop failed"
	annexed_notpresent annexedfile
	git_annex env "move" [annexedfile, "--from", "foo"] @? "move --from hook remote failed"
	annexed_present annexedfile
	not <$> git_annex env "drop" [annexedfile, "--numcopies=2"] @? "drop failed to fail"
	annexed_present annexedfile
  where
	dir = "dir"
	loc = dir ++ "/$ANNEX_KEY"
	git_config k v = boolSystem "git" [Param "config", Param k, Param v]
		@? "git config failed"
#else
	-- this test doesn't work in Windows TODO
	noop
#endif

test_directory_remote :: TestEnv -> Assertion
test_directory_remote env = intmpclonerepo env $ do
	createDirectory "dir"
	git_annex env "initremote" (words $ "foo type=directory encryption=none directory=dir") @? "initremote failed"
	git_annex env "get" [annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex env "copy" [annexedfile, "--to", "foo"] @? "copy --to directory remote failed"
	annexed_present annexedfile
	git_annex env "drop" [annexedfile, "--numcopies=2"] @? "drop failed"
	annexed_notpresent annexedfile
	git_annex env "move" [annexedfile, "--from", "foo"] @? "move --from directory remote failed"
	annexed_present annexedfile
	not <$> git_annex env "drop" [annexedfile, "--numcopies=2"] @? "drop failed to fail"
	annexed_present annexedfile

test_rsync_remote :: TestEnv -> Assertion
test_rsync_remote env = intmpclonerepo env $ do
#ifndef mingw32_HOST_OS
	createDirectory "dir"
	git_annex env "initremote" (words $ "foo type=rsync encryption=none rsyncurl=dir") @? "initremote failed"
	git_annex env "get" [annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex env "copy" [annexedfile, "--to", "foo"] @? "copy --to rsync remote failed"
	annexed_present annexedfile
	git_annex env "drop" [annexedfile, "--numcopies=2"] @? "drop failed"
	annexed_notpresent annexedfile
	git_annex env "move" [annexedfile, "--from", "foo"] @? "move --from rsync remote failed"
	annexed_present annexedfile
	not <$> git_annex env "drop" [annexedfile, "--numcopies=2"] @? "drop failed to fail"
	annexed_present annexedfile
#else
	-- this test doesn't work in Windows TODO
	noop
#endif

test_bup_remote :: TestEnv -> Assertion
test_bup_remote env = intmpclonerepo env $ when Build.SysConfig.bup $ do
	dir <- absPath "dir" -- bup special remote needs an absolute path
	createDirectory dir
	git_annex env "initremote" (words $ "foo type=bup encryption=none buprepo="++dir) @? "initremote failed"
	git_annex env "get" [annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex env "copy" [annexedfile, "--to", "foo"] @? "copy --to bup remote failed"
	annexed_present annexedfile
	git_annex env "drop" [annexedfile, "--numcopies=2"] @? "drop failed"
	annexed_notpresent annexedfile
	git_annex env "copy" [annexedfile, "--from", "foo"] @? "copy --from bup remote failed"
	annexed_present annexedfile
	not <$> git_annex env "move" [annexedfile, "--from", "foo"] @? "move --from bup remote failed to fail"
	annexed_present annexedfile

-- gpg is not a build dependency, so only test when it's available
test_crypto :: TestEnv -> Assertion
#ifndef mingw32_HOST_OS
test_crypto env = do
	testscheme "shared"
	testscheme "hybrid"
	testscheme "pubkey"
  where
	testscheme scheme = intmpclonerepo env $ whenM (Utility.Path.inPath Utility.Gpg.gpgcmd) $ do
		Utility.Gpg.testTestHarness @? "test harness self-test failed"
		Utility.Gpg.testHarness $ do
			createDirectory "dir"
			let a cmd = git_annex env cmd $
				[ "foo"
				, "type=directory"
				, "encryption=" ++ scheme
				, "directory=dir"
				, "highRandomQuality=false"
				] ++ if scheme `elem` ["hybrid","pubkey"]
					then ["keyid=" ++ Utility.Gpg.testKeyId]
					else []
			a "initremote" @? "initremote failed"
			not <$> a "initremote" @? "initremote failed to fail when run twice in a row"
			a "enableremote" @? "enableremote failed"
			a "enableremote" @? "enableremote failed when run twice in a row"
			git_annex env "get" [annexedfile] @? "get of file failed"
			annexed_present annexedfile
			git_annex env "copy" [annexedfile, "--to", "foo"] @? "copy --to encrypted remote failed"
			(c,k) <- annexeval $ do
				uuid <- Remote.nameToUUID "foo"
				rs <- Logs.Remote.readRemoteLog
				Just (k,_) <- Backend.lookupFile annexedfile
				return (fromJust $ M.lookup uuid rs, k)
			let key = if scheme `elem` ["hybrid","pubkey"]
					then Just $ Utility.Gpg.KeyIds [Utility.Gpg.testKeyId]
					else Nothing
			testEncryptedRemote scheme key c [k] @? "invalid crypto setup"
	
			annexed_present annexedfile
			git_annex env "drop" [annexedfile, "--numcopies=2"] @? "drop failed"
			annexed_notpresent annexedfile
			git_annex env "move" [annexedfile, "--from", "foo"] @? "move --from encrypted remote failed"
			annexed_present annexedfile
			not <$> git_annex env "drop" [annexedfile, "--numcopies=2"] @? "drop failed to fail"
			annexed_present annexedfile
	{- Ensure the configuration complies with the encryption scheme, and
	 - that all keys are encrypted properly for the given directory remote. -}
	testEncryptedRemote scheme ks c keys = case Remote.Helper.Encryptable.extractCipher c of
		Just cip@Crypto.SharedCipher{} | scheme == "shared" && isNothing ks ->
			checkKeys cip Nothing
		Just cip@(Crypto.EncryptedCipher encipher v ks')
			| checkScheme v && keysMatch ks' ->
				checkKeys cip (Just v) <&&> checkCipher encipher ks'
		_ -> return False
	  where
		keysMatch (Utility.Gpg.KeyIds ks') =
			maybe False (\(Utility.Gpg.KeyIds ks2) ->
					sort (nub ks2) == sort (nub ks')) ks
		checkCipher encipher = Utility.Gpg.checkEncryptionStream encipher . Just
		checkScheme Types.Crypto.Hybrid = scheme == "hybrid"
		checkScheme Types.Crypto.PubKey = scheme == "pubkey"
		checkKeys cip mvariant = do
			cipher <- Crypto.decryptCipher cip
			files <- filterM doesFileExist $
				map ("dir" </>) $ concatMap (key2files cipher) keys
			return (not $ null files) <&&> allM (checkFile mvariant) files
		checkFile mvariant filename =
			Utility.Gpg.checkEncryptionFile filename $
				if mvariant == Just Types.Crypto.PubKey then ks else Nothing
		key2files cipher = Locations.keyPaths .
			Crypto.encryptKey Types.Crypto.HmacSha1 cipher
#else
test_crypto _env = putStrLn "gpg testing not implemented on Windows"
#endif

-- This is equivilant to running git-annex, but it's all run in-process
-- (when the OS allows) so test coverage collection works.
git_annex :: TestEnv -> String -> [String] -> IO Bool
git_annex env command params = do
#ifndef mingw32_HOST_OS
	forM_ (M.toList env) $ \(var, val) ->
		Utility.Env.setEnv var val True

	-- catch all errors, including normally fatal errors
	r <- try (run)::IO (Either SomeException ())
	case r of
		Right _ -> return True
		Left _ -> return False
  where
	run = GitAnnex.run (command:"-q":params)
#else
	Utility.SafeCommand.boolSystemEnv "git-annex"
		(map Param $ command : params)
		(Just $ M.toList env)
#endif

{- Runs git-annex and returns its output. -}
git_annex_output :: TestEnv -> String -> [String] -> IO String
git_annex_output env command params = do
	got <- Utility.Process.readProcessEnv "git-annex" (command:params)
		(Just $ M.toList env)
	-- XXX since the above is a separate process, code coverage stats are
	-- not gathered for things run in it.
	-- Run same command again, to get code coverage.
	_ <- git_annex env command params
	return got

git_annex_expectoutput :: TestEnv -> String -> [String] -> [String] -> IO ()
git_annex_expectoutput env command params expected = do
	got <- lines <$> git_annex_output env command params
	got == expected @? ("unexpected value running " ++ command ++ " " ++ show params ++ " -- got: " ++ show got ++ " expected: " ++ show expected)

-- Runs an action in the current annex. Note that shutdown actions
-- are not run; this should only be used for actions that query state.
annexeval :: Types.Annex a -> IO a
annexeval a = do
	s <- Annex.new =<< Git.CurrentRepo.get
	Annex.eval s $ do
		Annex.setOutput Types.Messages.QuietOutput
		a

innewrepo :: TestEnv -> Assertion -> Assertion
innewrepo env a = withgitrepo env $ \r -> indir env r a

inmainrepo :: TestEnv -> Assertion -> Assertion
inmainrepo env a = indir env mainrepodir a

intmpclonerepo :: TestEnv -> Assertion -> Assertion
intmpclonerepo env a = withtmpclonerepo env False $ \r -> indir env r a

intmpclonerepoInDirect :: TestEnv -> Assertion -> Assertion
intmpclonerepoInDirect env a = intmpclonerepo env $
	ifM isdirect
		( putStrLn "not supported in direct mode; skipping"
		, a
		)
  where
  	isdirect = annexeval $ do
		Annex.Init.initialize Nothing
		Config.isDirect

intmpbareclonerepo :: TestEnv -> Assertion -> Assertion
intmpbareclonerepo env a = withtmpclonerepo env True $ \r -> indir env r a

withtmpclonerepo :: TestEnv -> Bool -> (FilePath -> Assertion) -> Assertion
withtmpclonerepo env bare a = do
	dir <- tmprepodir
	bracket (clonerepo env mainrepodir dir bare) cleanup a

withgitrepo :: TestEnv -> (FilePath -> Assertion) -> Assertion
withgitrepo env = bracket (setuprepo env mainrepodir) return

indir :: TestEnv -> FilePath -> Assertion -> Assertion
indir env dir a = do
	cwd <- getCurrentDirectory
	-- Assertion failures throw non-IO errors; catch
	-- any type of error and change back to cwd before
	-- rethrowing.
	r <- bracket_ (changeToTmpDir env dir) (setCurrentDirectory cwd)
		(try (a)::IO (Either SomeException ()))
	case r of
		Right () -> return ()
		Left e -> throw e

setuprepo :: TestEnv -> FilePath -> IO FilePath
setuprepo env dir = do
	cleanup dir
	ensuretmpdir
	boolSystem "git" [Params "init -q", File dir] @? "git init failed"
	indir env dir $ do
		boolSystem "git" [Params "config user.name", Param "Test User"] @? "git config failed"
		boolSystem "git" [Params "config user.email test@example.com"] @? "git config failed"
	return dir

-- clones are always done as local clones; we cannot test ssh clones
clonerepo :: TestEnv -> FilePath -> FilePath -> Bool -> IO FilePath
clonerepo env old new bare = do
	cleanup new
	ensuretmpdir
	let b = if bare then " --bare" else ""
	boolSystem "git" [Params ("clone -q" ++ b), File old, File new] @? "git clone failed"
	indir env new $
		git_annex env "init" ["-q", new] @? "git annex init failed"
	when (not bare) $
		indir env new $
			handleforcedirect env
	return new

handleforcedirect :: TestEnv -> IO ()
handleforcedirect env = when (M.lookup "FORCEDIRECT" env == Just "1") $
	git_annex env "direct" ["-q"] @? "git annex direct failed"
	
ensuretmpdir :: IO ()
ensuretmpdir = do
	e <- doesDirectoryExist tmpdir
	unless e $
		createDirectory tmpdir

cleanup :: FilePath -> IO ()
cleanup dir = do
	e <- doesDirectoryExist dir
	when e $ do
		-- git-annex prevents annexed file content from being
		-- removed via directory permissions; undo
		recurseDir SystemFS dir >>=
			filterM doesDirectoryExist >>=
				mapM_ Utility.FileMode.allowWrite
		-- For unknown reasons, this sometimes fails on Windows.
		void $ tryIO $ removeDirectoryRecursive dir
	
checklink :: FilePath -> Assertion
checklink f = do
	s <- getSymbolicLinkStatus f
	-- in direct mode, it may be a symlink, or not, depending
	-- on whether the content is present.
	unlessM (annexeval Config.isDirect) $
		isSymbolicLink s @? f ++ " is not a symlink"

checkregularfile :: FilePath -> Assertion
checkregularfile f = do
	s <- getSymbolicLinkStatus f
	isRegularFile s @? f ++ " is not a normal file"
	return ()

checkcontent :: FilePath -> Assertion
checkcontent f = do
	c <- Utility.Exception.catchDefaultIO "could not read file" $ readFile f
	assertEqual ("checkcontent " ++ f) (content f) c

checkunwritable :: FilePath -> Assertion
checkunwritable f = unlessM (annexeval Config.isDirect) $ do
	-- Look at permissions bits rather than trying to write or
	-- using fileAccess because if run as root, any file can be
	-- modified despite permissions.
	s <- getFileStatus f
	let mode = fileMode s
	if (mode == mode `unionFileModes` ownerWriteMode)
		then assertFailure $ "able to modify annexed file's " ++ f ++ " content"
		else return ()

checkwritable :: FilePath -> Assertion
checkwritable f = do
	r <- tryIO $ writeFile f $ content f
	case r of
		Left _ -> assertFailure $ "unable to modify " ++ f
		Right _ -> return ()

checkdangling :: FilePath -> Assertion
checkdangling f = ifM (annexeval Config.crippledFileSystem)
	( return () -- probably no real symlinks to test
	, do
		r <- tryIO $ readFile f
		case r of
			Left _ -> return () -- expected; dangling link
			Right _ -> assertFailure $ f ++ " was not a dangling link as expected"
	)

checklocationlog :: FilePath -> Bool -> Assertion
checklocationlog f expected = do
	thisuuid <- annexeval Annex.UUID.getUUID
	r <- annexeval $ Backend.lookupFile f
	case r of
		Just (k, _) -> do
			uuids <- annexeval $ Remote.keyLocations k
			assertEqual ("bad content in location log for " ++ f ++ " key " ++ (Types.Key.key2file k) ++ " uuid " ++ show thisuuid)
				expected (thisuuid `elem` uuids)
		_ -> assertFailure $ f ++ " failed to look up key"

checkbackend :: FilePath -> Types.Backend -> Assertion
checkbackend file expected = do
	r <- annexeval $ Backend.lookupFile file
	let b = snd $ fromJust r
	assertEqual ("backend for " ++ file) expected b

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
annexed_notpresent = runchecks
	[checklink, checkdangling, notinlocationlog]

annexed_present :: FilePath -> Assertion
annexed_present = runchecks
	[checklink, checkcontent, checkunwritable, inlocationlog]

unannexed :: FilePath -> Assertion
unannexed = runchecks [checkregularfile, checkcontent, checkwritable]

withTestEnv :: Bool -> (IO TestEnv -> TestTree) -> TestTree
withTestEnv forcedirect = withResource prepare release
  where
	prepare = do
		env <- prepareTestEnv forcedirect
		case tryIngredients [consoleTestReporter] mempty (initTests env) of
			Nothing -> error "No tests found!?"
			Just act -> unlessM act $
				error "init tests failed! cannot continue"
		return env
	release = releaseTestEnv

releaseTestEnv :: TestEnv -> IO ()
releaseTestEnv _env = do
	cleanup tmpdir

prepareTestEnv :: Bool -> IO TestEnv
prepareTestEnv forcedirect = do
	whenM (doesDirectoryExist tmpdir) $
		error $ "The temporary directory " ++ tmpdir ++ " already exists; cannot run test suite."

	cwd <- getCurrentDirectory
	p <- Utility.Env.getEnvDefault "PATH" ""

	let env =
		-- Ensure that the just-built git annex is used.
		[ ("PATH", cwd ++ [searchPathSeparator] ++ p)
		, ("TOPDIR", cwd)
		-- Avoid git complaining if it cannot determine the user's
		-- email address, or exploding if it doesn't know the user's
		-- name.
		, ("GIT_AUTHOR_EMAIL", "test@example.com")
		, ("GIT_AUTHOR_NAME", "git-annex test")
		, ("GIT_COMMITTER_EMAIL", "test@example.com")
		, ("GIT_COMMITTER_NAME", "git-annex test")
		-- force gpg into batch mode for the tests
		, ("GPG_BATCH", "1")
		, ("FORCEDIRECT", if forcedirect then "1" else "")
		]

	return $ M.fromList env

changeToTmpDir :: TestEnv -> FilePath -> IO ()
changeToTmpDir env t = do
	let topdir = fromMaybe "" $ M.lookup "TOPDIR" env
	setCurrentDirectory $ topdir ++ "/" ++ t

tmpdir :: String
tmpdir = ".t"

mainrepodir :: FilePath
mainrepodir = tmpdir </> "repo"

tmprepodir :: IO FilePath
tmprepodir = go (0 :: Int)
  where
	go n = do
		let d = tmpdir </> "tmprepo" ++ show n
		ifM (doesDirectoryExist d)
			( go $ n + 1
			, return d
			)

annexedfile :: String
annexedfile = "foo"

wormannexedfile :: String
wormannexedfile = "apple"

sha1annexedfile :: String
sha1annexedfile = "sha1foo"

sha1annexedfiledup :: String
sha1annexedfiledup = "sha1foodup"

ingitfile :: String
ingitfile = "bar"

content :: FilePath -> String		
content f
	| f == annexedfile = "annexed file content"
	| f == ingitfile = "normal file content"
	| f == sha1annexedfile ="sha1 annexed file content"
	| f == sha1annexedfiledup = content sha1annexedfile
	| f == wormannexedfile = "worm annexed file content"
	| otherwise = "unknown file " ++ f

changecontent :: FilePath -> IO ()
changecontent f = writeFile f $ changedcontent f

changedcontent :: FilePath -> String
changedcontent f = (content f) ++ " (modified)"

backendSHA1 :: Types.Backend
backendSHA1 = backend_ "SHA1"

backendSHA256 :: Types.Backend
backendSHA256 = backend_ "SHA256"

backendWORM :: Types.Backend
backendWORM = backend_ "WORM"

backend_ :: String -> Types.Backend
backend_ name = Backend.lookupBackendName name
