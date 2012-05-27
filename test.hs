{- git-annex test suite
 -
 - Copyright 2010,2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.HUnit
import Test.HUnit.Tools
import Test.QuickCheck

import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.Files
import System.Posix.Env
import Control.Exception.Extensible
import qualified Data.Map as M
import System.IO.HVFS (SystemFS(..))
import Text.JSON

import Common

import qualified Utility.SafeCommand
import qualified Annex
import qualified Annex.UUID
import qualified Backend
import qualified Git.CurrentRepo
import qualified Git.Filename
import qualified Locations
import qualified Types.Backend
import qualified Types
import qualified GitAnnex
import qualified Logs.UUIDBased
import qualified Logs.Trust
import qualified Logs.Remote
import qualified Logs.Unused
import qualified Remote
import qualified Types.Key
import qualified Types.Messages
import qualified Config
import qualified Crypto
import qualified Utility.Path
import qualified Utility.FileMode
import qualified Utility.Gpg
import qualified Build.SysConfig
import qualified Utility.Format

-- for quickcheck
instance Arbitrary Types.Key.Key where
	arbitrary = do
		n <- arbitrary
		b <- elements ['A'..'Z']
		return Types.Key.Key {
			Types.Key.keyName = n,
			Types.Key.keyBackendName = [b],
			Types.Key.keySize = Nothing,
			Types.Key.keyMtime = Nothing
		}

main :: IO ()
main = do
	prepare
	r <- runVerboseTests $ TestList [quickcheck, blackbox]
	cleanup tmpdir
	propigate r

propigate :: (Counts, Int) -> IO ()
propigate (Counts { errors = e , failures = f }, _)
	| e+f > 0 = error "failed"
	| otherwise = return ()

quickcheck :: Test
quickcheck = TestLabel "quickcheck" $ TestList
	[ qctest "prop_idempotent_deencode_git" Git.Filename.prop_idempotent_deencode
	, qctest "prop_idempotent_deencode" Utility.Format.prop_idempotent_deencode
	, qctest "prop_idempotent_fileKey" Locations.prop_idempotent_fileKey
	, qctest "prop_idempotent_key_read_show" Types.Key.prop_idempotent_key_read_show
	, qctest "prop_idempotent_shellEscape" Utility.SafeCommand.prop_idempotent_shellEscape
	, qctest "prop_idempotent_shellEscape_multiword" Utility.SafeCommand.prop_idempotent_shellEscape_multiword
	, qctest "prop_idempotent_configEscape" Logs.Remote.prop_idempotent_configEscape
	, qctest "prop_parentDir_basics" Utility.Path.prop_parentDir_basics

	, qctest "prop_relPathDirToFile_basics" Utility.Path.prop_relPathDirToFile_basics
	, qctest "prop_relPathDirToFile_regressionTest" Utility.Path.prop_relPathDirToFile_regressionTest
	, qctest "prop_cost_sane" Config.prop_cost_sane
	, qctest "prop_hmacWithCipher_sane" Crypto.prop_hmacWithCipher_sane
	, qctest "prop_TimeStamp_sane" Logs.UUIDBased.prop_TimeStamp_sane
	, qctest "prop_addLog_sane" Logs.UUIDBased.prop_addLog_sane
	]

blackbox :: Test
blackbox = TestLabel "blackbox" $ TestList
	-- test order matters, later tests may rely on state from earlier
	[ test_init
	, test_add
	, test_reinject
	, test_unannex
	, test_drop
	, test_get
	, test_move
	, test_copy
	, test_lock
	, test_edit
	, test_fix
	, test_trust
	, test_fsck
	, test_migrate
	, test_unused
	, test_describe
	, test_find
	, test_merge
	, test_status
	, test_version
	, test_sync
	, test_map
	, test_uninit
	, test_upgrade
	, test_whereis
	, test_hook_remote
	, test_directory_remote
	, test_rsync_remote
	, test_bup_remote
	, test_crypto
	]

test_init :: Test
test_init = "git-annex init" ~: TestCase $ innewrepo $ do
	git_annex "init" [reponame] @? "init failed"
	where
		reponame = "test repo"

test_add :: Test
test_add = "git-annex add" ~: TestList [basic, sha1dup, subdirs]
	where
		-- this test case runs in the main repo, to set up a basic
		-- annexed file that later tests will use
		basic = TestCase $ inmainrepo $ do
			writeFile annexedfile $ content annexedfile
			git_annex "add" [annexedfile] @? "add failed"
			annexed_present annexedfile
			writeFile sha1annexedfile $ content sha1annexedfile
			git_annex "add" [sha1annexedfile, "--backend=SHA1"] @? "add with SHA1 failed"
			annexed_present sha1annexedfile
			checkbackend sha1annexedfile backendSHA1
			writeFile wormannexedfile $ content wormannexedfile
			git_annex "add" [wormannexedfile, "--backend=WORM"] @? "add with WORM failed"
			annexed_present wormannexedfile
			checkbackend wormannexedfile backendWORM
			boolSystem "git" [Params "rm --force -q", File wormannexedfile] @? "git rm failed"
			writeFile ingitfile $ content ingitfile
			boolSystem "git" [Param "add", File ingitfile] @? "git add failed"
			boolSystem "git" [Params "commit -q -a -m commit"] @? "git commit failed"
			git_annex "add" [ingitfile] @? "add ingitfile should be no-op"
			unannexed ingitfile
		sha1dup = TestCase $ intmpclonerepo $ do
			writeFile sha1annexedfiledup $ content sha1annexedfiledup
			git_annex "add" [sha1annexedfiledup, "--backend=SHA1"] @? "add of second file with same SHA1 failed"
			annexed_present sha1annexedfiledup
			annexed_present sha1annexedfile
		subdirs = TestCase $ intmpclonerepo $ do
			createDirectory "dir"
			writeFile "dir/foo" $ content annexedfile
			git_annex "add" ["dir"] @? "add of subdir failed"
			createDirectory "dir2"
			writeFile "dir2/foo" $ content annexedfile
			changeWorkingDirectory "dir"
			git_annex "add" ["../dir2"] @? "add of ../subdir failed"

test_reinject :: Test
test_reinject = "git-annex reinject/fromkey" ~: TestCase $ intmpclonerepo $ do
	git_annex "drop" ["--force", sha1annexedfile] @? "drop failed"
	writeFile tmp $ content sha1annexedfile
	r <- annexeval $ Types.Backend.getKey backendSHA1 tmp
	let key = show $ fromJust r
	git_annex "reinject" [tmp, sha1annexedfile] @? "reinject failed"
	git_annex "fromkey" [key, sha1annexedfiledup] @? "fromkey failed"
	annexed_present sha1annexedfiledup
	where
		tmp = "tmpfile"

test_unannex :: Test
test_unannex = "git-annex unannex" ~: TestList [nocopy, withcopy]
	where
		nocopy = "no content" ~: intmpclonerepo $ do
			annexed_notpresent annexedfile
			git_annex "unannex" [annexedfile] @? "unannex failed with no copy"
			annexed_notpresent annexedfile
		withcopy = "with content" ~: intmpclonerepo $ do
			git_annex "get" [annexedfile] @? "get failed"
			annexed_present annexedfile
			git_annex "unannex" [annexedfile, sha1annexedfile] @? "unannex failed"
			unannexed annexedfile
			git_annex "unannex" [annexedfile] @? "unannex failed on non-annexed file"
			unannexed annexedfile
			git_annex "unannex" [ingitfile] @? "unannex ingitfile should be no-op"
			unannexed ingitfile

test_drop :: Test
test_drop = "git-annex drop" ~: TestList [noremote, withremote, untrustedremote]
	where
		noremote = "no remotes" ~: TestCase $ intmpclonerepo $ do
			git_annex "get" [annexedfile] @? "get failed"
			boolSystem "git" [Params "remote rm origin"]
				@? "git remote rm origin failed"
			not <$> git_annex "drop" [annexedfile] @? "drop wrongly succeeded with no known copy of file"
			annexed_present annexedfile
			git_annex "drop" ["--force", annexedfile] @? "drop --force failed"
			annexed_notpresent annexedfile
			git_annex "drop" [annexedfile] @? "drop of dropped file failed"
			git_annex "drop" [ingitfile] @? "drop ingitfile should be no-op"
			unannexed ingitfile
		withremote = "with remote" ~: TestCase $ intmpclonerepo $ do
			git_annex "get" [annexedfile] @? "get failed"
			annexed_present annexedfile
			git_annex "drop" [annexedfile] @? "drop failed though origin has copy"
			annexed_notpresent annexedfile
			inmainrepo $ annexed_present annexedfile
		untrustedremote = "untrusted remote" ~: TestCase $ intmpclonerepo $ do
			git_annex "untrust" ["origin"] @? "untrust of origin failed"
			git_annex "get" [annexedfile] @? "get failed"
			annexed_present annexedfile
			not <$> git_annex "drop" [annexedfile] @? "drop wrongly suceeded with only an untrusted copy of the file"
			annexed_present annexedfile
			inmainrepo $ annexed_present annexedfile

test_get :: Test
test_get = "git-annex get" ~: TestCase $ intmpclonerepo $ do
	inmainrepo $ annexed_present annexedfile
	annexed_notpresent annexedfile
	git_annex "get" [annexedfile] @? "get of file failed"
	inmainrepo $ annexed_present annexedfile
	annexed_present annexedfile
	git_annex "get" [annexedfile] @? "get of file already here failed"
	inmainrepo $ annexed_present annexedfile
	annexed_present annexedfile
	inmainrepo $ unannexed ingitfile
	unannexed ingitfile
	git_annex "get" [ingitfile] @? "get ingitfile should be no-op"
	inmainrepo $ unannexed ingitfile
	unannexed ingitfile

test_move :: Test
test_move = "git-annex move" ~: TestCase $ intmpclonerepo $ do
	annexed_notpresent annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "move" ["--from", "origin", annexedfile] @? "move --from of file failed"
	annexed_present annexedfile
	inmainrepo $ annexed_notpresent annexedfile
	git_annex "move" ["--from", "origin", annexedfile] @? "move --from of file already here failed"
	annexed_present annexedfile
	inmainrepo $ annexed_notpresent annexedfile
	git_annex "move" ["--to", "origin", annexedfile] @? "move --to of file failed"
	inmainrepo $ annexed_present annexedfile
	annexed_notpresent annexedfile
	git_annex "move" ["--to", "origin", annexedfile] @? "move --to of file already there failed"
	inmainrepo $ annexed_present annexedfile
	annexed_notpresent annexedfile
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile
	git_annex "move" ["--to", "origin", ingitfile] @? "move of ingitfile should be no-op"
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile
	git_annex "move" ["--from", "origin", ingitfile] @? "move of ingitfile should be no-op"
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile

test_copy :: Test
test_copy = "git-annex copy" ~: TestCase $ intmpclonerepo $ do
	annexed_notpresent annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "copy" ["--from", "origin", annexedfile] @? "copy --from of file failed"
	annexed_present annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "copy" ["--from", "origin", annexedfile] @? "copy --from of file already here failed"
	annexed_present annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "copy" ["--to", "origin", annexedfile] @? "copy --to of file already there failed"
	annexed_present annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "move" ["--to", "origin", annexedfile] @? "move --to of file already there failed"
	annexed_notpresent annexedfile
	inmainrepo $ annexed_present annexedfile
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile
	git_annex "copy" ["--to", "origin", ingitfile] @? "copy of ingitfile should be no-op"
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile
	git_annex "copy" ["--from", "origin", ingitfile] @? "copy of ingitfile should be no-op"
	checkregularfile ingitfile
	checkcontent ingitfile

test_lock :: Test
test_lock = "git-annex unlock/lock" ~: intmpclonerepo $ do
	-- regression test: unlock of not present file should skip it
	annexed_notpresent annexedfile
	not <$> git_annex "unlock" [annexedfile] @? "unlock failed to fail with not present file"
	annexed_notpresent annexedfile

	git_annex "get" [annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex "unlock" [annexedfile] @? "unlock failed"		
	unannexed annexedfile
	-- write different content, to verify that lock
	-- throws it away
	changecontent annexedfile
	writeFile annexedfile $ content annexedfile ++ "foo"
	git_annex "lock" [annexedfile] @? "lock failed"
	annexed_present annexedfile
	git_annex "unlock" [annexedfile] @? "unlock failed"		
	unannexed annexedfile
	changecontent annexedfile
	git_annex "add" [annexedfile] @? "add of modified file failed"
	runchecks [checklink, checkunwritable] annexedfile
	c <- readFile annexedfile
	assertEqual "content of modified file" c (changedcontent annexedfile)
	r' <- git_annex "drop" [annexedfile]
	not r' @? "drop wrongly succeeded with no known copy of modified file"

test_edit :: Test
test_edit = "git-annex edit/commit" ~: TestList [t False, t True]
	where t precommit = TestCase $ intmpclonerepo $ do
		git_annex "get" [annexedfile] @? "get of file failed"
		annexed_present annexedfile
		git_annex "edit" [annexedfile] @? "edit failed"
		unannexed annexedfile
		changecontent annexedfile
		if precommit
			then do
				-- pre-commit depends on the file being
				-- staged, normally git commit does this
				boolSystem "git" [Param "add", File annexedfile]
					@? "git add of edited file failed"
				git_annex "pre-commit" []
					@? "pre-commit failed"
			else do
				boolSystem "git" [Params "commit -q -a -m contentchanged"]
					@? "git commit of edited file failed"
		runchecks [checklink, checkunwritable] annexedfile
		c <- readFile annexedfile
		assertEqual "content of modified file" c (changedcontent annexedfile)
		not <$> git_annex "drop" [annexedfile] @? "drop wrongly succeeded with no known copy of modified file"

test_fix :: Test
test_fix = "git-annex fix" ~: intmpclonerepo $ do
	annexed_notpresent annexedfile
	git_annex "fix" [annexedfile] @? "fix of not present failed"
	annexed_notpresent annexedfile
	git_annex "get" [annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex "fix" [annexedfile] @? "fix of present file failed"
	annexed_present annexedfile
	createDirectory subdir
	boolSystem "git" [Param "mv", File annexedfile, File subdir]
		@? "git mv failed"
	git_annex "fix" [newfile] @? "fix of moved file failed"
	runchecks [checklink, checkunwritable] newfile
	c <- readFile newfile
	assertEqual "content of moved file" c (content annexedfile)
	where
		subdir = "s"
		newfile = subdir ++ "/" ++ annexedfile

test_trust :: Test
test_trust = "git-annex trust/untrust/semitrust/dead" ~: intmpclonerepo $ do
	git_annex "trust" [repo] @? "trust failed"
	trustcheck Logs.Trust.Trusted "trusted 1"
	git_annex "trust" [repo] @? "trust of trusted failed"
	trustcheck Logs.Trust.Trusted "trusted 2"
	git_annex "untrust" [repo] @? "untrust failed"
	trustcheck Logs.Trust.UnTrusted "untrusted 1"
	git_annex "untrust" [repo] @? "untrust of untrusted failed"
	trustcheck Logs.Trust.UnTrusted "untrusted 2"
	git_annex "dead" [repo] @? "dead failed"
	trustcheck Logs.Trust.DeadTrusted "deadtrusted 1"
	git_annex "dead" [repo] @? "dead of dead failed"
	trustcheck Logs.Trust.DeadTrusted "deadtrusted 2"
	git_annex "semitrust" [repo] @? "semitrust failed"
	trustcheck Logs.Trust.SemiTrusted "semitrusted 1"
	git_annex "semitrust" [repo] @? "semitrust of semitrusted failed"
	trustcheck Logs.Trust.SemiTrusted "semitrusted 2"
	where
		repo = "origin"
		trustcheck expected msg = do
			present <- annexeval $ do
				l <- Logs.Trust.trustGet expected
				u <- Remote.nameToUUID repo
				return $ u `elem` l
			assertBool msg present

test_fsck :: Test
test_fsck = "git-annex fsck" ~: TestList [basicfsck, barefsck, withlocaluntrusted, withremoteuntrusted]
	where
		basicfsck = TestCase $ intmpclonerepo $ do
			git_annex "fsck" [] @? "fsck failed"
			boolSystem "git" [Params "config annex.numcopies 2"] @? "git config failed"
			fsck_should_fail "numcopies unsatisfied"
			boolSystem "git" [Params "config annex.numcopies 1"] @? "git config failed"
			corrupt annexedfile
			corrupt sha1annexedfile
		barefsck = TestCase $ intmpbareclonerepo $ do
			git_annex "fsck" [] @? "fsck failed"
		withlocaluntrusted = TestCase $ intmpclonerepo $ do
			git_annex "get" [annexedfile] @? "get failed"
			git_annex "untrust" ["origin"] @? "untrust of origin repo failed"
			git_annex "untrust" ["."] @? "untrust of current repo failed"
			fsck_should_fail "content only available in untrusted (current) repository"
			git_annex "trust" ["."] @? "trust of current repo failed"
			git_annex "fsck" [annexedfile] @? "fsck failed on file present in trusted repo"
		withremoteuntrusted = TestCase $ intmpclonerepo $ do
			boolSystem "git" [Params "config annex.numcopies 2"] @? "git config failed"
			git_annex "get" [annexedfile] @? "get failed"
			git_annex "get" [sha1annexedfile] @? "get failed"
			git_annex "fsck" [] @? "fsck failed with numcopies=2 and 2 copies"
			git_annex "untrust" ["origin"] @? "untrust of origin failed"
			fsck_should_fail "content not replicated to enough non-untrusted repositories"

		corrupt f = do
			git_annex "get" [f] @? "get of file failed"
			Utility.FileMode.allowWrite f
			writeFile f (changedcontent f)
			not <$> git_annex "fsck" [] @? "fsck failed to fail with corrupted file content"
			git_annex "fsck" [] @? "fsck unexpectedly failed again; previous one did not fix problem with " ++ f
		fsck_should_fail m = do
			not <$> git_annex "fsck" [] @? "fsck failed to fail with " ++ m

test_migrate :: Test
test_migrate = "git-annex migrate" ~: TestList [t False, t True]
	where t usegitattributes = TestCase $ intmpclonerepo $ do
		annexed_notpresent annexedfile
		annexed_notpresent sha1annexedfile
		git_annex "migrate" [annexedfile] @? "migrate of not present failed"
		git_annex "migrate" [sha1annexedfile] @? "migrate of not present failed"
		git_annex "get" [annexedfile] @? "get of file failed"
		git_annex "get" [sha1annexedfile] @? "get of file failed"
		annexed_present annexedfile
		annexed_present sha1annexedfile
		if usegitattributes
			then do
				writeFile ".gitattributes" $ "* annex.backend=SHA1"
				git_annex "migrate" [sha1annexedfile]
					@? "migrate sha1annexedfile failed"
				git_annex "migrate" [annexedfile]
					@? "migrate annexedfile failed"
			else do
				git_annex "migrate" [sha1annexedfile, "--backend", "SHA1"]
					@? "migrate sha1annexedfile failed"
				git_annex "migrate" [annexedfile, "--backend", "SHA1"]
					@? "migrate annexedfile failed"
		annexed_present annexedfile
		annexed_present sha1annexedfile
		checkbackend annexedfile backendSHA1
		checkbackend sha1annexedfile backendSHA1

		-- check that reversing a migration works
		writeFile ".gitattributes" $ "* annex.backend=SHA256"
		git_annex "migrate" [sha1annexedfile]
			@? "migrate sha1annexedfile failed"
		git_annex "migrate" [annexedfile]
			@? "migrate annexedfile failed"
		annexed_present annexedfile
		annexed_present sha1annexedfile
		checkbackend annexedfile backendSHA256
		checkbackend sha1annexedfile backendSHA256

test_unused :: Test
test_unused = "git-annex unused/dropunused" ~: intmpclonerepo $ do
	-- keys have to be looked up before files are removed
	annexedfilekey <- annexeval $ findkey annexedfile
	sha1annexedfilekey <- annexeval $ findkey sha1annexedfile
	git_annex "get" [annexedfile] @? "get of file failed"
	git_annex "get" [sha1annexedfile] @? "get of file failed"
	checkunused []
	boolSystem "git" [Params "rm -q", File annexedfile] @? "git rm failed"
	checkunused []
	boolSystem "git" [Params "commit -q -m foo"] @? "git commit failed"
	checkunused []
	-- unused checks origin/master; once it's gone it is really unused
	boolSystem "git" [Params "remote rm origin"] @? "git remote rm origin failed"
	checkunused [annexedfilekey]
	boolSystem "git" [Params "rm -q", File sha1annexedfile] @? "git rm failed"
	boolSystem "git" [Params "commit -q -m foo"] @? "git commit failed"
	checkunused [annexedfilekey, sha1annexedfilekey]

	-- good opportunity to test dropkey also
	git_annex "dropkey" ["--force", show annexedfilekey]
		@? "dropkey failed"
	checkunused [sha1annexedfilekey]

	git_annex "dropunused" ["1", "2"] @? "dropunused failed"
	checkunused []
	git_annex "dropunused" ["10", "501"] @? "dropunused failed on bogus numbers"

	where
		checkunused expectedkeys = do
			git_annex "unused" [] @? "unused failed"
			unusedmap <- annexeval $ Logs.Unused.readUnusedLog ""
			let unusedkeys = M.elems unusedmap
			assertEqual "unused keys differ"
				(sort expectedkeys) (sort unusedkeys)
		findkey f = do
			r <- Backend.lookupFile f
			return $ fst $ fromJust r

test_describe :: Test
test_describe = "git-annex describe" ~: intmpclonerepo $ do
	git_annex "describe" [".", "this repo"] @? "describe 1 failed"
	git_annex "describe" ["origin", "origin repo"] @? "describe 2 failed"

test_find :: Test
test_find = "git-annex find" ~: intmpclonerepo $ do
	annexed_notpresent annexedfile
	git_annex_expectoutput "find" [] []
	git_annex "get" [annexedfile] @? "get failed"
	annexed_present annexedfile
	annexed_notpresent sha1annexedfile
	git_annex_expectoutput "find" [] [annexedfile]
	git_annex_expectoutput "find" ["--exclude", annexedfile, "--and", "--exclude", sha1annexedfile] []
	git_annex_expectoutput "find" ["--include", annexedfile] [annexedfile]
	git_annex_expectoutput "find" ["--not", "--in", "origin"] []
	git_annex_expectoutput "find" ["--copies", "1", "--and", "--not", "--copies", "2"] [sha1annexedfile]
	git_annex_expectoutput "find" ["--inbackend", "SHA1"] [sha1annexedfile]
	git_annex_expectoutput "find" ["--inbackend", "WORM"] []

test_merge :: Test
test_merge = "git-annex merge" ~: intmpclonerepo $ do
	git_annex "merge" [] @? "merge failed"

test_status :: Test
test_status = "git-annex status" ~: intmpclonerepo $ do
	json <- git_annex_output "status" ["--json"]
	case Text.JSON.decodeStrict json :: Text.JSON.Result (JSObject JSValue) of
		Ok _ -> return ()
		Error e -> assertFailure e

test_version :: Test
test_version = "git-annex version" ~: intmpclonerepo $ do
	git_annex "version" [] @? "version failed"

test_sync :: Test
test_sync = "git-annex sync" ~: intmpclonerepo $ do
	git_annex "sync" [] @? "sync failed"

test_map :: Test
test_map = "git-annex map" ~: intmpclonerepo $ do
	-- set descriptions, that will be looked for in the map
	git_annex "describe" [".", "this repo"] @? "describe 1 failed"
	git_annex "describe" ["origin", "origin repo"] @? "describe 2 failed"
	-- --fast avoids it running graphviz, not a build dependency
	git_annex "map" ["--fast"] @? "map failed"
	doesFileExist "map.dot" @? "map.dot not generated"
	c <- readFile "map.dot"
	("this repo" `isInfixOf` c && "origin repo" `isInfixOf` c) @? ("map.dot bad content: " ++ c)

test_uninit :: Test
test_uninit = "git-annex uninit" ~: intmpclonerepo $ do
	git_annex "get" [] @? "get failed"
	annexed_present annexedfile
	boolSystem "git" [Params "checkout git-annex"] @? "git checkout git-annex"
	not <$> git_annex "uninit" [] @? "uninit failed to fail when git-annex branch was checked out"
	boolSystem "git" [Params "checkout master"] @? "git checkout master"
	_ <- git_annex "uninit" [] -- exit status not checked; does abnormal exit
	checkregularfile annexedfile
	doesDirectoryExist ".git" @? ".git vanished in uninit"
	not <$> doesDirectoryExist ".git/annex" @? ".git/annex still present after uninit"

test_upgrade :: Test
test_upgrade = "git-annex upgrade" ~: intmpclonerepo $ do
	git_annex "upgrade" [] @? "upgrade from same version failed"

test_whereis :: Test
test_whereis = "git-annex whereis" ~: intmpclonerepo $ do
	annexed_notpresent annexedfile
	git_annex "whereis" [annexedfile] @? "whereis on non-present file failed"
	git_annex "untrust" ["origin"] @? "untrust failed"
	not <$> git_annex "whereis" [annexedfile] @? "whereis on non-present file only present in untrusted repo failed to fail"
	git_annex "get" [annexedfile] @? "get failed"
	annexed_present annexedfile
	git_annex "whereis" [annexedfile] @? "whereis on present file failed"

test_hook_remote :: Test
test_hook_remote = "git-annex hook remote" ~: intmpclonerepo $ do
	git_annex "initremote" (words "foo type=hook encryption=none hooktype=foo") @? "initremote failed"
	createDirectory dir
	git_config "annex.foo-store-hook" $
		"cp $ANNEX_FILE " ++ loc
	git_config "annex.foo-retrieve-hook" $
		"cp " ++ loc ++ " $ANNEX_FILE"
	git_config "annex.foo-remove-hook" $
		"rm -f " ++ loc
	git_config "annex.foo-checkpresent-hook" $
		"if [ -e " ++ loc ++ " ]; then echo $ANNEX_KEY; fi"
	git_annex "get" [annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex "copy" [annexedfile, "--to", "foo"] @? "copy --to hook remote failed"
	annexed_present annexedfile
	git_annex "drop" [annexedfile, "--numcopies=2"] @? "drop failed"
	annexed_notpresent annexedfile
	git_annex "move" [annexedfile, "--from", "foo"] @? "move --from hook remote failed"
	annexed_present annexedfile
	not <$> git_annex "drop" [annexedfile, "--numcopies=2"] @? "drop failed to fail"
	annexed_present annexedfile
	where
		dir = "dir"
		loc = dir ++ "/$ANNEX_KEY"
		git_config k v = boolSystem "git" [Param "config", Param k, Param v]
			@? "git config failed"

test_directory_remote :: Test
test_directory_remote = "git-annex directory remote" ~: intmpclonerepo $ do
	createDirectory "dir"
	git_annex "initremote" (words $ "foo type=directory encryption=none directory=dir") @? "initremote failed"
	git_annex "get" [annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex "copy" [annexedfile, "--to", "foo"] @? "copy --to directory remote failed"
	annexed_present annexedfile
	git_annex "drop" [annexedfile, "--numcopies=2"] @? "drop failed"
	annexed_notpresent annexedfile
	git_annex "move" [annexedfile, "--from", "foo"] @? "move --from directory remote failed"
	annexed_present annexedfile
	not <$> git_annex "drop" [annexedfile, "--numcopies=2"] @? "drop failed to fail"
	annexed_present annexedfile

test_rsync_remote :: Test
test_rsync_remote = "git-annex rsync remote" ~: intmpclonerepo $ do
	createDirectory "dir"
	git_annex "initremote" (words $ "foo type=rsync encryption=none rsyncurl=dir") @? "initremote failed"
	git_annex "get" [annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex "copy" [annexedfile, "--to", "foo"] @? "copy --to rsync remote failed"
	annexed_present annexedfile
	git_annex "drop" [annexedfile, "--numcopies=2"] @? "drop failed"
	annexed_notpresent annexedfile
	git_annex "move" [annexedfile, "--from", "foo"] @? "move --from rsync remote failed"
	annexed_present annexedfile
	not <$> git_annex "drop" [annexedfile, "--numcopies=2"] @? "drop failed to fail"
	annexed_present annexedfile

test_bup_remote :: Test
test_bup_remote = "git-annex bup remote" ~: intmpclonerepo $ when Build.SysConfig.bup $ do
	dir <- absPath "dir" -- bup special remote needs an absolute path
	createDirectory dir
	git_annex "initremote" (words $ "foo type=bup encryption=none buprepo="++dir) @? "initremote failed"
	git_annex "get" [annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex "copy" [annexedfile, "--to", "foo"] @? "copy --to bup remote failed"
	annexed_present annexedfile
	git_annex "drop" [annexedfile, "--numcopies=2"] @? "drop failed"
	annexed_notpresent annexedfile
	git_annex "copy" [annexedfile, "--from", "foo"] @? "copy --from bup remote failed"
	annexed_present annexedfile
	not <$> git_annex "move" [annexedfile, "--from", "foo"] @? "move --from bup remote failed to fail"
	annexed_present annexedfile

-- gpg is not a build dependency, so only test when it's available
test_crypto :: Test
test_crypto = "git-annex crypto" ~: intmpclonerepo $ when Build.SysConfig.gpg $ do
	-- force gpg into batch mode for the tests
	setEnv "GPG_BATCH" "1" True
	Utility.Gpg.testTestHarness @? "test harness self-test failed"
	Utility.Gpg.testHarness $ do
		createDirectory "dir"
		let initremote = git_annex "initremote"
			[ "foo"
			, "type=directory"
			, "encryption=" ++ Utility.Gpg.testKeyId
			, "directory=dir"
			]
		initremote @? "initremote failed"
		initremote @? "initremote failed when run twice in a row"
		git_annex "get" [annexedfile] @? "get of file failed"
		annexed_present annexedfile
		git_annex "copy" [annexedfile, "--to", "foo"] @? "copy --to encrypted remote failed"
		annexed_present annexedfile
		git_annex "drop" [annexedfile, "--numcopies=2"] @? "drop failed"
		annexed_notpresent annexedfile
		git_annex "move" [annexedfile, "--from", "foo"] @? "move --from encrypted remote failed"
		annexed_present annexedfile
		not <$> git_annex "drop" [annexedfile, "--numcopies=2"] @? "drop failed to fail"
		annexed_present annexedfile	

-- This is equivilant to running git-annex, but it's all run in-process
-- so test coverage collection works.
git_annex :: String -> [String] -> IO Bool
git_annex command params = do
	-- catch all errors, including normally fatal errors
	r <- try (run)::IO (Either SomeException ())
	case r of
		Right _ -> return True
		Left _ -> return False
	where
		run = GitAnnex.run (command:"-q":params)

{- Runs git-annex and returns its output. -}
git_annex_output :: String -> [String] -> IO String
git_annex_output command params = do
	(frompipe, topipe) <- createPipe
	pid <- forkProcess $ do
		_ <- dupTo topipe stdOutput
		closeFd frompipe
		_ <- git_annex command params
		exitSuccess
	-- XXX since the above is a separate process, code coverage stats are
	-- not gathered for things run in it.
	closeFd topipe
	fromh <- fdToHandle frompipe
	got <- hGetContentsStrict fromh
	hClose fromh
	_ <- getProcessStatus True False pid
	-- XXX hack Run same command again, to get code coverage.
	_ <- git_annex command params
	return got

git_annex_expectoutput :: String -> [String] -> [String] -> IO ()
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
		a

innewrepo :: Assertion -> Assertion
innewrepo a = withgitrepo $ \r -> indir r a

inmainrepo :: Assertion -> Assertion
inmainrepo a = indir mainrepodir a

intmpclonerepo :: Assertion -> Assertion
intmpclonerepo a = withtmpclonerepo False $ \r -> indir r a

intmpbareclonerepo :: Assertion -> Assertion
intmpbareclonerepo a = withtmpclonerepo True $ \r -> indir r a

withtmpclonerepo :: Bool -> (FilePath -> Assertion) -> Assertion
withtmpclonerepo bare = bracket (clonerepo mainrepodir tmprepodir bare) cleanup

withgitrepo :: (FilePath -> Assertion) -> Assertion
withgitrepo = bracket (setuprepo mainrepodir) return

indir :: FilePath -> Assertion -> Assertion
indir dir a = do
	cwd <- getCurrentDirectory
	-- Assertion failures throw non-IO errors; catch
	-- any type of error and change back to cwd before
	-- rethrowing.
	r <- bracket_ (changeToTmpDir dir) (changeWorkingDirectory cwd)
		(try (a)::IO (Either SomeException ()))
	case r of
		Right () -> return ()
		Left e -> throw e

setuprepo :: FilePath -> IO FilePath
setuprepo dir = do
	cleanup dir
	ensuretmpdir
	boolSystem "git" [Params "init -q", File dir] @? "git init failed"
	indir dir $ do
		boolSystem "git" [Params "config user.name", Param "Test User"] @? "git config failed"
		boolSystem "git" [Params "config user.email test@example.com"] @? "git config failed"
	return dir

-- clones are always done as local clones; we cannot test ssh clones
clonerepo :: FilePath -> FilePath -> Bool -> IO FilePath
clonerepo old new bare = do
	cleanup new
	ensuretmpdir
	let b = if bare then " --bare" else ""
	boolSystem "git" [Params ("clone -q" ++ b), File old, File new] @? "git clone failed"
	indir new $ git_annex "init" ["-q", new] @? "git annex init failed"
	return new
	
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
		removeDirectoryRecursive dir
	
checklink :: FilePath -> Assertion
checklink f = do
	s <- getSymbolicLinkStatus f
	isSymbolicLink s @? f ++ " is not a symlink"

checkregularfile :: FilePath -> Assertion
checkregularfile f = do
	s <- getSymbolicLinkStatus f
	isRegularFile s @? f ++ " is not a normal file"
	return ()

checkcontent :: FilePath -> Assertion
checkcontent f = do
	c <- readFile f
	assertEqual ("checkcontent " ++ f) c (content f)

checkunwritable :: FilePath -> Assertion
checkunwritable f = do
	-- Look at permissions bits rather than trying to write or using
	-- fileAccess because if run as root, any file can be modified
	-- despite permissions.
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
checkdangling f = do
	r <- tryIO $ readFile f
	case r of
		Left _ -> return () -- expected; dangling link
		Right _ -> assertFailure $ f ++ " was not a dangling link as expected"

checklocationlog :: FilePath -> Bool -> Assertion
checklocationlog f expected = do
	thisuuid <- annexeval Annex.UUID.getUUID
	r <- annexeval $ Backend.lookupFile f
	case r of
		Just (k, _) -> do
			uuids <- annexeval $ Remote.keyLocations k
			assertEqual ("bad content in location log for " ++ f ++ " key " ++ (show k) ++ " uuid " ++ show thisuuid)
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

prepare :: IO ()
prepare = do
	-- While PATH is mostly avoided, the commit hook does run it. Make
	-- sure that the just-built git annex is used.
	cwd <- getCurrentDirectory
	p <- getEnvDefault  "PATH" ""
	setEnv "PATH" (cwd ++ ":" ++ p) True
	setEnv "TOPDIR" cwd True
	-- Avoid git complaining if it cannot determine the user's email
	-- address.
	setEnv "EMAIL" "git-annex test <test@example.com>" True

changeToTmpDir :: FilePath -> IO ()
changeToTmpDir t = do
	-- Hack alert. Threading state to here was too much bother.
	topdir <- getEnvDefault "TOPDIR" ""
	changeWorkingDirectory $ topdir ++ "/" ++ t

tmpdir :: String
tmpdir = ".t"

mainrepodir :: String
mainrepodir = tmpdir ++ "/repo"

tmprepodir :: String
tmprepodir = tmpdir ++ "/tmprepo"

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
