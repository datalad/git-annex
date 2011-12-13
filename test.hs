{- git-annex test suite
 -
 - Copyright 2010,2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import Test.HUnit
import Test.HUnit.Tools
import Test.QuickCheck

import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.Files
import Control.Exception (bracket_, bracket, throw)
import System.IO.Error
import System.Posix.Env
import qualified Control.Exception.Extensible as E
import qualified Data.Map as M
import System.IO.HVFS (SystemFS(..))

import Common

import qualified Utility.SafeCommand
import qualified Annex
import qualified Annex.UUID
import qualified Backend
import qualified Git
import qualified Git.Config
import qualified Git.Construct
import qualified Git.Filename
import qualified Locations
import qualified Types.Backend
import qualified Types
import qualified GitAnnex
import qualified Logs.Location
import qualified Logs.UUIDBased
import qualified Logs.Trust
import qualified Logs.Remote
import qualified Remote
import qualified Command.DropUnused
import qualified Types.Key
import qualified Config
import qualified Crypto
import qualified Utility.Path
import qualified Utility.FileMode

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
	[ qctest "prop_idempotent_deencode" Git.Filename.prop_idempotent_deencode
	, qctest "prop_idempotent_fileKey" Locations.prop_idempotent_fileKey
	, qctest "prop_idempotent_key_read_show" Types.Key.prop_idempotent_key_read_show
	, qctest "prop_idempotent_shellEscape" Utility.SafeCommand.prop_idempotent_shellEscape
	, qctest "prop_idempotent_shellEscape_multiword" Utility.SafeCommand.prop_idempotent_shellEscape_multiword
	, qctest "prop_idempotent_configEscape" Logs.Remote.prop_idempotent_configEscape
	, qctest "prop_parentDir_basics" Utility.Path.prop_parentDir_basics

	, qctest "prop_relPathDirToFile_basics" Utility.Path.prop_relPathDirToFile_basics
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
	]

test_init :: Test
test_init = "git-annex init" ~: TestCase $ innewrepo $ do
	git_annex "init" ["-q", reponame] @? "init failed"
	where
		reponame = "test repo"

test_add :: Test
test_add = "git-annex add" ~: TestList [basic, sha1dup, subdirs]
	where
		-- this test case runs in the main repo, to set up a basic
		-- annexed file that later tests will use
		basic = TestCase $ inmainrepo $ do
			writeFile annexedfile $ content annexedfile
			git_annex "add" ["-q", annexedfile] @? "add failed"
			annexed_present annexedfile
			writeFile sha1annexedfile $ content sha1annexedfile
			git_annex "add" ["-q", sha1annexedfile, "--backend=SHA1"] @? "add with SHA1 failed"
			annexed_present sha1annexedfile
			writeFile ingitfile $ content ingitfile
			boolSystem "git" [Param "add", File ingitfile] @? "git add failed"
			boolSystem "git" [Params "commit -q -a -m commit"] @? "git commit failed"
			git_annex "add" ["-q", ingitfile] @? "add ingitfile should be no-op"
			unannexed ingitfile
		sha1dup = TestCase $ intmpclonerepo $ do
			writeFile sha1annexedfiledup $ content sha1annexedfiledup
			git_annex "add" ["-q", sha1annexedfiledup, "--backend=SHA1"] @? "add of second file with same SHA1 failed"
			annexed_present sha1annexedfiledup
			annexed_present sha1annexedfile
		subdirs = TestCase $ intmpclonerepo $ do
			createDirectory "dir"
			writeFile "dir/foo" $ content annexedfile
			git_annex "add" ["-q", "dir"] @? "add of subdir failed"
			createDirectory "dir2"
			writeFile "dir2/foo" $ content annexedfile
			changeWorkingDirectory "dir"
			git_annex "add" ["-q", "../dir2"] @? "add of ../subdir failed"

test_reinject :: Test
test_reinject = "git-annex reinject/fromkey" ~: TestCase $ intmpclonerepo $ do
	git_annex "drop" ["-q", "--force", sha1annexedfile] @? "drop failed"
	writeFile tmp $ content sha1annexedfile
	r <- annexeval $ Types.Backend.getKey backendSHA1 tmp
	let key = show $ fromJust r
	git_annex "reinject" ["-q", tmp, sha1annexedfile] @? "reinject failed"
	git_annex "fromkey" ["-q", key, sha1annexedfiledup] @? "fromkey failed"
	annexed_present sha1annexedfiledup
	where
		tmp = "tmpfile"

test_unannex :: Test
test_unannex = "git-annex unannex" ~: TestList [nocopy, withcopy]
	where
		nocopy = "no content" ~: intmpclonerepo $ do
			annexed_notpresent annexedfile
			git_annex "unannex" ["-q", annexedfile] @? "unannex failed with no copy"
			annexed_notpresent annexedfile
		withcopy = "with content" ~: intmpclonerepo $ do
			git_annex "get" ["-q", annexedfile] @? "get failed"
			annexed_present annexedfile
			git_annex "unannex" ["-q", annexedfile, sha1annexedfile] @? "unannex failed"
			unannexed annexedfile
			git_annex "unannex" ["-q", annexedfile] @? "unannex failed on non-annexed file"
			unannexed annexedfile
			git_annex "unannex" ["-q", ingitfile] @? "unannex ingitfile should be no-op"
			unannexed ingitfile

test_drop :: Test
test_drop = "git-annex drop" ~: TestList [noremote, withremote, untrustedremote]
	where
		noremote = "no remotes" ~: TestCase $ intmpclonerepo $ do
			git_annex "get" ["-q", annexedfile] @? "get failed"
			boolSystem "git" [Params "remote rm origin"]
				@? "git remote rm origin failed"
			r <- git_annex "drop" ["-q", annexedfile]
			not r @? "drop wrongly succeeded with no known copy of file"
			annexed_present annexedfile
			git_annex "drop" ["-q", "--force", annexedfile] @? "drop --force failed"
			annexed_notpresent annexedfile
			git_annex "drop" ["-q", annexedfile] @? "drop of dropped file failed"
			git_annex "drop" ["-q", ingitfile] @? "drop ingitfile should be no-op"
			unannexed ingitfile
		withremote = "with remote" ~: TestCase $ intmpclonerepo $ do
			git_annex "get" ["-q", annexedfile] @? "get failed"
			annexed_present annexedfile
			git_annex "drop" ["-q", annexedfile] @? "drop failed though origin has copy"
			annexed_notpresent annexedfile
			inmainrepo $ annexed_present annexedfile
		untrustedremote = "untrusted remote" ~: TestCase $ intmpclonerepo $ do
			git_annex "untrust" ["-q", "origin"] @? "untrust of origin failed"
			git_annex "get" ["-q", annexedfile] @? "get failed"
			annexed_present annexedfile
			r <- git_annex "drop" ["-q", annexedfile]
			not r @? "drop wrongly suceeded with only an untrusted copy of the file"
			annexed_present annexedfile
			inmainrepo $ annexed_present annexedfile

test_get :: Test
test_get = "git-annex get" ~: TestCase $ intmpclonerepo $ do
	inmainrepo $ annexed_present annexedfile
	annexed_notpresent annexedfile
	git_annex "get" ["-q", annexedfile] @? "get of file failed"
	inmainrepo $ annexed_present annexedfile
	annexed_present annexedfile
	git_annex "get" ["-q", annexedfile] @? "get of file already here failed"
	inmainrepo $ annexed_present annexedfile
	annexed_present annexedfile
	inmainrepo $ unannexed ingitfile
	unannexed ingitfile
	git_annex "get" ["-q", ingitfile] @? "get ingitfile should be no-op"
	inmainrepo $ unannexed ingitfile
	unannexed ingitfile

test_move :: Test
test_move = "git-annex move" ~: TestCase $ intmpclonerepo $ do
	annexed_notpresent annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "move" ["-q", "--from", "origin", annexedfile] @? "move --from of file failed"
	annexed_present annexedfile
	inmainrepo $ annexed_notpresent annexedfile
	git_annex "move" ["-q", "--from", "origin", annexedfile] @? "move --from of file already here failed"
	annexed_present annexedfile
	inmainrepo $ annexed_notpresent annexedfile
	git_annex "move" ["-q", "--to", "origin", annexedfile] @? "move --to of file failed"
	inmainrepo $ annexed_present annexedfile
	annexed_notpresent annexedfile
	git_annex "move" ["-q", "--to", "origin", annexedfile] @? "move --to of file already there failed"
	inmainrepo $ annexed_present annexedfile
	annexed_notpresent annexedfile
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile
	git_annex "move" ["-q", "--to", "origin", ingitfile] @? "move of ingitfile should be no-op"
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile
	git_annex "move" ["-q", "--from", "origin", ingitfile] @? "move of ingitfile should be no-op"
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile

test_copy :: Test
test_copy = "git-annex copy" ~: TestCase $ intmpclonerepo $ do
	annexed_notpresent annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "copy" ["-q", "--from", "origin", annexedfile] @? "copy --from of file failed"
	annexed_present annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "copy" ["-q", "--from", "origin", annexedfile] @? "copy --from of file already here failed"
	annexed_present annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "copy" ["-q", "--to", "origin", annexedfile] @? "copy --to of file already there failed"
	annexed_present annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "move" ["-q", "--to", "origin", annexedfile] @? "move --to of file already there failed"
	annexed_notpresent annexedfile
	inmainrepo $ annexed_present annexedfile
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile
	git_annex "copy" ["-q", "--to", "origin", ingitfile] @? "copy of ingitfile should be no-op"
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile
	git_annex "copy" ["-q", "--from", "origin", ingitfile] @? "copy of ingitfile should be no-op"
	checkregularfile ingitfile
	checkcontent ingitfile

test_lock :: Test
test_lock = "git-annex unlock/lock" ~: intmpclonerepo $ do
	-- regression test: unlock of not present file should skip it
	annexed_notpresent annexedfile
	r <- git_annex "unlock" ["-q", annexedfile]
	not r @? "unlock failed to fail with not present file"
	annexed_notpresent annexedfile

	git_annex "get" ["-q", annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex "unlock" ["-q", annexedfile] @? "unlock failed"		
	unannexed annexedfile
	-- write different content, to verify that lock
	-- throws it away
	changecontent annexedfile
	writeFile annexedfile $ content annexedfile ++ "foo"
	git_annex "lock" ["-q", annexedfile] @? "lock failed"
	annexed_present annexedfile
	git_annex "unlock" ["-q", annexedfile] @? "unlock failed"		
	unannexed annexedfile
	changecontent annexedfile
	git_annex "add" ["-q", annexedfile] @? "add of modified file failed"
	runchecks [checklink, checkunwritable] annexedfile
	c <- readFile annexedfile
	assertEqual "content of modified file" c (changedcontent annexedfile)
	r' <- git_annex "drop" ["-q", annexedfile]
	not r' @? "drop wrongly succeeded with no known copy of modified file"

test_edit :: Test
test_edit = "git-annex edit/commit" ~: TestList [t False, t True]
	where t precommit = TestCase $ intmpclonerepo $ do
		git_annex "get" ["-q", annexedfile] @? "get of file failed"
		annexed_present annexedfile
		git_annex "edit" ["-q", annexedfile] @? "edit failed"
		unannexed annexedfile
		changecontent annexedfile
		if precommit
			then do
				-- pre-commit depends on the file being
				-- staged, normally git commit does this
				boolSystem "git" [Param "add", File annexedfile]
					@? "git add of edited file failed"
				git_annex "pre-commit" ["-q"]
					@? "pre-commit failed"
			else do
				boolSystem "git" [Params "commit -q -a -m contentchanged"]
					@? "git commit of edited file failed"
		runchecks [checklink, checkunwritable] annexedfile
		c <- readFile annexedfile
		assertEqual "content of modified file" c (changedcontent annexedfile)
		r <- git_annex "drop" ["-q", annexedfile]
		not r @? "drop wrongly succeeded with no known copy of modified file"

test_fix :: Test
test_fix = "git-annex fix" ~: intmpclonerepo $ do
	annexed_notpresent annexedfile
	git_annex "fix" ["-q", annexedfile] @? "fix of not present failed"
	annexed_notpresent annexedfile
	git_annex "get" ["-q", annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex "fix" ["-q", annexedfile] @? "fix of present file failed"
	annexed_present annexedfile
	createDirectory subdir
	boolSystem "git" [Param "mv", File annexedfile, File subdir]
		@? "git mv failed"
	git_annex "fix" ["-q", newfile] @? "fix of moved file failed"
	runchecks [checklink, checkunwritable] newfile
	c <- readFile newfile
	assertEqual "content of moved file" c (content annexedfile)
	where
		subdir = "s"
		newfile = subdir ++ "/" ++ annexedfile

test_trust :: Test
test_trust = "git-annex trust/untrust/semitrust" ~: intmpclonerepo $ do
	git_annex "trust" ["-q", repo] @? "trust failed"
	trustcheck Logs.Trust.Trusted "trusted 1"
	git_annex "trust" ["-q", repo] @? "trust of trusted failed"
	trustcheck Logs.Trust.Trusted "trusted 2"
	git_annex "untrust" ["-q", repo] @? "untrust failed"
	trustcheck Logs.Trust.UnTrusted "untrusted 1"
	git_annex "untrust" ["-q", repo] @? "untrust of untrusted failed"
	trustcheck Logs.Trust.UnTrusted "untrusted 2"
	git_annex "semitrust" ["-q", repo] @? "semitrust failed"
	trustcheck Logs.Trust.SemiTrusted "semitrusted 1"
	git_annex "semitrust" ["-q", repo] @? "semitrust of semitrusted failed"
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
test_fsck = "git-annex fsck" ~: TestList [basicfsck, withlocaluntrusted, withremoteuntrusted]
	where
		basicfsck = TestCase $ intmpclonerepo $ do
			git_annex "fsck" ["-q"] @? "fsck failed"
			boolSystem "git" [Params "config annex.numcopies 2"] @? "git config failed"
			fsck_should_fail "numcopies unsatisfied"
			boolSystem "git" [Params "config annex.numcopies 1"] @? "git config failed"
			corrupt annexedfile
			corrupt sha1annexedfile
		withlocaluntrusted = TestCase $ intmpclonerepo $ do
			git_annex "get" ["-q", annexedfile] @? "get failed"
			git_annex "untrust" ["-q", "origin"] @? "untrust of origin repo failed"
			git_annex "untrust" ["-q", "."] @? "untrust of current repo failed"
			fsck_should_fail "content only available in untrusted (current) repository"
			git_annex "trust" ["-q", "."] @? "trust of current repo failed"
			git_annex "fsck" ["-q", annexedfile] @? "fsck failed on file present in trusted repo"
		withremoteuntrusted = TestCase $ intmpclonerepo $ do
			boolSystem "git" [Params "config annex.numcopies 2"] @? "git config failed"
			git_annex "get" ["-q", annexedfile] @? "get failed"
			git_annex "get" ["-q", sha1annexedfile] @? "get failed"
			git_annex "fsck" ["-q"] @? "fsck failed with numcopies=2 and 2 copies"
			git_annex "untrust" ["-q", "origin"] @? "untrust of origin failed"
			fsck_should_fail "content not replicated to enough non-untrusted repositories"

		corrupt f = do
			git_annex "get" ["-q", f] @? "get of file failed"
			Utility.FileMode.allowWrite f
			writeFile f (changedcontent f)
			r <- git_annex "fsck" ["-q"]
			not r @? "fsck failed to fail with corrupted file content"
			git_annex "fsck" ["-q"] @? "fsck unexpectedly failed again; previous one did not fix problem with " ++ f
		fsck_should_fail m = do
			r <- git_annex "fsck" ["-q"]
			not r @? "fsck failed to fail with " ++ m

test_migrate :: Test
test_migrate = "git-annex migrate" ~: TestList [t False, t True]
	where t usegitattributes = TestCase $ intmpclonerepo $ do
		annexed_notpresent annexedfile
		annexed_notpresent sha1annexedfile
		git_annex "migrate" ["-q", annexedfile] @? "migrate of not present failed"
		git_annex "migrate" ["-q", sha1annexedfile] @? "migrate of not present failed"
		git_annex "get" ["-q", annexedfile] @? "get of file failed"
		git_annex "get" ["-q", sha1annexedfile] @? "get of file failed"
		annexed_present annexedfile
		annexed_present sha1annexedfile
		if usegitattributes
			then do
				writeFile ".gitattributes" $ "* annex.backend=SHA1"
				git_annex "migrate" ["-q", sha1annexedfile]
					@? "migrate sha1annexedfile failed"
				git_annex "migrate" ["-q", annexedfile]
					@? "migrate annexedfile failed"
			else do
				git_annex "migrate" ["-q", sha1annexedfile, "--backend", "SHA1"]
					@? "migrate sha1annexedfile failed"
				git_annex "migrate" ["-q", annexedfile, "--backend", "SHA1"]
					@? "migrate annexedfile failed"
		annexed_present annexedfile
		annexed_present sha1annexedfile
		checkbackend annexedfile backendSHA1
		checkbackend sha1annexedfile backendSHA1

		-- check that reversing a migration works
		writeFile ".gitattributes" $ "* annex.backend=WORM"
		git_annex "migrate" ["-q", sha1annexedfile]
			@? "migrate sha1annexedfile failed"
		git_annex "migrate" ["-q", annexedfile]
			@? "migrate annexedfile failed"
		annexed_present annexedfile
		annexed_present sha1annexedfile
		checkbackend annexedfile backendWORM
		checkbackend sha1annexedfile backendWORM
		
		where
			checkbackend file expected = do
				r <- annexeval $ Backend.lookupFile file
				let b = snd $ fromJust r
				assertEqual ("backend for " ++ file) expected b

test_unused :: Test
test_unused = "git-annex unused/dropunused" ~: intmpclonerepo $ do
	-- keys have to be looked up before files are removed
	annexedfilekey <- annexeval $ findkey annexedfile
	sha1annexedfilekey <- annexeval $ findkey sha1annexedfile
	git_annex "get" ["-q", annexedfile] @? "get of file failed"
	git_annex "get" ["-q", sha1annexedfile] @? "get of file failed"
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
	git_annex "dropkey" ["-q", "--force", show annexedfilekey]
		@? "dropkey failed"
	checkunused [sha1annexedfilekey]

	git_annex "dropunused" ["-q", "1", "2"] @? "dropunused failed"
	checkunused []
	git_annex "dropunused" ["-q", "10", "501"] @? "dropunused failed on bogus numbers"

	where
		checkunused expectedkeys = do
			git_annex "unused" ["-q"] @? "unused failed"
			unusedmap <- annexeval $ Command.DropUnused.readUnusedLog ""
			let unusedkeys = M.elems unusedmap
			assertEqual "unused keys differ"
				(sort expectedkeys) (sort unusedkeys)
		findkey f = do
			r <- Backend.lookupFile f
			return $ fst $ fromJust r

-- This is equivilant to running git-annex, but it's all run in-process
-- so test coverage collection works.
git_annex :: String -> [String] -> IO Bool
git_annex command params = do
	-- catch all errors, including normally fatal errors
	r <- E.try (run)::IO (Either E.SomeException ())
	case r of
		Right _ -> return True
		Left _ -> return False
	where
		run = GitAnnex.run (command:params)

-- Runs an action in the current annex. Note that shutdown actions
-- are not run; this should only be used for actions that query state.
annexeval :: Types.Annex a -> IO a
annexeval a = do
	g <- Git.Construct.fromCwd
	g' <- Git.Config.read g
	s <- Annex.new g'
	Annex.eval s a

innewrepo :: Assertion -> Assertion
innewrepo a = withgitrepo $ \r -> indir r a

inmainrepo :: Assertion -> Assertion
inmainrepo a = indir mainrepodir a

intmpclonerepo :: Assertion -> Assertion
intmpclonerepo a = withtmpclonerepo $ \r -> indir r a

withtmpclonerepo :: (FilePath -> Assertion) -> Assertion
withtmpclonerepo = bracket (clonerepo mainrepodir tmprepodir) cleanup

withgitrepo :: (FilePath -> Assertion) -> Assertion
withgitrepo = bracket (setuprepo mainrepodir) return

indir :: FilePath -> Assertion -> Assertion
indir dir a = do
	cwd <- getCurrentDirectory
	-- Assertion failures throw non-IO errors; catch
	-- any type of error and change back to cwd before
	-- rethrowing.
	r <- bracket_ (changeToTmpDir dir) (changeWorkingDirectory cwd)
		(E.try (a)::IO (Either E.SomeException ()))
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
clonerepo :: FilePath -> FilePath -> IO FilePath
clonerepo old new = do
	cleanup new
	ensuretmpdir
	boolSystem "git" [Params "clone -q", File old, File new] @? "git clone failed"
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
	r <- try $ writeFile f $ content f
	case r of
		Left _ -> assertFailure $ "unable to modify " ++ f
		Right _ -> return ()

checkdangling :: FilePath -> Assertion
checkdangling f = do
	r <- try $ readFile f
	case r of
		Left _ -> return () -- expected; dangling link
		Right _ -> assertFailure $ f ++ " was not a dangling link as expected"

checklocationlog :: FilePath -> Bool -> Assertion
checklocationlog f expected = do
	thisuuid <- annexeval Annex.UUID.getUUID
	r <- annexeval $ Backend.lookupFile f
	case r of
		Just (k, _) -> do
			uuids <- annexeval $ Logs.Location.keyLocations k
			assertEqual ("bad content in location log for " ++ f ++ " key " ++ (show k) ++ " uuid " ++ show thisuuid)
				expected (thisuuid `elem` uuids)
		_ -> assertFailure $ f ++ " failed to look up key"

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
	| otherwise = "unknown file " ++ f

changecontent :: FilePath -> IO ()
changecontent f = writeFile f $ changedcontent f

changedcontent :: FilePath -> String
changedcontent f = (content f) ++ " (modified)"

backendSHA1 :: Types.Backend Types.Annex
backendSHA1 = backend_ "SHA1"

backendWORM :: Types.Backend Types.Annex
backendWORM = backend_ "WORM"

backend_ :: String -> Types.Backend Types.Annex
backend_ name = Backend.lookupBackendName name
