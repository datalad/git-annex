{- git-annex file locations
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Locations (
	keyFile,
	fileKey,
	keyPaths,
	keyPath,
	annexDir,
	objectDir,
	gitAnnexLocation,
	gitAnnexLocationDepth,
	gitAnnexLink,
	gitAnnexLinkCanonical,
	gitAnnexContentLock,
	gitAnnexMapping,
	gitAnnexInodeCache,
	gitAnnexInodeSentinal,
	gitAnnexInodeSentinalCache,
	annexLocations,
	gitAnnexDir,
	gitAnnexObjectDir,
	gitAnnexTmpMiscDir,
	gitAnnexTmpObjectDir,
	gitAnnexTmpObjectLocation,
	gitAnnexBadDir,
	gitAnnexBadLocation,
	gitAnnexUnusedLog,
	gitAnnexKeysDb,
	gitAnnexKeysDbLock,
	gitAnnexFsckState,
	gitAnnexFsckDbDir,
	gitAnnexFsckDbLock,
	gitAnnexFsckResultsLog,
	gitAnnexScheduleState,
	gitAnnexTransferDir,
	gitAnnexCredsDir,
	gitAnnexWebCertificate,
	gitAnnexWebPrivKey,
	gitAnnexFeedStateDir,
	gitAnnexFeedState,
	gitAnnexMergeDir,
	gitAnnexJournalDir,
	gitAnnexJournalLock,
	gitAnnexPreCommitLock,
	gitAnnexMergeLock,
	gitAnnexIndex,
	gitAnnexIndexStatus,
	gitAnnexViewIndex,
	gitAnnexViewLog,
	gitAnnexIgnoredRefs,
	gitAnnexPidFile,
	gitAnnexPidLockFile,
	gitAnnexDaemonStatusFile,
	gitAnnexLogFile,
	gitAnnexFuzzTestLogFile,
	gitAnnexHtmlShim,
	gitAnnexUrlFile,
	gitAnnexTmpCfgFile,
	gitAnnexSshDir,
	gitAnnexRemotesDir,
	gitAnnexAssistantDefaultDir,
	HashLevels(..),
	hashDirMixed,
	hashDirLower,
	preSanitizeKeyName,

	prop_isomorphic_fileKey
) where

import Data.Char
import Data.Default

import Common
import Types.Key
import Types.UUID
import Types.GitConfig
import Types.Difference
import qualified Git
import qualified Git.Types as Git
import Git.FilePath
import Annex.DirHashes
import Annex.Fixup

{- Conventions:
 -
 - Functions ending in "Dir" should always return values ending with a
 - trailing path separator. Most code does not rely on that, but a few
 - things do. 
 -
 - Everything else should not end in a trailing path sepatator. 
 -
 - Only functions (with names starting with "git") that build a path
 - based on a git repository should return full path relative to the git
 - repository. Everything else returns path segments.
 -}

{- The directory git annex uses for local state, relative to the .git
 - directory -}
annexDir :: FilePath
annexDir = addTrailingPathSeparator "annex"

{- The directory git annex uses for locally available object content,
 - relative to the .git directory -}
objectDir :: FilePath
objectDir = addTrailingPathSeparator $ annexDir </> "objects"

{- Annexed file's possible locations relative to the .git directory.
 - There are two different possibilities, using different hashes.
 -
 - Also, some repositories have a Difference in hash directory depth.
 -}
annexLocations :: GitConfig -> Key -> [FilePath]
annexLocations config key = map (annexLocation config key) dirHashes

annexLocation :: GitConfig -> Key -> (HashLevels -> Hasher) -> FilePath
annexLocation config key hasher = objectDir </> keyPath key (hasher $ objectHashLevels config)

{- Number of subdirectories from the gitAnnexObjectDir
 - to the gitAnnexLocation. -}
gitAnnexLocationDepth :: GitConfig -> Int
gitAnnexLocationDepth config = hashlevels + 1
  where
	HashLevels hashlevels = objectHashLevels config

{- Annexed object's location in a repository.
 -
 - When there are multiple possible locations, returns the one where the
 - file is actually present.
 -
 - When the file is not present, returns the location where the file should
 - be stored.
 -
 - This does not take direct mode into account, so in direct mode it is not
 - the actual location of the file's content.
 -}
gitAnnexLocation :: Key -> Git.Repo -> GitConfig -> IO FilePath
gitAnnexLocation key r config = gitAnnexLocation' key r config (annexCrippledFileSystem config) (coreSymlinks config) doesFileExist (Git.localGitDir r)
gitAnnexLocation' :: Key -> Git.Repo -> GitConfig -> Bool -> Bool -> (FilePath -> IO Bool) -> FilePath -> IO FilePath
gitAnnexLocation' key r config crippled symlinkssupported checker gitdir
	{- Bare repositories default to hashDirLower for new
	 - content, as it's more portable. But check all locations. -}
	| Git.repoIsLocalBare r = checkall
	| hasDifference ObjectHashLower (annexDifferences config) = 
		only hashDirLower
	{- Repositories on crippled filesystems use hashDirLower
	 - for new content, unless symlinks are supported too.
	 - Then hashDirMixed is used. But, the content could be
	 - in either location so check both. -}
	| symlinkssupported = check $ map inrepo $ reverse $ annexLocations config key
	| crippled = checkall
	{- Regular repositories only use hashDirMixed, so
	 - don't need to do any work to check if the file is
	 - present. -}
	| otherwise = only hashDirMixed
  where
	only = return . inrepo . annexLocation config key
	checkall = check $ map inrepo $ annexLocations config key

	inrepo d = gitdir </> d
	check locs@(l:_) = fromMaybe l <$> firstM checker locs
	check [] = error "internal"

{- Calculates a symlink target to link a file to an annexed object. -}
gitAnnexLink :: FilePath -> Key -> Git.Repo -> GitConfig -> IO FilePath
gitAnnexLink file key r config = do
	currdir <- getCurrentDirectory
	let absfile = fromMaybe whoops $ absNormPathUnix currdir file
	let gitdir = getgitdir currdir
	loc <- gitAnnexLocation' key r config False False (\_ -> return True) gitdir
	toInternalGitPath <$> relPathDirToFile (parentDir absfile) loc
  where
	getgitdir currdir
		{- This special case is for git submodules on filesystems not
		 - supporting symlinks; generate link target that will
		 - work portably. -}
		| not (coreSymlinks config) && needsSubmoduleFixup r =
			fromMaybe whoops $ absNormPathUnix currdir $
				Git.repoPath r </> ".git"
		| otherwise = Git.localGitDir r
	whoops = error $ "unable to normalize " ++ file

{- Calculates a symlink target as would be used in a typical git
 - repository, with .git in the top of the work tree. -}
gitAnnexLinkCanonical :: FilePath -> Key -> Git.Repo -> GitConfig -> IO FilePath
gitAnnexLinkCanonical file key r config = gitAnnexLink file key r' config'
  where
	r' = case r of
		Git.Repo { Git.location = l@Git.Local { Git.worktree = Just wt } } ->
			r { Git.location = l { Git.gitdir = wt </> ".git" } }
		_ -> r
	config' = config
		{ annexCrippledFileSystem = False
		, coreSymlinks = True
		}

{- File used to lock a key's content. -}
gitAnnexContentLock :: Key -> Git.Repo -> GitConfig -> IO FilePath
gitAnnexContentLock key r config = do
	loc <- gitAnnexLocation key r config
	return $ loc ++ ".lck"

{- File that maps from a key to the file(s) in the git repository.
 - Used in direct mode. -}
gitAnnexMapping :: Key -> Git.Repo -> GitConfig -> IO FilePath
gitAnnexMapping key r config = do
	loc <- gitAnnexLocation key r config
	return $ loc ++ ".map"

{- File that caches information about a key's content, used to determine
 - if a file has changed.
 - Used in direct mode. -}
gitAnnexInodeCache :: Key -> Git.Repo -> GitConfig -> IO FilePath
gitAnnexInodeCache key r config  = do
	loc <- gitAnnexLocation key r config
	return $ loc ++ ".cache"

gitAnnexInodeSentinal :: Git.Repo -> FilePath
gitAnnexInodeSentinal r = gitAnnexDir r </> "sentinal"

gitAnnexInodeSentinalCache :: Git.Repo -> FilePath
gitAnnexInodeSentinalCache r = gitAnnexInodeSentinal r ++ ".cache"

{- The annex directory of a repository. -}
gitAnnexDir :: Git.Repo -> FilePath
gitAnnexDir r = addTrailingPathSeparator $ Git.localGitDir r </> annexDir

{- The part of the annex directory where file contents are stored. -}
gitAnnexObjectDir :: Git.Repo -> FilePath
gitAnnexObjectDir r = addTrailingPathSeparator $ Git.localGitDir r </> objectDir

{- .git/annex/misctmp/ is used for random temp files -}
gitAnnexTmpMiscDir :: Git.Repo -> FilePath
gitAnnexTmpMiscDir r = addTrailingPathSeparator $ gitAnnexDir r </> "misctmp"

{- .git/annex/tmp/ is used for temp files for key's contents -}
gitAnnexTmpObjectDir :: Git.Repo -> FilePath
gitAnnexTmpObjectDir r = addTrailingPathSeparator $ gitAnnexDir r </> "tmp"

{- The temp file to use for a given key's content. -}
gitAnnexTmpObjectLocation :: Key -> Git.Repo -> FilePath
gitAnnexTmpObjectLocation key r = gitAnnexTmpObjectDir r </> keyFile key

{- .git/annex/bad/ is used for bad files found during fsck -}
gitAnnexBadDir :: Git.Repo -> FilePath
gitAnnexBadDir r = addTrailingPathSeparator $ gitAnnexDir r </> "bad"

{- The bad file to use for a given key. -}
gitAnnexBadLocation :: Key -> Git.Repo -> FilePath
gitAnnexBadLocation key r = gitAnnexBadDir r </> keyFile key

{- .git/annex/foounused is used to number possibly unused keys -}
gitAnnexUnusedLog :: FilePath -> Git.Repo -> FilePath
gitAnnexUnusedLog prefix r = gitAnnexDir r </> (prefix ++ "unused")

{- .git/annex/keys/ contains a database of information about keys. -}
gitAnnexKeysDb :: Git.Repo -> FilePath
gitAnnexKeysDb r = gitAnnexDir r </> "keys"

{- Lock file for the keys database. -}
gitAnnexKeysDbLock :: Git.Repo -> FilePath
gitAnnexKeysDbLock r = gitAnnexKeysDb r ++ ".lck"

{- .git/annex/fsck/uuid/ is used to store information about incremental
 - fscks. -}
gitAnnexFsckDir :: UUID -> Git.Repo -> FilePath
gitAnnexFsckDir u r = gitAnnexDir r </> "fsck" </> fromUUID u

{- used to store information about incremental fscks. -}
gitAnnexFsckState :: UUID -> Git.Repo -> FilePath
gitAnnexFsckState u r = gitAnnexFsckDir u r </> "state"

{- Directory containing database used to record fsck info. -}
gitAnnexFsckDbDir :: UUID -> Git.Repo -> FilePath
gitAnnexFsckDbDir u r = gitAnnexFsckDir u r </> "db"

{- Lock file for the fsck database. -}
gitAnnexFsckDbLock :: UUID -> Git.Repo -> FilePath
gitAnnexFsckDbLock u r = gitAnnexFsckDir u r </> "fsck.lck"

{- .git/annex/fsckresults/uuid is used to store results of git fscks -}
gitAnnexFsckResultsLog :: UUID -> Git.Repo -> FilePath
gitAnnexFsckResultsLog u r = gitAnnexDir r </> "fsckresults" </> fromUUID u

{- .git/annex/schedulestate is used to store information about when
 - scheduled jobs were last run. -}
gitAnnexScheduleState :: Git.Repo -> FilePath
gitAnnexScheduleState r = gitAnnexDir r </> "schedulestate"

{- .git/annex/creds/ is used to store credentials to access some special
 - remotes. -}
gitAnnexCredsDir :: Git.Repo -> FilePath
gitAnnexCredsDir r = addTrailingPathSeparator $ gitAnnexDir r </> "creds"

{- .git/annex/certificate.pem and .git/annex/key.pem are used by the webapp
 - when HTTPS is enabled -}
gitAnnexWebCertificate :: Git.Repo -> FilePath
gitAnnexWebCertificate r = gitAnnexDir r </> "certificate.pem"
gitAnnexWebPrivKey :: Git.Repo -> FilePath
gitAnnexWebPrivKey r = gitAnnexDir r </> "privkey.pem"

{- .git/annex/feeds/ is used to record per-key (url) state by importfeeds -}
gitAnnexFeedStateDir :: Git.Repo -> FilePath
gitAnnexFeedStateDir r = addTrailingPathSeparator $ gitAnnexDir r </> "feedstate"

gitAnnexFeedState :: Key -> Git.Repo -> FilePath
gitAnnexFeedState k r = gitAnnexFeedStateDir r </> keyFile k

{- .git/annex/merge/ is used as a empty work tree for direct mode merges and
 - merges in adjusted branches. -}
gitAnnexMergeDir :: Git.Repo -> FilePath
gitAnnexMergeDir r = addTrailingPathSeparator $ gitAnnexDir r </> "merge"

{- .git/annex/transfer/ is used to record keys currently
 - being transferred, and other transfer bookkeeping info. -}
gitAnnexTransferDir :: Git.Repo -> FilePath
gitAnnexTransferDir r = addTrailingPathSeparator $ gitAnnexDir r </> "transfer"

{- .git/annex/journal/ is used to journal changes made to the git-annex
 - branch -}
gitAnnexJournalDir :: Git.Repo -> FilePath
gitAnnexJournalDir r = addTrailingPathSeparator $ gitAnnexDir r </> "journal"

{- Lock file for the journal. -}
gitAnnexJournalLock :: Git.Repo -> FilePath
gitAnnexJournalLock r = gitAnnexDir r </> "journal.lck"

{- Lock file for the pre-commit hook. -}
gitAnnexPreCommitLock :: Git.Repo -> FilePath
gitAnnexPreCommitLock r = gitAnnexDir r </> "precommit.lck"

{- Lock file for direct mode merge. -}
gitAnnexMergeLock :: Git.Repo -> FilePath
gitAnnexMergeLock r = gitAnnexDir r </> "merge.lck"

{- .git/annex/index is used to stage changes to the git-annex branch -}
gitAnnexIndex :: Git.Repo -> FilePath
gitAnnexIndex r = gitAnnexDir r </> "index"

{- Holds the ref of the git-annex branch that the index was last updated to.
 -
 - The .lck in the name is a historical accident; this is not used as a
 - lock. -}
gitAnnexIndexStatus :: Git.Repo -> FilePath
gitAnnexIndexStatus r = gitAnnexDir r </> "index.lck"

{- The index file used to generate a filtered branch view._-}
gitAnnexViewIndex :: Git.Repo -> FilePath
gitAnnexViewIndex r = gitAnnexDir r </> "viewindex"

{- File containing a log of recently accessed views. -}
gitAnnexViewLog :: Git.Repo -> FilePath
gitAnnexViewLog r = gitAnnexDir r </> "viewlog"

{- List of refs that should not be merged into the git-annex branch. -}
gitAnnexIgnoredRefs :: Git.Repo -> FilePath
gitAnnexIgnoredRefs r = gitAnnexDir r </> "ignoredrefs"

{- Pid file for daemon mode. -}
gitAnnexPidFile :: Git.Repo -> FilePath
gitAnnexPidFile r = gitAnnexDir r </> "daemon.pid"

{- Pid lock file for pidlock mode -}
gitAnnexPidLockFile :: Git.Repo -> FilePath
gitAnnexPidLockFile r = gitAnnexDir r </> "pidlock"

{- Status file for daemon mode. -}
gitAnnexDaemonStatusFile :: Git.Repo -> FilePath
gitAnnexDaemonStatusFile r = gitAnnexDir r </> "daemon.status"

{- Log file for daemon mode. -}
gitAnnexLogFile :: Git.Repo -> FilePath
gitAnnexLogFile r = gitAnnexDir r </> "daemon.log"

{- Log file for fuzz test. -}
gitAnnexFuzzTestLogFile :: Git.Repo -> FilePath
gitAnnexFuzzTestLogFile r = gitAnnexDir r </> "fuzztest.log"

{- Html shim file used to launch the webapp. -}
gitAnnexHtmlShim :: Git.Repo -> FilePath
gitAnnexHtmlShim r = gitAnnexDir r </> "webapp.html"

{- File containing the url to the webapp. -}
gitAnnexUrlFile :: Git.Repo -> FilePath
gitAnnexUrlFile r = gitAnnexDir r </> "url"

{- Temporary file used to edit configuriation from the git-annex branch. -}
gitAnnexTmpCfgFile :: Git.Repo -> FilePath
gitAnnexTmpCfgFile r = gitAnnexDir r </> "config.tmp"

{- .git/annex/ssh/ is used for ssh connection caching -}
gitAnnexSshDir :: Git.Repo -> FilePath
gitAnnexSshDir r = addTrailingPathSeparator $ gitAnnexDir r </> "ssh"

{- .git/annex/remotes/ is used for remote-specific state. -}
gitAnnexRemotesDir :: Git.Repo -> FilePath
gitAnnexRemotesDir r = addTrailingPathSeparator $ gitAnnexDir r </> "remotes"

{- This is the base directory name used by the assistant when making
 - repositories, by default. -}
gitAnnexAssistantDefaultDir :: FilePath
gitAnnexAssistantDefaultDir = "annex"

{- Sanitizes a String that will be used as part of a Key's keyName,
 - dealing with characters that cause problems on substandard filesystems.
 -
 - This is used when a new Key is initially being generated, eg by getKey.
 - Unlike keyFile and fileKey, it does not need to be a reversable
 - escaping. Also, it's ok to change this to add more problimatic
 - characters later. Unlike changing keyFile, which could result in the
 - filenames used for existing keys changing and contents getting lost.
 -
 - It is, however, important that the input and output of this function
 - have a 1:1 mapping, to avoid two different inputs from mapping to the
 - same key.
 -}
preSanitizeKeyName :: String -> String
preSanitizeKeyName = concatMap escape
  where
	escape c
		| isAsciiUpper c || isAsciiLower c || isDigit c = [c]
		| c `elem` ".-_ " = [c] -- common, assumed safe
		| c `elem` "/%:" = [c] -- handled by keyFile
		-- , is safe and uncommon, so will be used to escape
		-- other characters. By itself, it is escaped to 
		-- doubled form.
		| c == ',' = ",,"
		| otherwise = ',' : show (ord c)

{- Converts a key into a filename fragment without any directory.
 -
 - Escape "/" in the key name, to keep a flat tree of files and avoid
 - issues with keys containing "/../" or ending with "/" etc. 
 -
 - "/" is escaped to "%" because it's short and rarely used, and resembles
 -     a slash
 - "%" is escaped to "&s", and "&" to "&a"; this ensures that the mapping
 -     is one to one.
 - ":" is escaped to "&c", because it seemed like a good idea at the time.
 -
 - Changing what this function escapes and how is not a good idea, as it
 - can cause existing objects to get lost.
 -}
keyFile :: Key -> FilePath
keyFile key = replace "/" "%" $ replace ":" "&c" $
	replace "%" "&s" $ replace "&" "&a"  $ key2file key

{- Reverses keyFile, converting a filename fragment (ie, the basename of
 - the symlink target) into a key. -}
fileKey :: FilePath -> Maybe Key
fileKey file = file2key $
	replace "&a" "&" $ replace "&s" "%" $
		replace "&c" ":" $ replace "%" "/" file

{- for quickcheck -}
prop_isomorphic_fileKey :: String -> Bool
prop_isomorphic_fileKey s
	| null s = True -- it's not legal for a key to have no keyName
	| otherwise= Just k == fileKey (keyFile k)
  where
	k = stubKey { keyName = s, keyBackendName = "test" }

{- A location to store a key on a special remote that uses a filesystem.
 - A directory hash is used, to protect against filesystems that dislike
 - having many items in a single directory.
 -
 - The file is put in a directory with the same name, this allows
 - write-protecting the directory to avoid accidental deletion of the file.
 -}
keyPath :: Key -> Hasher -> FilePath
keyPath key hasher = hasher key </> f </> f
  where
	f = keyFile key

{- All possibile locations to store a key in a special remote
 - using different directory hashes.
 -
 - This is compatible with the annexLocations, for interoperability between
 - special remotes and git-annex repos.
 -}
keyPaths :: Key -> [FilePath]
keyPaths key = map (\h -> keyPath key (h def)) dirHashes
