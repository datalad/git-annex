{- git-annex file locations
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.Locations (
	keyFile,
	fileKey,
	keyPaths,
	keyPath,
	annexDir,
	objectDir,
	objectDir',
	gitAnnexLocation,
	gitAnnexLocationDepth,
	gitAnnexLink,
	gitAnnexLinkCanonical,
	gitAnnexContentLock,
	gitAnnexInodeSentinal,
	gitAnnexInodeSentinalCache,
	annexLocations,
	gitAnnexDir,
	gitAnnexObjectDir,
	gitAnnexTmpOtherDir,
	gitAnnexTmpOtherLock,
	gitAnnexTmpOtherDirOld,
	gitAnnexTmpWatcherDir,
	gitAnnexTmpObjectDir,
	gitAnnexTmpObjectLocation,
	gitAnnexTmpWorkDir,
	gitAnnexBadDir,
	gitAnnexBadLocation,
	gitAnnexUnusedLog,
	gitAnnexKeysDb,
	gitAnnexKeysDbLock,
	gitAnnexKeysDbIndexCache,
	gitAnnexFsckState,
	gitAnnexFsckDbDir,
	gitAnnexFsckDbDirOld,
	gitAnnexFsckDbLock,
	gitAnnexFsckResultsLog,
	gitAnnexSmudgeLog,
	gitAnnexSmudgeLock,
	gitAnnexMoveLog,
	gitAnnexMoveLock,
	gitAnnexExportDir,
	gitAnnexExportDbDir,
	gitAnnexExportLock,
	gitAnnexExportUpdateLock,
	gitAnnexExportExcludeLog,
	gitAnnexContentIdentifierDbDir,
	gitAnnexContentIdentifierLock,
	gitAnnexScheduleState,
	gitAnnexTransferDir,
	gitAnnexCredsDir,
	gitAnnexWebCertificate,
	gitAnnexWebPrivKey,
	gitAnnexFeedStateDir,
	gitAnnexFeedState,
	gitAnnexMergeDir,
	gitAnnexJournalDir,
	gitAnnexJournalDir',
	gitAnnexJournalLock,
	gitAnnexGitQueueLock,
	gitAnnexIndex,
	gitAnnexIndexStatus,
	gitAnnexViewIndex,
	gitAnnexViewLog,
	gitAnnexMergedRefs,
	gitAnnexIgnoredRefs,
	gitAnnexPidFile,
	gitAnnexPidLockFile,
	gitAnnexDaemonStatusFile,
	gitAnnexDaemonLogFile,
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
	reSanitizeKeyName,
) where

import Data.Char
import Data.Default
import qualified Data.ByteString.Char8 as S8
import qualified System.FilePath.ByteString as P

import Common
import Key
import Types.UUID
import Types.GitConfig
import Types.Difference
import qualified Git
import qualified Git.Types as Git
import Git.FilePath
import Annex.DirHashes
import Annex.Fixup
import qualified Utility.RawFilePath as R

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
annexDir :: RawFilePath
annexDir = P.addTrailingPathSeparator "annex"

{- The directory git annex uses for locally available object content,
 - relative to the .git directory -}
objectDir :: FilePath
objectDir = fromRawFilePath objectDir'

objectDir' :: RawFilePath
objectDir' = P.addTrailingPathSeparator $ annexDir P.</> "objects"

{- Annexed file's possible locations relative to the .git directory.
 - There are two different possibilities, using different hashes.
 -
 - Also, some repositories have a Difference in hash directory depth.
 -}
annexLocations :: GitConfig -> Key -> [RawFilePath]
annexLocations config key = map (annexLocation config key) dirHashes

annexLocation :: GitConfig -> Key -> (HashLevels -> Hasher) -> RawFilePath
annexLocation config key hasher = objectDir' P.</> keyPath key (hasher $ objectHashLevels config)

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
 -}
gitAnnexLocation :: Key -> Git.Repo -> GitConfig -> IO RawFilePath
gitAnnexLocation key r config = gitAnnexLocation' key r config
	(annexCrippledFileSystem config)
	(coreSymlinks config)
	R.doesPathExist
	(Git.localGitDir r)

gitAnnexLocation' :: Key -> Git.Repo -> GitConfig -> Bool -> Bool -> (RawFilePath -> IO Bool) -> RawFilePath -> IO RawFilePath
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
	| crippled = if symlinkssupported
		then check $ map inrepo $ reverse $ annexLocations config key
		else checkall
	{- Regular repositories only use hashDirMixed, so
	 - don't need to do any work to check if the file is
	 - present. -}
	| otherwise = only hashDirMixed
  where
	only = return . inrepo . annexLocation config key
	checkall = check $ map inrepo $ annexLocations config key

	inrepo d = gitdir P.</> d
	check locs@(l:_) = fromMaybe l <$> firstM checker locs
	check [] = error "internal"

{- Calculates a symlink target to link a file to an annexed object. -}
gitAnnexLink :: RawFilePath -> Key -> Git.Repo -> GitConfig -> IO RawFilePath
gitAnnexLink file key r config = do
	currdir <- R.getCurrentDirectory
	let absfile = absNormPathUnix currdir file
	let gitdir = getgitdir currdir
	loc <- gitAnnexLocation' key r config False False (\_ -> return True) gitdir
	toInternalGitPath <$> relPathDirToFile (parentDir absfile) loc
  where
	getgitdir currdir
		{- This special case is for git submodules on filesystems not
		 - supporting symlinks; generate link target that will
		 - work portably. -}
		| not (coreSymlinks config) && needsSubmoduleFixup r =
			absNormPathUnix currdir (Git.repoPath r P.</> ".git")
		| otherwise = Git.localGitDir r
	absNormPathUnix d p = toInternalGitPath $
		absPathFrom (toInternalGitPath d) (toInternalGitPath p)

{- Calculates a symlink target as would be used in a typical git
 - repository, with .git in the top of the work tree. -}
gitAnnexLinkCanonical :: RawFilePath -> Key -> Git.Repo -> GitConfig -> IO RawFilePath
gitAnnexLinkCanonical file key r config = gitAnnexLink file key r' config'
  where
	r' = case r of
		Git.Repo { Git.location = l@Git.Local { Git.worktree = Just wt } } ->
			r { Git.location = l { Git.gitdir = wt P.</> ".git" } }
		_ -> r
	config' = config
		{ annexCrippledFileSystem = False
		, coreSymlinks = True
		}

{- File used to lock a key's content. -}
gitAnnexContentLock :: Key -> Git.Repo -> GitConfig -> IO RawFilePath
gitAnnexContentLock key r config = do
	loc <- gitAnnexLocation key r config
	return $ loc <> ".lck"

gitAnnexInodeSentinal :: Git.Repo -> RawFilePath
gitAnnexInodeSentinal r = gitAnnexDir r P.</> "sentinal"

gitAnnexInodeSentinalCache :: Git.Repo -> RawFilePath
gitAnnexInodeSentinalCache r = gitAnnexInodeSentinal r <> ".cache"

{- The annex directory of a repository. -}
gitAnnexDir :: Git.Repo -> RawFilePath
gitAnnexDir r = P.addTrailingPathSeparator $ Git.localGitDir r P.</> annexDir

{- The part of the annex directory where file contents are stored. -}
gitAnnexObjectDir :: Git.Repo -> RawFilePath
gitAnnexObjectDir r = P.addTrailingPathSeparator $
	Git.localGitDir r P.</> objectDir'

{- .git/annex/tmp/ is used for temp files for key's contents -}
gitAnnexTmpObjectDir :: Git.Repo -> RawFilePath
gitAnnexTmpObjectDir r = P.addTrailingPathSeparator $
	gitAnnexDir r P.</> "tmp"

{- .git/annex/othertmp/ is used for other temp files -}
gitAnnexTmpOtherDir :: Git.Repo -> RawFilePath
gitAnnexTmpOtherDir r = P.addTrailingPathSeparator $
	gitAnnexDir r P.</> "othertmp"

{- Lock file for gitAnnexTmpOtherDir. -}
gitAnnexTmpOtherLock :: Git.Repo -> RawFilePath
gitAnnexTmpOtherLock r = gitAnnexDir r P.</> "othertmp.lck"

{- .git/annex/misctmp/ was used by old versions of git-annex and is still
 - used during initialization -}
gitAnnexTmpOtherDirOld :: Git.Repo -> RawFilePath
gitAnnexTmpOtherDirOld r = P.addTrailingPathSeparator $ 
	gitAnnexDir r P.</> "misctmp"

{- .git/annex/watchtmp/ is used by the watcher and assistant -}
gitAnnexTmpWatcherDir :: Git.Repo -> RawFilePath
gitAnnexTmpWatcherDir r = P.addTrailingPathSeparator $
	gitAnnexDir r P.</> "watchtmp"

{- The temp file to use for a given key's content. -}
gitAnnexTmpObjectLocation :: Key -> Git.Repo -> RawFilePath
gitAnnexTmpObjectLocation key r = gitAnnexTmpObjectDir r P.</> keyFile key

{- Given a temp file such as gitAnnexTmpObjectLocation, makes a name for a
 - subdirectory in the same location, that can be used as a work area
 - when receiving the key's content.
 -
 - There are ordering requirements for creating these directories;
 - use Annex.Content.withTmpWorkDir to set them up.
 -}
gitAnnexTmpWorkDir :: RawFilePath -> RawFilePath
gitAnnexTmpWorkDir p =
	let (dir, f) = P.splitFileName p
	-- Using a prefix avoids name conflict with any other keys.
	in dir P.</> "work." <> f

{- .git/annex/bad/ is used for bad files found during fsck -}
gitAnnexBadDir :: Git.Repo -> RawFilePath
gitAnnexBadDir r = P.addTrailingPathSeparator $ gitAnnexDir r P.</> "bad"

{- The bad file to use for a given key. -}
gitAnnexBadLocation :: Key -> Git.Repo -> RawFilePath
gitAnnexBadLocation key r = gitAnnexBadDir r P.</> keyFile key

{- .git/annex/foounused is used to number possibly unused keys -}
gitAnnexUnusedLog :: RawFilePath -> Git.Repo -> RawFilePath
gitAnnexUnusedLog prefix r = gitAnnexDir r P.</> (prefix <> "unused")

{- .git/annex/keysdb/ contains a database of information about keys. -}
gitAnnexKeysDb :: Git.Repo -> RawFilePath
gitAnnexKeysDb r = gitAnnexDir r P.</> "keysdb"

{- Lock file for the keys database. -}
gitAnnexKeysDbLock :: Git.Repo -> RawFilePath
gitAnnexKeysDbLock r = gitAnnexKeysDb r <> ".lck"

{- Contains the stat of the last index file that was
 - reconciled with the keys database. -}
gitAnnexKeysDbIndexCache :: Git.Repo -> RawFilePath
gitAnnexKeysDbIndexCache r = gitAnnexKeysDb r <> ".cache"

{- .git/annex/fsck/uuid/ is used to store information about incremental
 - fscks. -}
gitAnnexFsckDir :: UUID -> Git.Repo -> RawFilePath
gitAnnexFsckDir u r = gitAnnexDir r P.</> "fsck" P.</> fromUUID u

{- used to store information about incremental fscks. -}
gitAnnexFsckState :: UUID -> Git.Repo -> RawFilePath
gitAnnexFsckState u r = gitAnnexFsckDir u r P.</> "state"

{- Directory containing database used to record fsck info. -}
gitAnnexFsckDbDir :: UUID -> Git.Repo -> RawFilePath
gitAnnexFsckDbDir u r = gitAnnexFsckDir u r P.</> "fsckdb"

{- Directory containing old database used to record fsck info. -}
gitAnnexFsckDbDirOld :: UUID -> Git.Repo -> RawFilePath
gitAnnexFsckDbDirOld u r = gitAnnexFsckDir u r P.</> "db"

{- Lock file for the fsck database. -}
gitAnnexFsckDbLock :: UUID -> Git.Repo -> RawFilePath
gitAnnexFsckDbLock u r = gitAnnexFsckDir u r P.</> "fsck.lck"

{- .git/annex/fsckresults/uuid is used to store results of git fscks -}
gitAnnexFsckResultsLog :: UUID -> Git.Repo -> RawFilePath
gitAnnexFsckResultsLog u r = 
	gitAnnexDir r P.</> "fsckresults" P.</> fromUUID u

{- .git/annex/smudge.log is used to log smudges worktree files that need to
 - be updated. -}
gitAnnexSmudgeLog :: Git.Repo -> RawFilePath
gitAnnexSmudgeLog r = gitAnnexDir r P.</> "smudge.log"

gitAnnexSmudgeLock :: Git.Repo -> RawFilePath
gitAnnexSmudgeLock r = gitAnnexDir r P.</> "smudge.lck"

{- .git/annex/move.log is used to log moves that are in progress,
 - to better support resuming an interrupted move. -}
gitAnnexMoveLog :: Git.Repo -> RawFilePath
gitAnnexMoveLog r = gitAnnexDir r P.</> "move.log"

gitAnnexMoveLock :: Git.Repo -> RawFilePath
gitAnnexMoveLock r = gitAnnexDir r P.</> "move.lck"

{- .git/annex/export/ is used to store information about
 - exports to special remotes. -}
gitAnnexExportDir :: Git.Repo -> RawFilePath
gitAnnexExportDir r = gitAnnexDir r P.</> "export"

{- Directory containing database used to record export info. -}
gitAnnexExportDbDir :: UUID -> Git.Repo -> RawFilePath
gitAnnexExportDbDir u r = gitAnnexExportDir r P.</> fromUUID u P.</> "exportdb"

{- Lock file for export state for a special remote. -}
gitAnnexExportLock :: UUID -> Git.Repo -> RawFilePath
gitAnnexExportLock u r = gitAnnexExportDbDir u r <> ".lck"

{- Lock file for updating the export state for a special remote. -}
gitAnnexExportUpdateLock :: UUID -> Git.Repo -> RawFilePath
gitAnnexExportUpdateLock u r = gitAnnexExportDbDir u r <> ".upl"

{- Log file used to keep track of files that were in the tree exported to a
 - remote, but were excluded by its preferred content settings. -}
gitAnnexExportExcludeLog :: UUID -> Git.Repo -> RawFilePath
gitAnnexExportExcludeLog u r = gitAnnexDir r P.</> "export.ex" P.</> fromUUID u

{- Directory containing database used to record remote content ids.
 -
 - (This used to be "cid", but a problem with the database caused it to
 - need to be rebuilt with a new name.)
 -}
gitAnnexContentIdentifierDbDir :: Git.Repo -> RawFilePath
gitAnnexContentIdentifierDbDir r = gitAnnexDir r P.</> "cidsdb"

{- Lock file for writing to the content id database. -}
gitAnnexContentIdentifierLock :: Git.Repo -> RawFilePath
gitAnnexContentIdentifierLock r = gitAnnexContentIdentifierDbDir r <> ".lck"

{- .git/annex/schedulestate is used to store information about when
 - scheduled jobs were last run. -}
gitAnnexScheduleState :: Git.Repo -> RawFilePath
gitAnnexScheduleState r = gitAnnexDir r P.</> "schedulestate"

{- .git/annex/creds/ is used to store credentials to access some special
 - remotes. -}
gitAnnexCredsDir :: Git.Repo -> RawFilePath
gitAnnexCredsDir r = P.addTrailingPathSeparator $ gitAnnexDir r P.</> "creds"

{- .git/annex/certificate.pem and .git/annex/key.pem are used by the webapp
 - when HTTPS is enabled -}
gitAnnexWebCertificate :: Git.Repo -> FilePath
gitAnnexWebCertificate r = fromRawFilePath $ gitAnnexDir r P.</> "certificate.pem"
gitAnnexWebPrivKey :: Git.Repo -> FilePath
gitAnnexWebPrivKey r = fromRawFilePath $ gitAnnexDir r P.</> "privkey.pem"

{- .git/annex/feeds/ is used to record per-key (url) state by importfeeds -}
gitAnnexFeedStateDir :: Git.Repo -> RawFilePath
gitAnnexFeedStateDir r = P.addTrailingPathSeparator $
	gitAnnexDir r P.</> "feedstate"

gitAnnexFeedState :: Key -> Git.Repo -> RawFilePath
gitAnnexFeedState k r = gitAnnexFeedStateDir r P.</> keyFile k

{- .git/annex/merge/ is used as a empty work tree for merges in 
 - adjusted branches. -}
gitAnnexMergeDir :: Git.Repo -> FilePath
gitAnnexMergeDir r = fromRawFilePath $
	P.addTrailingPathSeparator $ gitAnnexDir r P.</> "merge"

{- .git/annex/transfer/ is used to record keys currently
 - being transferred, and other transfer bookkeeping info. -}
gitAnnexTransferDir :: Git.Repo -> RawFilePath
gitAnnexTransferDir r =
	P.addTrailingPathSeparator $ gitAnnexDir r P.</> "transfer"

{- .git/annex/journal/ is used to journal changes made to the git-annex
 - branch -}
gitAnnexJournalDir :: Git.Repo -> RawFilePath
gitAnnexJournalDir r = 
	P.addTrailingPathSeparator $ gitAnnexDir r P.</> "journal"

gitAnnexJournalDir' :: Git.Repo -> RawFilePath
gitAnnexJournalDir' r =
	P.addTrailingPathSeparator $ gitAnnexDir r P.</> "journal"

{- Lock file for the journal. -}
gitAnnexJournalLock :: Git.Repo -> RawFilePath
gitAnnexJournalLock r = gitAnnexDir r P.</> "journal.lck"

{- Lock file for flushing a git queue that writes to the git index or
 - other git state that should only have one writer at a time. -}
gitAnnexGitQueueLock :: Git.Repo -> RawFilePath
gitAnnexGitQueueLock r = gitAnnexDir r P.</> "gitqueue.lck"

{- .git/annex/index is used to stage changes to the git-annex branch -}
gitAnnexIndex :: Git.Repo -> RawFilePath
gitAnnexIndex r = gitAnnexDir r P.</> "index"

{- Holds the ref of the git-annex branch that the index was last updated to.
 -
 - The .lck in the name is a historical accident; this is not used as a
 - lock. -}
gitAnnexIndexStatus :: Git.Repo -> RawFilePath
gitAnnexIndexStatus r = gitAnnexDir r P.</> "index.lck"

{- The index file used to generate a filtered branch view._-}
gitAnnexViewIndex :: Git.Repo -> RawFilePath
gitAnnexViewIndex r = gitAnnexDir r P.</> "viewindex"

{- File containing a log of recently accessed views. -}
gitAnnexViewLog :: Git.Repo -> RawFilePath
gitAnnexViewLog r = gitAnnexDir r P.</> "viewlog"

{- List of refs that have already been merged into the git-annex branch. -}
gitAnnexMergedRefs :: Git.Repo -> RawFilePath
gitAnnexMergedRefs r = gitAnnexDir r P.</> "mergedrefs"

{- List of refs that should not be merged into the git-annex branch. -}
gitAnnexIgnoredRefs :: Git.Repo -> RawFilePath
gitAnnexIgnoredRefs r = gitAnnexDir r P.</> "ignoredrefs"

{- Pid file for daemon mode. -}
gitAnnexPidFile :: Git.Repo -> RawFilePath
gitAnnexPidFile r = gitAnnexDir r P.</> "daemon.pid"

{- Pid lock file for pidlock mode -}
gitAnnexPidLockFile :: Git.Repo -> RawFilePath
gitAnnexPidLockFile r = gitAnnexDir r P.</> "pidlock"

{- Status file for daemon mode. -}
gitAnnexDaemonStatusFile :: Git.Repo -> FilePath
gitAnnexDaemonStatusFile r = fromRawFilePath $
	gitAnnexDir r P.</> "daemon.status"

{- Log file for daemon mode. -}
gitAnnexDaemonLogFile :: Git.Repo -> RawFilePath
gitAnnexDaemonLogFile r = gitAnnexDir r P.</> "daemon.log"

{- Log file for fuzz test. -}
gitAnnexFuzzTestLogFile :: Git.Repo -> FilePath
gitAnnexFuzzTestLogFile r = fromRawFilePath $
	gitAnnexDir r P.</> "fuzztest.log"

{- Html shim file used to launch the webapp. -}
gitAnnexHtmlShim :: Git.Repo -> RawFilePath
gitAnnexHtmlShim r = gitAnnexDir r P.</> "webapp.html"

{- File containing the url to the webapp. -}
gitAnnexUrlFile :: Git.Repo -> RawFilePath
gitAnnexUrlFile r = gitAnnexDir r P.</> "url"

{- Temporary file used to edit configuriation from the git-annex branch. -}
gitAnnexTmpCfgFile :: Git.Repo -> RawFilePath
gitAnnexTmpCfgFile r = gitAnnexDir r P.</> "config.tmp"

{- .git/annex/ssh/ is used for ssh connection caching -}
gitAnnexSshDir :: Git.Repo -> RawFilePath
gitAnnexSshDir r = P.addTrailingPathSeparator $ gitAnnexDir r P.</> "ssh"

{- .git/annex/remotes/ is used for remote-specific state. -}
gitAnnexRemotesDir :: Git.Repo -> RawFilePath
gitAnnexRemotesDir r =
	P.addTrailingPathSeparator $ gitAnnexDir r P.</> "remotes"

{- This is the base directory name used by the assistant when making
 - repositories, by default. -}
gitAnnexAssistantDefaultDir :: FilePath
gitAnnexAssistantDefaultDir = "annex"

{- Sanitizes a String that will be used as part of a Key's keyName,
 - dealing with characters that cause problems.
 -
 - This is used when a new Key is initially being generated, eg by genKey.
 - Unlike keyFile and fileKey, it does not need to be a reversable
 - escaping. Also, it's ok to change this to add more problematic
 - characters later. Unlike changing keyFile, which could result in the
 - filenames used for existing keys changing and contents getting lost.
 -
 - It is, however, important that the input and output of this function
 - have a 1:1 mapping, to avoid two different inputs from mapping to the
 - same key.
 -}
preSanitizeKeyName :: String -> String
preSanitizeKeyName = preSanitizeKeyName' False

preSanitizeKeyName' :: Bool -> String -> String
preSanitizeKeyName' resanitize = concatMap escape
  where
	escape c
		| isAsciiUpper c || isAsciiLower c || isDigit c = [c]
		| c `elem` ['.', '-', '_'] = [c] -- common, assumed safe
		| c `elem` ['/', '%', ':'] = [c] -- handled by keyFile
		-- , is safe and uncommon, so will be used to escape
		-- other characters. By itself, it is escaped to 
		-- doubled form.
		| c == ',' = if not resanitize
			then ",,"
			else ","
		| otherwise = ',' : show (ord c)

{- Converts a keyName that has been santizied with an old version of
 - preSanitizeKeyName to be sanitized with the new version. -}
reSanitizeKeyName :: String -> String
reSanitizeKeyName = preSanitizeKeyName' True

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
keyFile :: Key -> RawFilePath
keyFile k = 
	let b = serializeKey' k
	in if S8.any (`elem` ['&', '%', ':', '/']) b
		then S8.concatMap esc b
		else b
  where
	esc '&' = "&a"
	esc '%' = "&s"
	esc ':' = "&c"
	esc '/' = "%"
	esc c = S8.singleton c

{- Reverses keyFile, converting a filename fragment (ie, the basename of
 - the symlink target) into a key. -}
fileKey :: RawFilePath -> Maybe Key
fileKey = deserializeKey' . S8.intercalate "/" . map go . S8.split '%'
  where
	go = S8.concat . unescafterfirst . S8.split '&'
	unescafterfirst [] = []
	unescafterfirst (b:bs) = b : map (unesc . S8.uncons) bs
	unesc :: Maybe (Char, S8.ByteString) -> S8.ByteString
	unesc Nothing = mempty
	unesc (Just ('c', b)) = S8.cons ':' b
	unesc (Just ('s', b)) = S8.cons '%' b
	unesc (Just ('a', b)) = S8.cons '&' b
	unesc (Just (c, b)) = S8.cons c b

{- A location to store a key on a special remote that uses a filesystem.
 - A directory hash is used, to protect against filesystems that dislike
 - having many items in a single directory.
 -
 - The file is put in a directory with the same name, this allows
 - write-protecting the directory to avoid accidental deletion of the file.
 -}
keyPath :: Key -> Hasher -> RawFilePath
keyPath key hasher = hasher key P.</> f P.</> f
  where
	f = keyFile key

{- All possibile locations to store a key in a special remote
 - using different directory hashes.
 -
 - This is compatible with the annexLocations, for interoperability between
 - special remotes and git-annex repos.
 -}
keyPaths :: Key -> [RawFilePath]
keyPaths key = map (\h -> keyPath key (h def)) dirHashes
