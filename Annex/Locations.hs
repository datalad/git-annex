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
	gitAnnexMapping,
	gitAnnexInodeCache,
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
	gitAnnexMergeLock,
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
import Utility.Path.AbsRel
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
 -
 - This does not take direct mode into account, so in direct mode it is not
 - the actual location of the file's content.
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
gitAnnexContentLock :: Key -> Git.Repo -> GitConfig -> IO FilePath
gitAnnexContentLock key r config = do
	loc <- gitAnnexLocation key r config
	return $ fromRawFilePath loc ++ ".lck"

{- File that maps from a key to the file(s) in the git repository.
 - Used in direct mode. -}
gitAnnexMapping :: Key -> Git.Repo -> GitConfig -> IO FilePath
gitAnnexMapping key r config = do
	loc <- gitAnnexLocation key r config
	return $ fromRawFilePath loc ++ ".map"

{- File that caches information about a key's content, used to determine
 - if a file has changed.
 - Used in direct mode. -}
gitAnnexInodeCache :: Key -> Git.Repo -> GitConfig -> IO FilePath
gitAnnexInodeCache key r config = do
	loc <- gitAnnexLocation key r config
	return $ fromRawFilePath loc ++ ".cache"

gitAnnexInodeSentinal :: Git.Repo -> RawFilePath
gitAnnexInodeSentinal r = gitAnnexDir r P.</> "sentinal"

gitAnnexInodeSentinalCache :: Git.Repo -> RawFilePath
gitAnnexInodeSentinalCache r = gitAnnexInodeSentinal r <> ".cache"

{- The annex directory of a repository. -}
gitAnnexDir :: Git.Repo -> RawFilePath
gitAnnexDir r = P.addTrailingPathSeparator $ Git.localGitDir r P.</> annexDir

{- The part of the annex directory where file contents are stored. -}
gitAnnexObjectDir :: Git.Repo -> FilePath
gitAnnexObjectDir r = fromRawFilePath $ 
	P.addTrailingPathSeparator $ Git.localGitDir r P.</> objectDir'

{- .git/annex/tmp/ is used for temp files for key's contents -}
gitAnnexTmpObjectDir :: Git.Repo -> FilePath
gitAnnexTmpObjectDir = fromRawFilePath . gitAnnexTmpObjectDir'

gitAnnexTmpObjectDir' :: Git.Repo -> RawFilePath
gitAnnexTmpObjectDir' r = P.addTrailingPathSeparator $ gitAnnexDir r P.</> "tmp"

{- .git/annex/othertmp/ is used for other temp files -}
gitAnnexTmpOtherDir :: Git.Repo -> FilePath
gitAnnexTmpOtherDir r = fromRawFilePath $
	P.addTrailingPathSeparator $ gitAnnexDir r P.</> "othertmp"

{- Lock file for gitAnnexTmpOtherDir. -}
gitAnnexTmpOtherLock :: Git.Repo -> FilePath
gitAnnexTmpOtherLock r = fromRawFilePath $ gitAnnexDir r P.</> "othertmp.lck"

{- .git/annex/misctmp/ was used by old versions of git-annex and is still
 - used during initialization -}
gitAnnexTmpOtherDirOld :: Git.Repo -> FilePath
gitAnnexTmpOtherDirOld r = fromRawFilePath $ 
	P.addTrailingPathSeparator $ gitAnnexDir r P.</> "misctmp"

{- .git/annex/watchtmp/ is used by the watcher and assistant -}
gitAnnexTmpWatcherDir :: Git.Repo -> FilePath
gitAnnexTmpWatcherDir r = fromRawFilePath $
	P.addTrailingPathSeparator $ gitAnnexDir r P.</> "watchtmp"

{- The temp file to use for a given key's content. -}
gitAnnexTmpObjectLocation :: Key -> Git.Repo -> FilePath
gitAnnexTmpObjectLocation key r = fromRawFilePath $
	gitAnnexTmpObjectDir' r P.</> keyFile key

{- Given a temp file such as gitAnnexTmpObjectLocation, makes a name for a
 - subdirectory in the same location, that can be used as a work area
 - when receiving the key's content.
 -
 - There are ordering requirements for creating these directories;
 - use Annex.Content.withTmpWorkDir to set them up.
 -}
gitAnnexTmpWorkDir :: FilePath -> FilePath
gitAnnexTmpWorkDir p =
	let (dir, f) = splitFileName p
	-- Using a prefix avoids name conflict with any other keys.
	in dir </> "work." ++ f

{- .git/annex/bad/ is used for bad files found during fsck -}
gitAnnexBadDir :: Git.Repo -> FilePath
gitAnnexBadDir r = fromRawFilePath $
	P.addTrailingPathSeparator $ gitAnnexDir r P.</> "bad"

{- The bad file to use for a given key. -}
gitAnnexBadLocation :: Key -> Git.Repo -> FilePath
gitAnnexBadLocation key r = gitAnnexBadDir r </> fromRawFilePath (keyFile key)

{- .git/annex/foounused is used to number possibly unused keys -}
gitAnnexUnusedLog :: FilePath -> Git.Repo -> FilePath
gitAnnexUnusedLog prefix r =
	fromRawFilePath (gitAnnexDir r) </> (prefix ++ "unused")

{- .git/annex/keysdb/ contains a database of information about keys. -}
gitAnnexKeysDb :: Git.Repo -> FilePath
gitAnnexKeysDb r = fromRawFilePath $ gitAnnexDir r P.</> "keysdb"

{- Lock file for the keys database. -}
gitAnnexKeysDbLock :: Git.Repo -> FilePath
gitAnnexKeysDbLock r = gitAnnexKeysDb r ++ ".lck"

{- Contains the stat of the last index file that was
 - reconciled with the keys database. -}
gitAnnexKeysDbIndexCache :: Git.Repo -> FilePath
gitAnnexKeysDbIndexCache r = gitAnnexKeysDb r ++ ".cache"

{- .git/annex/fsck/uuid/ is used to store information about incremental
 - fscks. -}
gitAnnexFsckDir :: UUID -> Git.Repo -> FilePath
gitAnnexFsckDir u r = fromRawFilePath $
	gitAnnexDir r P.</> "fsck" P.</> fromUUID u

{- used to store information about incremental fscks. -}
gitAnnexFsckState :: UUID -> Git.Repo -> FilePath
gitAnnexFsckState u r = gitAnnexFsckDir u r </> "state"

{- Directory containing database used to record fsck info. -}
gitAnnexFsckDbDir :: UUID -> Git.Repo -> FilePath
gitAnnexFsckDbDir u r = gitAnnexFsckDir u r </> "fsckdb"

{- Directory containing old database used to record fsck info. -}
gitAnnexFsckDbDirOld :: UUID -> Git.Repo -> FilePath
gitAnnexFsckDbDirOld u r = gitAnnexFsckDir u r </> "db"

{- Lock file for the fsck database. -}
gitAnnexFsckDbLock :: UUID -> Git.Repo -> FilePath
gitAnnexFsckDbLock u r = gitAnnexFsckDir u r </> "fsck.lck"

{- .git/annex/fsckresults/uuid is used to store results of git fscks -}
gitAnnexFsckResultsLog :: UUID -> Git.Repo -> FilePath
gitAnnexFsckResultsLog u r = fromRawFilePath $ 
	gitAnnexDir r P.</> "fsckresults" P.</> fromUUID u

{- .git/annex/smudge.log is used to log smudges worktree files that need to
 - be updated. -}
gitAnnexSmudgeLog :: Git.Repo -> FilePath
gitAnnexSmudgeLog r = fromRawFilePath $ gitAnnexDir r P.</> "smudge.log"

gitAnnexSmudgeLock :: Git.Repo -> FilePath
gitAnnexSmudgeLock r = fromRawFilePath $ gitAnnexDir r P.</> "smudge.lck"

{- .git/annex/move.log is used to log moves that are in progress,
 - to better support resuming an interrupted move. -}
gitAnnexMoveLog :: Git.Repo -> FilePath
gitAnnexMoveLog r = fromRawFilePath $ gitAnnexDir r P.</> "move.log"

gitAnnexMoveLock :: Git.Repo -> FilePath
gitAnnexMoveLock r = fromRawFilePath $ gitAnnexDir r P.</> "move.lck"

{- .git/annex/export/ is used to store information about
 - exports to special remotes. -}
gitAnnexExportDir :: Git.Repo -> FilePath
gitAnnexExportDir r = fromRawFilePath $ gitAnnexDir r P.</> "export"

{- Directory containing database used to record export info. -}
gitAnnexExportDbDir :: UUID -> Git.Repo -> FilePath
gitAnnexExportDbDir u r = gitAnnexExportDir r </> fromUUID u </> "exportdb"

{- Lock file for export state for a special remote. -}
gitAnnexExportLock :: UUID -> Git.Repo -> FilePath
gitAnnexExportLock u r = gitAnnexExportDbDir u r ++ ".lck"

{- Lock file for updating the export state for a special remote. -}
gitAnnexExportUpdateLock :: UUID -> Git.Repo -> FilePath
gitAnnexExportUpdateLock u r = gitAnnexExportDbDir u r ++ ".upl"

{- Log file used to keep track of files that were in the tree exported to a
 - remote, but were excluded by its preferred content settings. -}
gitAnnexExportExcludeLog :: UUID -> Git.Repo -> FilePath
gitAnnexExportExcludeLog u r = fromRawFilePath $
	gitAnnexDir r P.</> "export.ex" P.</> fromUUID u

{- Directory containing database used to record remote content ids.
 -
 - (This used to be "cid", but a problem with the database caused it to
 - need to be rebuilt with a new name.)
 -}
gitAnnexContentIdentifierDbDir :: Git.Repo -> FilePath
gitAnnexContentIdentifierDbDir r = fromRawFilePath $ gitAnnexDir r P.</> "cidsdb"

{- Lock file for writing to the content id database. -}
gitAnnexContentIdentifierLock :: Git.Repo -> FilePath
gitAnnexContentIdentifierLock r = gitAnnexContentIdentifierDbDir r ++ ".lck"

{- .git/annex/schedulestate is used to store information about when
 - scheduled jobs were last run. -}
gitAnnexScheduleState :: Git.Repo -> FilePath
gitAnnexScheduleState r = fromRawFilePath $ gitAnnexDir r P.</> "schedulestate"

{- .git/annex/creds/ is used to store credentials to access some special
 - remotes. -}
gitAnnexCredsDir :: Git.Repo -> FilePath
gitAnnexCredsDir r = fromRawFilePath $
	P.addTrailingPathSeparator $ gitAnnexDir r P.</> "creds"

{- .git/annex/certificate.pem and .git/annex/key.pem are used by the webapp
 - when HTTPS is enabled -}
gitAnnexWebCertificate :: Git.Repo -> FilePath
gitAnnexWebCertificate r = fromRawFilePath $ gitAnnexDir r P.</> "certificate.pem"
gitAnnexWebPrivKey :: Git.Repo -> FilePath
gitAnnexWebPrivKey r = fromRawFilePath $ gitAnnexDir r P.</> "privkey.pem"

{- .git/annex/feeds/ is used to record per-key (url) state by importfeeds -}
gitAnnexFeedStateDir :: Git.Repo -> FilePath
gitAnnexFeedStateDir r = fromRawFilePath $
	P.addTrailingPathSeparator $ gitAnnexDir r P.</> "feedstate"

gitAnnexFeedState :: Key -> Git.Repo -> FilePath
gitAnnexFeedState k r = gitAnnexFeedStateDir r </> fromRawFilePath (keyFile k)

{- .git/annex/merge/ is used as a empty work tree for direct mode merges and
 - merges in adjusted branches. -}
gitAnnexMergeDir :: Git.Repo -> FilePath
gitAnnexMergeDir r = fromRawFilePath $
	P.addTrailingPathSeparator $ gitAnnexDir r P.</> "merge"

{- .git/annex/transfer/ is used to record keys currently
 - being transferred, and other transfer bookkeeping info. -}
gitAnnexTransferDir :: Git.Repo -> FilePath
gitAnnexTransferDir r = fromRawFilePath $
	P.addTrailingPathSeparator $ gitAnnexDir r P.</> "transfer"

{- .git/annex/journal/ is used to journal changes made to the git-annex
 - branch -}
gitAnnexJournalDir :: Git.Repo -> FilePath
gitAnnexJournalDir r = fromRawFilePath $
	P.addTrailingPathSeparator $ gitAnnexDir r P.</> "journal"

gitAnnexJournalDir' :: Git.Repo -> RawFilePath
gitAnnexJournalDir' r = P.addTrailingPathSeparator $ gitAnnexDir r P.</> "journal"

{- Lock file for the journal. -}
gitAnnexJournalLock :: Git.Repo -> FilePath
gitAnnexJournalLock r = fromRawFilePath $ gitAnnexDir r P.</> "journal.lck"

{- Lock file for flushing a git queue that writes to the git index or
 - other git state that should only have one writer at a time. -}
gitAnnexGitQueueLock :: Git.Repo -> FilePath
gitAnnexGitQueueLock r = fromRawFilePath $ gitAnnexDir r P.</> "gitqueue.lck"

{- Lock file for direct mode merge. -}
gitAnnexMergeLock :: Git.Repo -> FilePath
gitAnnexMergeLock r = fromRawFilePath $ gitAnnexDir r P.</> "merge.lck"

{- .git/annex/index is used to stage changes to the git-annex branch -}
gitAnnexIndex :: Git.Repo -> FilePath
gitAnnexIndex r = fromRawFilePath $ gitAnnexDir r P.</> "index"

{- Holds the ref of the git-annex branch that the index was last updated to.
 -
 - The .lck in the name is a historical accident; this is not used as a
 - lock. -}
gitAnnexIndexStatus :: Git.Repo -> FilePath
gitAnnexIndexStatus r = fromRawFilePath $ gitAnnexDir r P.</> "index.lck"

{- The index file used to generate a filtered branch view._-}
gitAnnexViewIndex :: Git.Repo -> FilePath
gitAnnexViewIndex r = fromRawFilePath $ gitAnnexDir r P.</> "viewindex"

{- File containing a log of recently accessed views. -}
gitAnnexViewLog :: Git.Repo -> FilePath
gitAnnexViewLog r = fromRawFilePath $ gitAnnexDir r P.</> "viewlog"

{- List of refs that have already been merged into the git-annex branch. -}
gitAnnexMergedRefs :: Git.Repo -> FilePath
gitAnnexMergedRefs r = fromRawFilePath $ gitAnnexDir r P.</> "mergedrefs"

{- List of refs that should not be merged into the git-annex branch. -}
gitAnnexIgnoredRefs :: Git.Repo -> FilePath
gitAnnexIgnoredRefs r = fromRawFilePath $ gitAnnexDir r P.</> "ignoredrefs"

{- Pid file for daemon mode. -}
gitAnnexPidFile :: Git.Repo -> FilePath
gitAnnexPidFile r = fromRawFilePath $ gitAnnexDir r P.</> "daemon.pid"

{- Pid lock file for pidlock mode -}
gitAnnexPidLockFile :: Git.Repo -> FilePath
gitAnnexPidLockFile r = fromRawFilePath $ gitAnnexDir r P.</> "pidlock"

{- Status file for daemon mode. -}
gitAnnexDaemonStatusFile :: Git.Repo -> FilePath
gitAnnexDaemonStatusFile r = fromRawFilePath $
	gitAnnexDir r P.</> "daemon.status"

{- Log file for daemon mode. -}
gitAnnexDaemonLogFile :: Git.Repo -> FilePath
gitAnnexDaemonLogFile r = fromRawFilePath $ gitAnnexDir r P.</> "daemon.log"

{- Log file for fuzz test. -}
gitAnnexFuzzTestLogFile :: Git.Repo -> FilePath
gitAnnexFuzzTestLogFile r = fromRawFilePath $
	gitAnnexDir r P.</> "fuzztest.log"

{- Html shim file used to launch the webapp. -}
gitAnnexHtmlShim :: Git.Repo -> FilePath
gitAnnexHtmlShim r = fromRawFilePath $ gitAnnexDir r P.</> "webapp.html"

{- File containing the url to the webapp. -}
gitAnnexUrlFile :: Git.Repo -> FilePath
gitAnnexUrlFile r = fromRawFilePath $ gitAnnexDir r P.</> "url"

{- Temporary file used to edit configuriation from the git-annex branch. -}
gitAnnexTmpCfgFile :: Git.Repo -> FilePath
gitAnnexTmpCfgFile r = fromRawFilePath $ gitAnnexDir r P.</> "config.tmp"

{- .git/annex/ssh/ is used for ssh connection caching -}
gitAnnexSshDir :: Git.Repo -> FilePath
gitAnnexSshDir r = fromRawFilePath $
	P.addTrailingPathSeparator $ gitAnnexDir r P.</> "ssh"

{- .git/annex/remotes/ is used for remote-specific state. -}
gitAnnexRemotesDir :: Git.Repo -> FilePath
gitAnnexRemotesDir r = fromRawFilePath $
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
