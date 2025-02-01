{- git-annex file locations
 -
 - Copyright 2010-2024 Joey Hess <id@joeyh.name>
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
	gitAnnexLocation,
	gitAnnexLocation',
	gitAnnexLocationDepth,
	gitAnnexLink,
	gitAnnexLinkCanonical,
	gitAnnexContentLock,
	gitAnnexContentRetentionTimestamp,
	gitAnnexContentRetentionTimestampLock,
	gitAnnexContentLockLock,
	gitAnnexInodeSentinal,
	gitAnnexInodeSentinalCache,
	annexLocationsBare,
	annexLocationsNonBare,
	annexLocation,
	exportAnnexObjectLocation,
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
	gitAnnexKeysDbDir,
	gitAnnexKeysDbLock,
	gitAnnexKeysDbIndexCache,
	gitAnnexFsckState,
	gitAnnexFsckDbDir,
	gitAnnexFsckDbDirOld,
	gitAnnexFsckDbLock,
	gitAnnexFsckResultsLog,
	gitAnnexUpgradeLog,
	gitAnnexUpgradeLock,
	gitAnnexSmudgeLog,
	gitAnnexSmudgeLock,
	gitAnnexRestageLog,
	gitAnnexRestageLogOld,
	gitAnnexRestageLock,
	gitAnnexAdjustedBranchUpdateLog,
	gitAnnexAdjustedBranchUpdateLock,
	gitAnnexMigrateLog,
	gitAnnexMigrateLock,
	gitAnnexMigrationsLog,
	gitAnnexMigrationsLock,
	gitAnnexMoveLog,
	gitAnnexMoveLock,
	gitAnnexExportDir,
	gitAnnexExportDbDir,
	gitAnnexExportLock,
	gitAnnexExportUpdateLock,
	gitAnnexExportExcludeLog,
	gitAnnexImportDir,
	gitAnnexImportLog,
	gitAnnexContentIdentifierDbDir,
	gitAnnexContentIdentifierLock,
	gitAnnexImportFeedDbDir,
	gitAnnexImportFeedDbLock,
	gitAnnexRepoSizeDbDir,
	gitAnnexRepoSizeDbLock,
	gitAnnexRepoSizeLiveDir,
	gitAnnexScheduleState,
	gitAnnexTransferDir,
	gitAnnexCredsDir,
	gitAnnexWebCertificate,
	gitAnnexWebPrivKey,
	gitAnnexFeedStateDir,
	gitAnnexFeedState,
	gitAnnexMergeDir,
	gitAnnexJournalDir,
	gitAnnexPrivateJournalDir,
	gitAnnexJournalLock,
	gitAnnexGitQueueLock,
	gitAnnexIndex,
	gitAnnexPrivateIndex,
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
	gitAnnexSimDir,
	HashLevels(..),
	hashDirMixed,
	hashDirLower,
	preSanitizeKeyName,
	reSanitizeKeyName,
) where

import Data.Char
import Data.Default
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Short as SB

import Common
import Key
import Types.UUID
import Types.GitConfig
import Types.Difference
import Types.BranchState
import Types.Export
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
 - Everything else should not end in a trailing path separator. 
 -
 - Only functions (with names starting with "git") that build a path
 - based on a git repository should return full path relative to the git
 - repository. Everything else returns path segments.
 -}

{- The directory git annex uses for local state, relative to the .git
 - directory -}
annexDir :: OsPath
annexDir = addTrailingPathSeparator (literalOsPath "annex")

{- The directory git annex uses for locally available object content,
 - relative to the .git directory -}
objectDir :: OsPath
objectDir = addTrailingPathSeparator $ annexDir </> literalOsPath "objects"

{- Annexed file's possible locations relative to the .git directory
 - in a non-bare eepository.
 - 
 - Normally it is hashDirMixed. However, it's always possible that a
 - bare repository was converted to non-bare, or that the cripped
 - filesystem setting changed, so still need to check both. -}
annexLocationsNonBare :: GitConfig -> Key -> [OsPath]
annexLocationsNonBare config key = 
	map (annexLocation config key) [hashDirMixed, hashDirLower]

{- Annexed file's possible locations relative to a bare repository. -}
annexLocationsBare :: GitConfig -> Key -> [OsPath]
annexLocationsBare config key = 
	map (annexLocation config key) [hashDirLower, hashDirMixed]

annexLocation :: GitConfig -> Key -> (HashLevels -> Hasher) -> OsPath
annexLocation config key hasher = objectDir </> keyPath key (hasher $ objectHashLevels config)

{- For exportree remotes with annexobjects=true, objects are stored
 - in this location as well as in the exported tree. -}
exportAnnexObjectLocation :: GitConfig -> Key -> ExportLocation
exportAnnexObjectLocation gc k =
	mkExportLocation $
		literalOsPath ".git" </> annexLocation gc k hashDirLower

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
gitAnnexLocation :: Key -> Git.Repo -> GitConfig -> IO OsPath
gitAnnexLocation = gitAnnexLocation' doesPathExist

gitAnnexLocation' :: (OsPath -> IO Bool) -> Key -> Git.Repo -> GitConfig -> IO OsPath
gitAnnexLocation' checker key r config = gitAnnexLocation'' key r config
	(annexCrippledFileSystem config)
	(coreSymlinks config)
	checker
	(Git.localGitDir r)

gitAnnexLocation'' :: Key -> Git.Repo -> GitConfig -> Bool -> Bool -> (OsPath -> IO Bool) -> OsPath -> IO OsPath
gitAnnexLocation'' key r config crippled symlinkssupported checker gitdir
	{- Bare repositories default to hashDirLower for new
	 - content, as it's more portable. But check all locations. -}
	| Git.repoIsLocalBare r = checkall annexLocationsBare
	{- If the repository is configured to only use lower, no need
	 - to check both. -}
	| hasDifference ObjectHashLower (annexDifferences config) = 
		only hashDirLower
	{- Repositories on crippled filesystems use same layout as bare
	 - repos for new content, unless symlinks are supported too. -}
	| crippled = if symlinkssupported
		then checkall annexLocationsNonBare
		else checkall annexLocationsBare
	| otherwise = checkall annexLocationsNonBare
  where
	only = return . inrepo . annexLocation config key
	checkall f = check $ map inrepo $ f config key

	inrepo d = gitdir </> d
	check locs@(l:_) = fromMaybe l <$> firstM checker locs
	check [] = error "internal"

{- Calculates a symlink target to link a file to an annexed object. -}
gitAnnexLink :: OsPath -> Key -> Git.Repo -> GitConfig -> IO OsPath
gitAnnexLink file key r config = do
	currdir <- getCurrentDirectory
	let absfile = absNormPathUnix currdir file
	let gitdir = getgitdir currdir
	loc <- gitAnnexLocation'' key r config False False (\_ -> return True) gitdir
	toInternalGitPath <$> relPathDirToFile (parentDir absfile) loc
  where
	getgitdir currdir
		{- This special case is for git submodules on filesystems not
		 - supporting symlinks; generate link target that will
		 - work portably. -}
		| not (coreSymlinks config) && needsSubmoduleFixup r =
			absNormPathUnix currdir (Git.repoPath r </> literalOsPath ".git")
		| otherwise = Git.localGitDir r
	absNormPathUnix d p = toInternalGitPath $
		absPathFrom (toInternalGitPath d) (toInternalGitPath p)

{- Calculates a symlink target as would be used in a typical git
 - repository, with .git in the top of the work tree. -}
gitAnnexLinkCanonical :: OsPath -> Key -> Git.Repo -> GitConfig -> IO OsPath
gitAnnexLinkCanonical file key r config = gitAnnexLink file key r' config'
  where
	r' = case r of
		Git.Repo { Git.location = l@Git.Local { Git.worktree = Just wt } } ->
			r { Git.location = l { Git.gitdir = wt </> literalOsPath ".git" } }
		_ -> r
	config' = config
		{ annexCrippledFileSystem = False
		, coreSymlinks = True
		}

{- File used to lock a key's content. -}
gitAnnexContentLock :: Key -> Git.Repo -> GitConfig -> IO OsPath
gitAnnexContentLock key r config = do
	loc <- gitAnnexLocation key r config
	return $ loc <> literalOsPath ".lck"

{- File used to indicate a key's content should not be dropped until after
 - a specified time. -}
gitAnnexContentRetentionTimestamp :: Key -> Git.Repo -> GitConfig -> IO OsPath
gitAnnexContentRetentionTimestamp key r config = do
	loc <- gitAnnexLocation key r config
	return $ loc <> literalOsPath ".rtm"

{- Lock file for gitAnnexContentRetentionTimestamp -}
gitAnnexContentRetentionTimestampLock :: Key -> Git.Repo -> GitConfig -> IO OsPath
gitAnnexContentRetentionTimestampLock key r config = do
	loc <- gitAnnexLocation key r config
	return $ loc <> literalOsPath ".rtl"

{- Lock that is held when taking the gitAnnexContentLock to support the v10
 - upgrade.
 -
 - This uses the gitAnnexInodeSentinal file, because it needs to be a file
 - that exists in the repository, even when it's an old v8 repository that
 - is mounted read-only. The gitAnnexInodeSentinal is created by git-annex
 - init, so should already exist.
 -}
gitAnnexContentLockLock :: Git.Repo -> OsPath
gitAnnexContentLockLock = gitAnnexInodeSentinal

gitAnnexInodeSentinal :: Git.Repo -> OsPath
gitAnnexInodeSentinal r = gitAnnexDir r </> literalOsPath "sentinal"

gitAnnexInodeSentinalCache :: Git.Repo -> OsPath
gitAnnexInodeSentinalCache r = gitAnnexInodeSentinal r <> literalOsPath ".cache"

{- The annex directory of a repository. -}
gitAnnexDir :: Git.Repo -> OsPath
gitAnnexDir r = addTrailingPathSeparator $ Git.localGitDir r </> annexDir

{- The part of the annex directory where file contents are stored. -}
gitAnnexObjectDir :: Git.Repo -> OsPath
gitAnnexObjectDir r = addTrailingPathSeparator $
	Git.localGitDir r </> objectDir

{- .git/annex/tmp/ is used for temp files for key's contents -}
gitAnnexTmpObjectDir :: Git.Repo -> OsPath
gitAnnexTmpObjectDir r = addTrailingPathSeparator $
	gitAnnexDir r </> literalOsPath "tmp"

{- .git/annex/othertmp/ is used for other temp files -}
gitAnnexTmpOtherDir :: Git.Repo -> OsPath
gitAnnexTmpOtherDir r = addTrailingPathSeparator $
	gitAnnexDir r </> literalOsPath "othertmp"

{- Lock file for gitAnnexTmpOtherDir. -}
gitAnnexTmpOtherLock :: Git.Repo -> OsPath
gitAnnexTmpOtherLock r = gitAnnexDir r </> literalOsPath "othertmp.lck"

{- .git/annex/misctmp/ was used by old versions of git-annex and is still
 - used during initialization -}
gitAnnexTmpOtherDirOld :: Git.Repo -> OsPath
gitAnnexTmpOtherDirOld r = addTrailingPathSeparator $ 
	gitAnnexDir r </> literalOsPath "misctmp"

{- .git/annex/watchtmp/ is used by the watcher and assistant -}
gitAnnexTmpWatcherDir :: Git.Repo -> OsPath
gitAnnexTmpWatcherDir r = addTrailingPathSeparator $
	gitAnnexDir r </> literalOsPath "watchtmp"

{- The temp file to use for a given key's content. -}
gitAnnexTmpObjectLocation :: Key -> Git.Repo -> OsPath
gitAnnexTmpObjectLocation key r = gitAnnexTmpObjectDir r </> keyFile key

{- Given a temp file such as gitAnnexTmpObjectLocation, makes a name for a
 - subdirectory in the same location, that can be used as a work area
 - when receiving the key's content.
 -
 - There are ordering requirements for creating these directories;
 - use Annex.Content.withTmpWorkDir to set them up.
 -}
gitAnnexTmpWorkDir :: OsPath -> OsPath
gitAnnexTmpWorkDir p =
	let (dir, f) = splitFileName p
	-- Using a prefix avoids name conflict with any other keys.
	in dir </> literalOsPath "work." <> f

{- .git/annex/bad/ is used for bad files found during fsck -}
gitAnnexBadDir :: Git.Repo -> OsPath
gitAnnexBadDir r = addTrailingPathSeparator $
	gitAnnexDir r </> literalOsPath "bad"

{- The bad file to use for a given key. -}
gitAnnexBadLocation :: Key -> Git.Repo -> OsPath
gitAnnexBadLocation key r = gitAnnexBadDir r </> keyFile key

{- .git/annex/foounused is used to number possibly unused keys -}
gitAnnexUnusedLog :: OsPath -> Git.Repo -> OsPath
gitAnnexUnusedLog prefix r =
	gitAnnexDir r </> (prefix <> literalOsPath "unused")

{- .git/annex/keysdb/ contains a database of information about keys. -}
gitAnnexKeysDbDir :: Git.Repo -> GitConfig -> OsPath
gitAnnexKeysDbDir r c = 
	fromMaybe (gitAnnexDir r) (annexDbDir c) </> literalOsPath "keysdb"

{- Lock file for the keys database. -}
gitAnnexKeysDbLock :: Git.Repo -> GitConfig -> OsPath
gitAnnexKeysDbLock r c = gitAnnexKeysDbDir r c <> literalOsPath ".lck"

{- Contains the stat of the last index file that was
 - reconciled with the keys database. -}
gitAnnexKeysDbIndexCache :: Git.Repo -> GitConfig -> OsPath
gitAnnexKeysDbIndexCache r c =
	gitAnnexKeysDbDir r c <> literalOsPath ".cache"

{- .git/annex/fsck/uuid/ is used to store information about incremental
 - fscks. -}
gitAnnexFsckDir :: UUID -> Git.Repo -> Maybe GitConfig -> OsPath
gitAnnexFsckDir u r mc = case annexDbDir =<< mc of
	Nothing -> go (gitAnnexDir r)
	Just d -> go d
  where
	go d = d </> literalOsPath "fsck" </> fromUUID u

{- used to store information about incremental fscks. -}
gitAnnexFsckState :: UUID -> Git.Repo -> OsPath
gitAnnexFsckState u r = gitAnnexFsckDir u r Nothing </> literalOsPath "state"

{- Directory containing database used to record fsck info. -}
gitAnnexFsckDbDir :: UUID -> Git.Repo -> GitConfig -> OsPath
gitAnnexFsckDbDir u r c = gitAnnexFsckDir u r (Just c) </> literalOsPath "fsckdb"

{- Directory containing old database used to record fsck info. -}
gitAnnexFsckDbDirOld :: UUID -> Git.Repo -> GitConfig -> OsPath
gitAnnexFsckDbDirOld u r c = gitAnnexFsckDir u r (Just c) </> literalOsPath "db"

{- Lock file for the fsck database. -}
gitAnnexFsckDbLock :: UUID -> Git.Repo -> GitConfig -> OsPath
gitAnnexFsckDbLock u r c = gitAnnexFsckDir u r (Just c) </> literalOsPath "fsck.lck"

{- .git/annex/fsckresults/uuid is used to store results of git fscks -}
gitAnnexFsckResultsLog :: UUID -> Git.Repo -> OsPath
gitAnnexFsckResultsLog u r = 
	gitAnnexDir r </> literalOsPath "fsckresults" </> fromUUID u

{- .git/annex/upgrade.log is used to record repository version upgrades. -}
gitAnnexUpgradeLog :: Git.Repo -> OsPath
gitAnnexUpgradeLog r = gitAnnexDir r </> literalOsPath "upgrade.log"

gitAnnexUpgradeLock :: Git.Repo -> OsPath
gitAnnexUpgradeLock r = gitAnnexDir r </> literalOsPath "upgrade.lck"

{- .git/annex/smudge.log is used to log smudged worktree files that need to
 - be updated. -}
gitAnnexSmudgeLog :: Git.Repo -> OsPath
gitAnnexSmudgeLog r = gitAnnexDir r </> literalOsPath "smudge.log"

gitAnnexSmudgeLock :: Git.Repo -> OsPath
gitAnnexSmudgeLock r = gitAnnexDir r </> literalOsPath "smudge.lck"

{- .git/annex/restage.log is used to log worktree files that need to be
 - restaged in git -}
gitAnnexRestageLog :: Git.Repo -> OsPath
gitAnnexRestageLog r = gitAnnexDir r </> literalOsPath "restage.log"

{- .git/annex/restage.old is used while restaging files in git -}
gitAnnexRestageLogOld :: Git.Repo -> OsPath
gitAnnexRestageLogOld r = gitAnnexDir r </> literalOsPath "restage.old"

gitAnnexRestageLock :: Git.Repo -> OsPath
gitAnnexRestageLock r = gitAnnexDir r </> literalOsPath "restage.lck"

{- .git/annex/adjust.log is used to log when the adjusted branch needs to
 - be updated. -}
gitAnnexAdjustedBranchUpdateLog :: Git.Repo -> OsPath
gitAnnexAdjustedBranchUpdateLog r = gitAnnexDir r </> literalOsPath "adjust.log"

gitAnnexAdjustedBranchUpdateLock :: Git.Repo -> OsPath
gitAnnexAdjustedBranchUpdateLock r = gitAnnexDir r </> literalOsPath "adjust.lck"

{- .git/annex/migrate.log is used to log migrations before committing them. -}
gitAnnexMigrateLog :: Git.Repo -> OsPath
gitAnnexMigrateLog r = gitAnnexDir r </> literalOsPath "migrate.log"

gitAnnexMigrateLock :: Git.Repo -> OsPath
gitAnnexMigrateLock r = gitAnnexDir r </> literalOsPath "migrate.lck"

{- .git/annex/migrations.log is used to log committed migrations. -}
gitAnnexMigrationsLog :: Git.Repo -> OsPath
gitAnnexMigrationsLog r = gitAnnexDir r </> literalOsPath "migrations.log"

gitAnnexMigrationsLock :: Git.Repo -> OsPath
gitAnnexMigrationsLock r = gitAnnexDir r </> literalOsPath "migrations.lck"

{- .git/annex/move.log is used to log moves that are in progress,
 - to better support resuming an interrupted move. -}
gitAnnexMoveLog :: Git.Repo -> OsPath
gitAnnexMoveLog r = gitAnnexDir r </> literalOsPath "move.log"

gitAnnexMoveLock :: Git.Repo -> OsPath
gitAnnexMoveLock r = gitAnnexDir r </> literalOsPath "move.lck"

{- .git/annex/export/ is used to store information about
 - exports to special remotes. -}
gitAnnexExportDir :: Git.Repo -> GitConfig -> OsPath
gitAnnexExportDir r c = fromMaybe (gitAnnexDir r) (annexDbDir c)
	</> literalOsPath "export"

{- Directory containing database used to record export info. -}
gitAnnexExportDbDir :: UUID -> Git.Repo -> GitConfig -> OsPath
gitAnnexExportDbDir u r c = 
	gitAnnexExportDir r c </> fromUUID u </> literalOsPath "exportdb"

{- Lock file for export database. -}
gitAnnexExportLock :: UUID -> Git.Repo -> GitConfig -> OsPath
gitAnnexExportLock u r c = gitAnnexExportDbDir u r c <> literalOsPath ".lck"

{- Lock file for updating the export database with information from the
 - repository. -}
gitAnnexExportUpdateLock :: UUID -> Git.Repo -> GitConfig -> OsPath
gitAnnexExportUpdateLock u r c = gitAnnexExportDbDir u r c <> literalOsPath ".upl"

{- Log file used to keep track of files that were in the tree exported to a
 - remote, but were excluded by its preferred content settings. -}
gitAnnexExportExcludeLog :: UUID -> Git.Repo -> OsPath
gitAnnexExportExcludeLog u r = gitAnnexDir r 
	</> literalOsPath "export.ex" </> fromUUID u

{- Directory containing database used to record remote content ids.
 -
 - (This used to be "cid", but a problem with the database caused it to
 - need to be rebuilt with a new name.)
 -}
gitAnnexContentIdentifierDbDir :: Git.Repo -> GitConfig -> OsPath
gitAnnexContentIdentifierDbDir r c =
	fromMaybe (gitAnnexDir r) (annexDbDir c) </> literalOsPath "cidsdb"

{- Lock file for writing to the content id database. -}
gitAnnexContentIdentifierLock :: Git.Repo -> GitConfig -> OsPath
gitAnnexContentIdentifierLock r c = 
	gitAnnexContentIdentifierDbDir r c <> literalOsPath ".lck"

{- .git/annex/import/ is used to store information about
 - imports from special remotes. -}
gitAnnexImportDir :: Git.Repo -> GitConfig -> OsPath
gitAnnexImportDir r c =
	fromMaybe (gitAnnexDir r) (annexDbDir c) </> literalOsPath "import"

{- File containing state about the last import done from a remote. -}
gitAnnexImportLog :: UUID -> Git.Repo -> GitConfig -> OsPath
gitAnnexImportLog u r c =
	gitAnnexImportDir r c </> fromUUID u </> literalOsPath "log"

{- Directory containing database used by importfeed. -}
gitAnnexImportFeedDbDir :: Git.Repo -> GitConfig -> OsPath
gitAnnexImportFeedDbDir r c =
	fromMaybe (gitAnnexDir r) (annexDbDir c) </> literalOsPath "importfeed"

{- Lock file for writing to the importfeed database. -}
gitAnnexImportFeedDbLock :: Git.Repo -> GitConfig -> OsPath
gitAnnexImportFeedDbLock r c =
	gitAnnexImportFeedDbDir r c <> literalOsPath ".lck"

{- Directory containing reposize database. -}
gitAnnexRepoSizeDbDir :: Git.Repo -> GitConfig -> OsPath
gitAnnexRepoSizeDbDir r c =
	fromMaybe (gitAnnexDir r) (annexDbDir c) </> literalOsPath "reposize" </> literalOsPath "db"

{- Lock file for the reposize database. -}
gitAnnexRepoSizeDbLock :: Git.Repo -> GitConfig -> OsPath
gitAnnexRepoSizeDbLock r c =
	fromMaybe (gitAnnexDir r) (annexDbDir c) </> literalOsPath "reposize" </> literalOsPath "lock"

{- Directory containing liveness pid files. -}
gitAnnexRepoSizeLiveDir :: Git.Repo -> GitConfig -> OsPath
gitAnnexRepoSizeLiveDir r c =
	fromMaybe (gitAnnexDir r) (annexDbDir c) </> literalOsPath "reposize" </> literalOsPath "live"

{- .git/annex/schedulestate is used to store information about when
 - scheduled jobs were last run. -}
gitAnnexScheduleState :: Git.Repo -> OsPath
gitAnnexScheduleState r = gitAnnexDir r </> literalOsPath "schedulestate"

{- .git/annex/creds/ is used to store credentials to access some special
 - remotes. -}
gitAnnexCredsDir :: Git.Repo -> OsPath
gitAnnexCredsDir r = addTrailingPathSeparator $
	gitAnnexDir r </> literalOsPath "creds"

{- .git/annex/certificate.pem and .git/annex/key.pem are used by the webapp
 - when HTTPS is enabled -}
gitAnnexWebCertificate :: Git.Repo -> FilePath
gitAnnexWebCertificate r = fromOsPath $
	gitAnnexDir r </> literalOsPath "certificate.pem"
gitAnnexWebPrivKey :: Git.Repo -> FilePath
gitAnnexWebPrivKey r = fromOsPath $
	gitAnnexDir r </> literalOsPath "privkey.pem"

{- .git/annex/feeds/ is used to record per-key (url) state by importfeed -}
gitAnnexFeedStateDir :: Git.Repo -> OsPath
gitAnnexFeedStateDir r = addTrailingPathSeparator $
	gitAnnexDir r </> literalOsPath "feedstate"

gitAnnexFeedState :: Key -> Git.Repo -> OsPath
gitAnnexFeedState k r = gitAnnexFeedStateDir r </> keyFile k

{- .git/annex/merge/ is used as a empty work tree for merges in 
 - adjusted branches. -}
gitAnnexMergeDir :: Git.Repo -> FilePath
gitAnnexMergeDir r = fromOsPath $
	addTrailingPathSeparator $ gitAnnexDir r </> literalOsPath "merge"

{- .git/annex/transfer/ is used to record keys currently
 - being transferred, and other transfer bookkeeping info. -}
gitAnnexTransferDir :: Git.Repo -> OsPath
gitAnnexTransferDir r =
	addTrailingPathSeparator $ gitAnnexDir r </> literalOsPath "transfer"

{- .git/annex/journal/ is used to journal changes made to the git-annex
 - branch -}
gitAnnexJournalDir :: BranchState -> Git.Repo -> OsPath
gitAnnexJournalDir st r = addTrailingPathSeparator $ 
	case alternateJournal st of
		Nothing -> gitAnnexDir r </> literalOsPath "journal"
		Just d -> d

{- .git/annex/journal.private/ is used to journal changes regarding private
 - repositories. -}
gitAnnexPrivateJournalDir :: BranchState -> Git.Repo -> OsPath
gitAnnexPrivateJournalDir st r = addTrailingPathSeparator $
	case alternateJournal st of
		Nothing -> gitAnnexDir r </> literalOsPath "journal-private"
		Just d -> d

{- Lock file for the journal. -}
gitAnnexJournalLock :: Git.Repo -> OsPath
gitAnnexJournalLock r = gitAnnexDir r </> literalOsPath "journal.lck"

{- Lock file for flushing a git queue that writes to the git index or
 - other git state that should only have one writer at a time. -}
gitAnnexGitQueueLock :: Git.Repo -> OsPath
gitAnnexGitQueueLock r = gitAnnexDir r </> literalOsPath "gitqueue.lck"

{- .git/annex/index is used to stage changes to the git-annex branch -}
gitAnnexIndex :: Git.Repo -> OsPath
gitAnnexIndex r = gitAnnexDir r </> literalOsPath "index"

{- .git/annex/index-private is used to store information that is not to
 - be exposed to the git-annex branch. -}
gitAnnexPrivateIndex :: Git.Repo -> OsPath
gitAnnexPrivateIndex r = gitAnnexDir r </> literalOsPath "index-private"

{- Holds the sha of the git-annex branch that the index was last updated to.
 -
 - The .lck in the name is a historical accident; this is not used as a
 - lock. -}
gitAnnexIndexStatus :: Git.Repo -> OsPath
gitAnnexIndexStatus r = gitAnnexDir r </> literalOsPath "index.lck"

{- The index file used to generate a filtered branch view._-}
gitAnnexViewIndex :: Git.Repo -> OsPath
gitAnnexViewIndex r = gitAnnexDir r </> literalOsPath "viewindex"

{- File containing a log of recently accessed views. -}
gitAnnexViewLog :: Git.Repo -> OsPath
gitAnnexViewLog r = gitAnnexDir r </> literalOsPath "viewlog"

{- List of refs that have already been merged into the git-annex branch. -}
gitAnnexMergedRefs :: Git.Repo -> OsPath
gitAnnexMergedRefs r = gitAnnexDir r </> literalOsPath "mergedrefs"

{- List of refs that should not be merged into the git-annex branch. -}
gitAnnexIgnoredRefs :: Git.Repo -> OsPath
gitAnnexIgnoredRefs r = gitAnnexDir r </> literalOsPath "ignoredrefs"

{- Pid file for daemon mode. -}
gitAnnexPidFile :: Git.Repo -> OsPath
gitAnnexPidFile r = gitAnnexDir r </> literalOsPath "daemon.pid"

{- Pid lock file for pidlock mode -}
gitAnnexPidLockFile :: Git.Repo -> OsPath
gitAnnexPidLockFile r = gitAnnexDir r </> literalOsPath "pidlock"

{- Status file for daemon mode. -}
gitAnnexDaemonStatusFile :: Git.Repo -> FilePath
gitAnnexDaemonStatusFile r = fromOsPath $
	gitAnnexDir r </> literalOsPath "daemon.status"

{- Log file for daemon mode. -}
gitAnnexDaemonLogFile :: Git.Repo -> OsPath
gitAnnexDaemonLogFile r = gitAnnexDir r </> literalOsPath "daemon.log"

{- Log file for fuzz test. -}
gitAnnexFuzzTestLogFile :: Git.Repo -> FilePath
gitAnnexFuzzTestLogFile r = fromOsPath $
	gitAnnexDir r </> literalOsPath "fuzztest.log"

{- Html shim file used to launch the webapp. -}
gitAnnexHtmlShim :: Git.Repo -> OsPath
gitAnnexHtmlShim r = gitAnnexDir r </> literalOsPath "webapp.html"

{- File containing the url to the webapp. -}
gitAnnexUrlFile :: Git.Repo -> OsPath
gitAnnexUrlFile r = gitAnnexDir r </> literalOsPath "url"

{- Temporary file used to edit configuriation from the git-annex branch. -}
gitAnnexTmpCfgFile :: Git.Repo -> OsPath
gitAnnexTmpCfgFile r = gitAnnexDir r </> literalOsPath "config.tmp"

{- .git/annex/ssh/ is used for ssh connection caching -}
gitAnnexSshDir :: Git.Repo -> OsPath
gitAnnexSshDir r = addTrailingPathSeparator $
	gitAnnexDir r </> literalOsPath "ssh"

{- .git/annex/remotes/ is used for remote-specific state. -}
gitAnnexRemotesDir :: Git.Repo -> OsPath
gitAnnexRemotesDir r = addTrailingPathSeparator $
	gitAnnexDir r </> literalOsPath "remotes"

{- This is the base directory name used by the assistant when making
 - repositories, by default. -}
gitAnnexAssistantDefaultDir :: FilePath
gitAnnexAssistantDefaultDir = "annex"

gitAnnexSimDir :: Git.Repo -> OsPath
gitAnnexSimDir r = addTrailingPathSeparator $
	gitAnnexDir r </> literalOsPath "sim"

{- Sanitizes a String that will be used as part of a Key's keyName,
 - dealing with characters that cause problems.
 -
 - This is used when a new Key is initially being generated, eg by genKey.
 - Unlike keyFile and fileKey, it does not need to be a reversible
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
keyFile :: Key -> OsPath
keyFile k = 
	let b = serializeKey'' k
	in toOsPath $ if SB.any (`elem` needesc) b
		then SB.concat $ map esc (SB.unpack b)
		else b
  where
	esc w = case chr (fromIntegral w) of
		'&' -> "&a"
		'%' -> "&s"
		':' -> "&c"
		'/' -> "%"
		_ -> SB.singleton w

	needesc = map (fromIntegral . ord) ['&', '%', ':', '/']

{- Reverses keyFile, converting a filename fragment (ie, the basename of
 - the symlink target) into a key. -}
fileKey :: OsPath -> Maybe Key
fileKey = deserializeKey' . S8.intercalate "/" . map go . S8.split '%' . fromOsPath
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
keyPath :: Key -> Hasher -> OsPath
keyPath key hasher = hasher key </> f </> f
  where
	f = keyFile key

{- All possible locations to store a key in a special remote
 - using different directory hashes.
 -
 - This is compatible with the annexLocationsNonBare and annexLocationsBare,
 - for interoperability between special remotes and git-annex repos.
 -}
keyPaths :: Key -> NE.NonEmpty OsPath
keyPaths key = NE.map (\h -> keyPath key (h def)) dirHashes

