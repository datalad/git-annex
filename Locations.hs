{- git-annex file locations
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Locations (
	keyFile,
	fileKey,
	keyPaths,
	gitAnnexLocation,
	annexLocations,
	gitAnnexDir,
	gitAnnexObjectDir,
	gitAnnexTmpDir,
	gitAnnexTmpLocation,
	gitAnnexBadDir,
	gitAnnexBadLocation,
	gitAnnexUnusedLog,
	gitAnnexJournalDir,
	gitAnnexJournalLock,
	gitAnnexIndex,
	gitAnnexIndexLock,
	gitAnnexIndexDirty,
	gitAnnexSshDir,
	gitAnnexRemotesDir,
	isLinkToAnnex,
	annexHashes,
	hashDirMixed,
	hashDirLower,

	prop_idempotent_fileKey
) where

import Data.Bits
import Data.Word
import Data.Hash.MD5

import Common
import Types
import Types.Key
import qualified Git

{- Conventions:
 -
 - Functions ending in "Dir" should always return values ending with a
 - trailing path separator. Most code does not rely on that, but a few
 - things do. 
 -
 - Everything else should not end in a trailing path sepatator. 
 -
 - Only functions (with names starting with "git") that build a path
 - based on a git repository should return an absolute path.
 - Everything else should use relative paths.
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
 - There are two different possibilities, using different hashes. -}
annexLocations :: Key -> [FilePath]
annexLocations key = map (annexLocation key) annexHashes
annexLocation :: Key -> Hasher -> FilePath
annexLocation key hasher = objectDir </> keyPath key hasher

{- Annexed file's absolute location in a repository.
 -
 - When there are multiple possible locations, returns the one where the
 - file is actually present.
 -
 - When the file is not present, returns the location where the file should
 - be stored.
 -}
gitAnnexLocation :: Key -> Git.Repo -> IO FilePath
gitAnnexLocation key r
	| Git.repoIsLocalBare r =
		{- Bare repositories default to hashDirLower for new
		 - content, as it's more portable. -}
		check $ map inrepo $ annexLocations key
	| otherwise =
		{- Non-bare repositories only use hashDirMixed, so
		 - don't need to do any work to check if the file is
		 - present. -}
		return $ inrepo $ annexLocation key hashDirMixed
	where
		inrepo d = Git.localGitDir r </> d
		check locs@(l:_) = fromMaybe l <$> firstM doesFileExist locs
		check [] = error "internal"

{- The annex directory of a repository. -}
gitAnnexDir :: Git.Repo -> FilePath
gitAnnexDir r = addTrailingPathSeparator $ Git.localGitDir r </> annexDir

{- The part of the annex directory where file contents are stored. -}
gitAnnexObjectDir :: Git.Repo -> FilePath
gitAnnexObjectDir r = addTrailingPathSeparator $ Git.localGitDir r </> objectDir

{- .git/annex/tmp/ is used for temp files -}
gitAnnexTmpDir :: Git.Repo -> FilePath
gitAnnexTmpDir r = addTrailingPathSeparator $ gitAnnexDir r </> "tmp"

{- The temp file to use for a given key. -}
gitAnnexTmpLocation :: Key -> Git.Repo -> FilePath
gitAnnexTmpLocation key r = gitAnnexTmpDir r </> keyFile key

{- .git/annex/bad/ is used for bad files found during fsck -}
gitAnnexBadDir :: Git.Repo -> FilePath
gitAnnexBadDir r = addTrailingPathSeparator $ gitAnnexDir r </> "bad"

{- The bad file to use for a given key. -}
gitAnnexBadLocation :: Key -> Git.Repo -> FilePath
gitAnnexBadLocation key r = gitAnnexBadDir r </> keyFile key

{- .git/annex/foounused is used to number possibly unused keys -}
gitAnnexUnusedLog :: FilePath -> Git.Repo -> FilePath
gitAnnexUnusedLog prefix r = gitAnnexDir r </> (prefix ++ "unused")

{- .git/annex/journal/ is used to journal changes made to the git-annex
 - branch -}
gitAnnexJournalDir :: Git.Repo -> FilePath
gitAnnexJournalDir r = addTrailingPathSeparator $ gitAnnexDir r </> "journal"

{- Lock file for the journal. -}
gitAnnexJournalLock :: Git.Repo -> FilePath
gitAnnexJournalLock r = gitAnnexDir r </> "journal.lck"

{- .git/annex/index is used to stage changes to the git-annex branch -}
gitAnnexIndex :: Git.Repo -> FilePath
gitAnnexIndex r = gitAnnexDir r </> "index"

{- Lock file for .git/annex/index. -}
gitAnnexIndexLock :: Git.Repo -> FilePath
gitAnnexIndexLock r = gitAnnexDir r </> "index.lck"

{- Flag file for .git/annex/index. -}
gitAnnexIndexDirty :: Git.Repo -> FilePath
gitAnnexIndexDirty r = gitAnnexDir r </> "index.dirty"

{- .git/annex/ssh/ is used for ssh connection caching -}
gitAnnexSshDir :: Git.Repo -> FilePath
gitAnnexSshDir r = addTrailingPathSeparator $ gitAnnexDir r </> "ssh"

{- .git/annex/remotes/ is used for remote-specific state. -}
gitAnnexRemotesDir :: Git.Repo -> FilePath
gitAnnexRemotesDir r = addTrailingPathSeparator $ gitAnnexDir r </> "remotes"

{- Checks a symlink target to see if it appears to point to annexed content. -}
isLinkToAnnex :: FilePath -> Bool
isLinkToAnnex s = ("/.git/" ++ objectDir) `isInfixOf` s

{- Converts a key into a filename fragment without any directory.
 -
 - Escape "/" in the key name, to keep a flat tree of files and avoid
 - issues with keys containing "/../" or ending with "/" etc. 
 -
 - "/" is escaped to "%" because it's short and rarely used, and resembles
 -     a slash
 - "%" is escaped to "&s", and "&" to "&a"; this ensures that the mapping
 -     is one to one.
 - ":" is escaped to "&c", because despite it being 2011, people still care
 -     about FAT.
 -}
keyFile :: Key -> FilePath
keyFile key = replace "/" "%" $ replace ":" "&c" $
	replace "%" "&s" $ replace "&" "&a"  $ show key

{- A location to store a key on the filesystem. A directory hash is used,
 - to protect against filesystems that dislike having many items in a
 - single directory.
 -
 - The file is put in a directory with the same name, this allows
 - write-protecting the directory to avoid accidental deletion of the file.
 -}
keyPath :: Key -> Hasher -> FilePath
keyPath key hasher = hasher key </> f </> f
	where
		f = keyFile key

{- All possibile locations to store a key using different directory hashes. -}
keyPaths :: Key -> [FilePath]
keyPaths key = map (keyPath key) annexHashes

{- Reverses keyFile, converting a filename fragment (ie, the basename of
 - the symlink target) into a key. -}
fileKey :: FilePath -> Maybe Key
fileKey file = readKey $
	replace "&a" "&" $ replace "&s" "%" $
		replace "&c" ":" $ replace "%" "/" file

{- for quickcheck -}
prop_idempotent_fileKey :: String -> Bool
prop_idempotent_fileKey s = Just k == fileKey (keyFile k)
	where k = stubKey { keyName = s, keyBackendName = "test" }

{- Two different directory hashes may be used. The mixed case hash
 - came first, and is fine, except for the problem of case-strict
 - filesystems such as Linux VFAT (mounted with shortname=mixed),
 - which do not allow using a directory "XX" when "xx" already exists.
 - To support that, most repositories use the lower case hash for new data. -}
type Hasher = Key -> FilePath
annexHashes :: [Hasher]
annexHashes = [hashDirLower, hashDirMixed]

hashDirMixed :: Hasher
hashDirMixed k = addTrailingPathSeparator $ take 2 dir </> drop 2 dir
	where
		dir = take 4 $ display_32bits_as_dir =<< [a,b,c,d]
		ABCD (a,b,c,d) = md5 $ encodeFilePath $ show k

hashDirLower :: Hasher
hashDirLower k = addTrailingPathSeparator $ take 3 dir </> drop 3 dir
	where
		dir = take 6 $ md5s $ encodeFilePath $ show k

{- modified version of display_32bits_as_hex from Data.Hash.MD5
 -   Copyright (C) 2001 Ian Lynagh 
 -   License: Either BSD or GPL
 -}
display_32bits_as_dir :: Word32 -> String
display_32bits_as_dir w = trim $ swap_pairs cs
	where 
		-- Need 32 characters to use. To avoid inaverdently making
		-- a real word, use letters that appear less frequently.
		chars = ['0'..'9'] ++ "zqjxkmvwgpfZQJXKMVWGPF"
		cs = map (\x -> getc $ (shiftR w (6*x)) .&. 31) [0..7]
		getc n = chars !! fromIntegral n
		swap_pairs (x1:x2:xs) = x2:x1:swap_pairs xs
		swap_pairs _ = []
		-- Last 2 will always be 00, so omit.
		trim = take 6
