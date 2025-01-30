{- git-annex file replacing
 -
 - Copyright 2013-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.ReplaceFile (
	replaceGitAnnexDirFile,
	replaceGitDirFile,
	replaceWorkTreeFile,
	replaceFile,
	replaceFile',
) where

import Annex.Common
import Annex.Tmp
import Annex.Perms
import Git
import Utility.Tmp
import Utility.Tmp.Dir
import Utility.Directory.Create

import qualified System.FilePath.ByteString as P

{- replaceFile on a file located inside the gitAnnexDir. -}
replaceGitAnnexDirFile :: RawFilePath -> (RawFilePath -> Annex a) -> Annex a
replaceGitAnnexDirFile = replaceFile createAnnexDirectory

{- replaceFile on a file located inside the .git directory. -}
replaceGitDirFile :: RawFilePath -> (RawFilePath -> Annex a) -> Annex a
replaceGitDirFile = replaceFile $ \dir -> do
	top <- fromRepo localGitDir
	liftIO $ createDirectoryUnder [top] dir

{- replaceFile on a worktree file. -}
replaceWorkTreeFile :: RawFilePath -> (RawFilePath -> Annex a) -> Annex a
replaceWorkTreeFile = replaceFile createWorkTreeDirectory

{- Replaces a possibly already existing file with a new version, 
 - atomically, by running an action.
 - 
 - The action is passed the name of temp file, in a temp directory, 
 - which it can write to, and once done the temp file is moved into place
 - and anything else in the temp directory is deleted.
 -
 - The action can throw an exception, in which case the temp directory
 - will be deleted, and the existing file will be preserved.
 -
 - Throws an IO exception when it was unable to replace the file.
 -
 - The createdirectory action is only run when moving the file into place
 - fails, and can create any parent directory structure needed.
 -}
replaceFile :: (RawFilePath -> Annex ()) -> RawFilePath -> (RawFilePath -> Annex a) -> Annex a
replaceFile createdirectory file action = replaceFile' createdirectory file (const True) action

replaceFile' :: (RawFilePath -> Annex ()) -> RawFilePath -> (a -> Bool) -> (RawFilePath -> Annex a) -> Annex a
replaceFile' createdirectory file checkres action = withOtherTmp $ \othertmpdir -> do
#ifndef mingw32_HOST_OS
	let basetmp = relatedTemplate' (P.takeFileName file)
#else
	let basetmp = toRawFilePath "t"
#endif
	withTmpDirIn (fromRawFilePath othertmpdir) (toOsPath basetmp) $ \tmpdir -> do
		let tmpfile = toRawFilePath tmpdir P.</> basetmp
		r <- action tmpfile
		when (checkres r) $
			replaceFileFrom tmpfile file createdirectory
		return r

replaceFileFrom :: RawFilePath -> RawFilePath -> (RawFilePath -> Annex ()) -> Annex ()
replaceFileFrom src dest createdirectory = go `catchIO` fallback
  where
	go = liftIO $ moveFile src dest
	fallback _ = do
		createdirectory (parentDir dest)
		go
