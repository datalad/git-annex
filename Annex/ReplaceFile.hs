{- git-annex file replacing
 -
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.ReplaceFile (
	replaceGitAnnexDirFile,
	replaceGitDirFile,
	replaceWorkTreeFile,
	replaceFile,
) where

import Annex.Common
import Annex.Tmp
import Annex.Perms
import Git
import Utility.Tmp.Dir
import Utility.Directory.Create
#ifndef mingw32_HOST_OS
import Utility.Path.Max
#endif

{- replaceFile on a file located inside the gitAnnexDir. -}
replaceGitAnnexDirFile :: FilePath -> (FilePath -> Annex a) -> Annex a
replaceGitAnnexDirFile = replaceFile createAnnexDirectory

{- replaceFile on a file located inside the .git directory. -}
replaceGitDirFile :: FilePath -> (FilePath -> Annex a) -> Annex a
replaceGitDirFile = replaceFile $ \dir -> do
	top <- fromRepo localGitDir
	liftIO $ createDirectoryUnder top dir

{- replaceFile on a worktree file. -}
replaceWorkTreeFile :: FilePath -> (FilePath -> Annex a) -> Annex a
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
replaceFile :: (RawFilePath -> Annex ()) -> FilePath -> (FilePath -> Annex a) -> Annex a
replaceFile createdirectory file action = withOtherTmp $ \othertmpdir -> do
	let othertmpdir' = fromRawFilePath othertmpdir
#ifndef mingw32_HOST_OS
	-- Use part of the filename as the template for the temp
	-- directory. This does not need to be unique, but it
	-- makes it more clear what this temp directory is for.
	filemax <- liftIO $ fileNameLengthLimit othertmpdir'
	let basetmp = take (filemax `div` 2) (takeFileName file)
#else
	-- Windows has limits on the whole path length, so keep
	-- it short.
	let basetmp = "t"
#endif
	withTmpDirIn othertmpdir' basetmp $ \tmpdir -> do
		let tmpfile = tmpdir </> basetmp
		r <- action tmpfile
		replaceFileFrom tmpfile file createdirectory
		return r

replaceFileFrom :: FilePath -> FilePath -> (RawFilePath -> Annex ()) -> Annex ()
replaceFileFrom src dest createdirectory = go `catchIO` fallback
  where
	go = liftIO $ moveFile src dest
	fallback _ = do
		createdirectory (parentDir (toRawFilePath dest))
		go
