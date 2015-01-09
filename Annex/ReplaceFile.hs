{- git-annex file replacing
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.ReplaceFile where

import Common.Annex
import Annex.Perms

{- Replaces a possibly already existing file with a new version, 
 - atomically, by running an action.
 - 
 - The action is passed a temp file, which it can write to, and once
 - done the temp file is moved into place.
 -
 - The action can throw an IO exception, in which case the temp file
 - will be deleted, and the existing file will be preserved.
 -
 - Throws an IO exception when it was unable to replace the file.
 -}
replaceFile :: FilePath -> (FilePath -> Annex ()) -> Annex ()
replaceFile file action = replaceFileOr file action (liftIO . nukeFile)

{- If unable to replace the file with the temp file, runs the
 - rollback action, which is responsible for cleaning up the temp file. -}
replaceFileOr :: FilePath -> (FilePath -> Annex ()) -> (FilePath -> Annex ()) -> Annex ()
replaceFileOr file action rollback = do
	tmpdir <- fromRepo gitAnnexTmpMiscDir
	void $ createAnnexDirectory tmpdir
	tmpfile <- liftIO $ setup tmpdir
	go tmpfile `catchNonAsync` (const $ rollback tmpfile)
  where
	setup tmpdir = do
		(tmpfile, h) <- openTempFileWithDefaultPermissions tmpdir "tmp"
		hClose h
		return tmpfile
	go tmpfile = do
		action tmpfile
		liftIO $ replaceFileFrom tmpfile file

replaceFileFrom :: FilePath -> FilePath -> IO ()
replaceFileFrom src dest = go `catchIO` fallback
  where
	go = moveFile src dest
	fallback _ = do
		createDirectoryIfMissing True $ parentDir dest
		go
