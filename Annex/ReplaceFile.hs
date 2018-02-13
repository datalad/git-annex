{- git-annex file replacing
 -
 - Copyright 2013-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.ReplaceFile where

import Annex.Common
import Annex.Perms
import Utility.Tmp.Dir
import Utility.Path.Max

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
 -}
replaceFile :: FilePath -> (FilePath -> Annex ()) -> Annex ()
replaceFile file action = do
	misctmpdir <- fromRepo gitAnnexTmpMiscDir
	void $ createAnnexDirectory misctmpdir
#ifndef mingw32_HOST_OS
	-- Use part of the filename as the template for the temp
	-- directory. This does not need to be unique, but it
	-- makes it more clear what this temp directory is for.
	filemax <- liftIO $ fileNameLengthLimit misctmpdir
	let basetmp = take (filemax `div` 2) (takeFileName file)
#else
	-- Windows has limits on the whole path length, so keep
	-- it short.
	let basetmp = "t"
#endif
	withTmpDirIn misctmpdir basetmp $ \tmpdir -> do
		let tmpfile = tmpdir </> basetmp
		action tmpfile
		liftIO $ replaceFileFrom tmpfile file

replaceFileFrom :: FilePath -> FilePath -> IO ()
replaceFileFrom src dest = go `catchIO` fallback
  where
	go = moveFile src dest
	fallback _ = do
		createDirectoryIfMissing True $ parentDir dest
		go
