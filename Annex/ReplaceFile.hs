{- git-annex file replacing
 -
 - Copyright 2013-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.ReplaceFile where

import Common.Annex
import Annex.Perms
import Utility.Tmp

{- Replaces a possibly already existing file with a new version, 
 - atomically, by running an action.
 - 
 - The action is passed the name of temp file, in a temp directory, 
 - which it can write to, and once done the temp file is moved into place
 - and anything else in the temp directory is deleted.
 -
 - The action can throw an IO exception, in which case the temp directory
 - will be deleted, and the existing file will be preserved.
 -
 - Throws an IO exception when it was unable to replace the file.
 -}
replaceFile :: FilePath -> (FilePath -> Annex ()) -> Annex ()
replaceFile file action = do
	misctmpdir <- fromRepo gitAnnexTmpMiscDir
	void $ createAnnexDirectory misctmpdir
	let basetmp = takeFileName file
	withTmpDirIn misctmpdir basetmp $ \tmpdir -> do
		let tmpfile = tmpdir <> basetmp
		action tmpfile
		liftIO $ replaceFileFrom tmpfile file

replaceFileFrom :: FilePath -> FilePath -> IO ()
replaceFileFrom src dest = go `catchIO` fallback
  where
	go = moveFile src dest
	fallback _ = do
		createDirectoryIfMissing True $ parentDir dest
		go
