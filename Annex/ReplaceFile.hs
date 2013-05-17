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
 -}
replaceFile :: FilePath -> (FilePath -> Annex ()) -> Annex ()
replaceFile file a = do
	tmpdir <- fromRepo gitAnnexTmpDir
	createAnnexDirectory tmpdir
	tmpfile <- liftIO $ do
		(tmpfile, h) <- openTempFileWithDefaultPermissions tmpdir $
			takeFileName file
		hClose h
		return tmpfile
	a tmpfile
	liftIO $ do
		r <- tryIO $ rename tmpfile file
		case r of
			Left _ -> do
				createDirectoryIfMissing True $ parentDir file
				rename tmpfile file
			_ -> noop
