{- git-annex worktree files
 -
 - Copyright 2013-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.WorkTree where

import Annex.Common
import Annex.Link
import Annex.CatFile
import Annex.CurrentBranch
import qualified Database.Keys

{- Looks up the key corresponding to an annexed file in the work tree,
 - by examining what the symlink points to.
 -
 - An unlocked file will not have a link on disk, so fall back to
 - looking for a pointer to a key in git.
 -
 - When in an adjusted branch that may have hidden the file, looks for a
 - pointer to a key in the original branch.
 -}
lookupKey :: RawFilePath -> Annex (Maybe Key)
lookupKey = lookupKey' catkeyfile
  where
	catkeyfile file =
		ifM (liftIO $ doesFileExist $ fromRawFilePath file)
			( catKeyFile file
			, catKeyFileHidden file =<< getCurrentBranch
			)

{- Like lookupKey, but only looks at files staged in git, not at unstaged
 - changes in the work tree. This means it's slower, but it also has
 - consistently the same behavior for locked files as for unlocked files.
 -}
lookupKeyStaged :: RawFilePath -> Annex (Maybe Key)
lookupKeyStaged file = catKeyFile file >>= \case
	Just k -> return (Just k)
	Nothing -> catKeyFileHidden file =<< getCurrentBranch

{- Like lookupKey, but does not find keys for hidden files. -}
lookupKeyNotHidden :: RawFilePath -> Annex (Maybe Key)
lookupKeyNotHidden = lookupKey' catkeyfile
  where
	catkeyfile file =
		ifM (liftIO $ doesFileExist $ fromRawFilePath file)
			( catKeyFile file
			, return Nothing
			)

lookupKey' :: (RawFilePath -> Annex (Maybe Key)) -> RawFilePath -> Annex (Maybe Key)
lookupKey' catkeyfile file = isAnnexLink file >>= \case
	Just key -> return (Just key)
	Nothing -> catkeyfile file

{- Find all annexed files and update the keys database for them. -}
scanAnnexedFiles :: Annex ()
scanAnnexedFiles = Database.Keys.updateDatabase
