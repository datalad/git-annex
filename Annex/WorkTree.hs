{- git-annex worktree files
 -
 - Copyright 2013-2021 Joey Hess <id@joeyh.name>
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
 - by examining what the file links to.
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

{- Modifies an action to only act on files that are already annexed,
 - and passes the key on to it. -}
whenAnnexed :: (RawFilePath -> Key -> Annex (Maybe a)) -> RawFilePath -> Annex (Maybe a)
whenAnnexed a file = ifAnnexed file (a file) (return Nothing)

ifAnnexed :: RawFilePath -> (Key -> Annex a) -> Annex a -> Annex a
ifAnnexed file yes no = maybe no yes =<< lookupKey file

{- Find all annexed files and update the keys database for them.
 - 
 - Normally the keys database is updated incrementally when it's being
 - opened, and changes are noticed. Calling this explicitly allows
 - running the update at an earlier point.
 -}
scanAnnexedFiles :: Annex ()
scanAnnexedFiles =
	-- All that needs to be done is to open the database,
	-- that will result in Database.Keys.reconcileStaged
	-- running, and doing the work.
	Database.Keys.runWriter (const noop)
