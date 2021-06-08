{- git-annex worktree files
 -
 - Copyright 2013-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.WorkTree where

import Annex.Common
import qualified Annex
import Annex.Link
import Annex.CatFile
import Annex.Content
import Annex.ReplaceFile
import Annex.CurrentBranch
import Annex.InodeSentinal
import Utility.InodeCache
import Git.FilePath
import Git.CatFile
import qualified Git.Ref
import qualified Git.LsTree
import qualified Git.Types
import qualified Database.Keys
import Config
import qualified Utility.RawFilePath as R

import qualified Data.ByteString.Lazy as L

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
 - This is expensive, and so normally the associated files are updated
 - incrementally when changes are noticed. So, this only needs to be done
 - when initializing/upgrading a repository.
 -
 - Also, the content for an unlocked file may already be present as
 - an annex object. If so, populate the pointer file with it.
 - But if worktree file does not have a pointer file's content, it is left
 - as-is.
 -}
scanAnnexedFiles :: Annex ()
scanAnnexedFiles = whenM (inRepo Git.Ref.headExists <&&> not <$> isBareRepo) $ do
	-- This gets the keys database populated with all annexed files,
	-- by running Database.Keys.reconcileStaged.
	Database.Keys.runWriter (const noop)
	-- The above tries to populate pointer files, but one thing it
	-- is not able to handle is populating a pointer file when the
	-- annex object file already exists, but its inode is not yet
	-- cached and annex.thin is set. So, the rest of this makes
	-- another pass over the tree to do that.
	whenM (annexThin <$> Annex.getGitConfig) $ do
		g <- Annex.gitRepo
		(l, cleanup) <- inRepo $ Git.LsTree.lsTree
			Git.LsTree.LsTreeRecursive
			(Git.LsTree.LsTreeLong True)
			Git.Ref.headRef
		catObjectStreamLsTree l want g go
		liftIO $ void cleanup
  where
	-- Want to process symlinks, and regular files.
	want i = case Git.Types.toTreeItemType (Git.LsTree.mode i) of
		Just Git.Types.TreeSymlink -> Just (i, False)
		Just Git.Types.TreeFile -> checkfilesize i
		Just Git.Types.TreeExecutable -> checkfilesize i
		_ -> Nothing
	
	-- Avoid processing files that are too large to be pointer files.
	checkfilesize i = case Git.LsTree.size i of
		Just n | n < maxPointerSz -> Just (i, True)
		_ -> Nothing
	
	go getnext = liftIO getnext >>= \case
		Just ((i, isregfile), Just c) -> do
			maybe noop (add i isregfile)
				(parseLinkTargetOrPointer (L.toStrict c))
			go getnext
		_ -> return ()
	
	add i isregfile k = do
		let tf = Git.LsTree.file i
		whenM (pure isregfile <&&> inAnnex k) $ do
			f <- fromRepo $ fromTopFilePath tf
			liftIO (isPointerFile f) >>= \case
				Just k' | k' == k -> do
					destmode <- liftIO $ catchMaybeIO $
						fileMode <$> R.getFileStatus f
					ic <- replaceWorkTreeFile (fromRawFilePath f) $ \tmp -> do
						let tmp' = toRawFilePath tmp
						linkFromAnnex k tmp' destmode >>= \case
							LinkAnnexOk -> 
								withTSDelta (liftIO . genInodeCache tmp')
							LinkAnnexNoop -> return Nothing
							LinkAnnexFailed -> liftIO $ do
								writePointerFile tmp' k destmode
								return Nothing
					maybe noop (restagePointerFile (Restage True) f) ic
				_ -> noop
