{- git-annex direct mode
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Direct where

import Common.Annex
import qualified Git
import qualified Git.LsFiles
import qualified Git.UpdateIndex
import qualified Git.HashObject
import qualified Git.Merge
import qualified Git.DiffTree as DiffTree
import Git.Sha
import Git.Types
import Annex.CatFile
import Utility.FileMode
import qualified Annex.Queue
import Logs.Location
import Backend
import Types.KeySource
import Annex.Content
import Annex.Content.Direct
import Utility.CopyFile

{- Uses git ls-files to find files that need to be committed, and stages
 - them into the index. Returns True if some changes were staged. -}
stageDirect :: Annex Bool
stageDirect = do
	Annex.Queue.flush
	top <- fromRepo Git.repoPath
	(l, cleanup) <- inRepo $ Git.LsFiles.stagedDetails [top]
	forM_ l go
	void $ liftIO cleanup
	staged <- Annex.Queue.size
	Annex.Queue.flush
	return $ staged /= 0
  where
	{- Determine what kind of modified or deleted file this is, as
	 - efficiently as we can, by getting any key that's associated
	 - with it in git, as well as its stat info. -}
	go (file, Just sha) = do
		mkey <- catKey sha
		mstat <- liftIO $ catchMaybeIO $ getSymbolicLinkStatus file
		case (mkey, mstat, toCache =<< mstat) of
			(Just key, _, Just cache) -> do
				{- All direct mode files will show as
				 - modified, so compare the cache to see if
				 - it really was. -}
				oldcache <- recordedCache key
				when (oldcache /= Just cache) $
					modifiedannexed file key cache
			(Just key, Nothing, _) -> deletedannexed file key
			(Nothing, Nothing, _) -> deletegit file
			(_, Just _, _) -> addgit file
	go _ = noop

	modifiedannexed file oldkey cache = do
		void $ removeAssociatedFile oldkey file
		void $ addDirect file cache
	
	deletedannexed file key = do
		void $ removeAssociatedFile key file
		deletegit file
	
	addgit file = Annex.Queue.addCommand "add" [Param "-f"] [file]

	deletegit file = Annex.Queue.addCommand "rm" [Param "-f"] [file]

{- Adds a file to the annex in direct mode. Can fail, if the file is
 - modified or deleted while it's being added. -}
addDirect :: FilePath -> Cache -> Annex Bool
addDirect file cache = do
	showStart "add" file
	let source = KeySource
		{ keyFilename = file
		, contentLocation = file
		}
	got =<< genKey source =<< chooseBackend file
  where
	got Nothing = do
		showEndFail
		return False
	got (Just (key, _)) = ifM (compareCache file $ Just cache)
		( do
			link <- calcGitLink file key
			sha <- inRepo $ Git.HashObject.hashObject BlobObject link
			Annex.Queue.addUpdateIndex =<<
				inRepo (Git.UpdateIndex.stageSymlink file sha)
			writeCache key cache
			void $ addAssociatedFile key file
			logStatus key InfoPresent
			showEndOk
			return True
		, do
			showEndFail
			return False
		)

{- In direct mode, git merge would usually refuse to do anything, since it
 - sees present direct mode files as type changed files. To avoid this,
 - merge is run with the work tree set to a temp directory.
 -
 - This should only be used once any changes to the real working tree have
 - already been committed, because it overwrites files in the working tree.
 -}
mergeDirect :: FilePath -> Git.Ref -> Git.Repo -> IO Bool
mergeDirect d branch g = do
	createDirectoryIfMissing True d
	let g' = g { location = Local { gitdir = Git.localGitDir g, worktree = Just d } }
	Git.Merge.mergeNonInteractive branch g'

{- Cleans up after a direct mode merge. The merge must have been committed,
 - and the commit sha passed in, along with the old sha of the tree
 - before the merge. Uses git diff-tree to find files that changed between
 - the two shas, and applies those changes to the work tree.
 -}
mergeDirectCleanup :: FilePath -> Git.Ref -> Git.Ref -> Annex ()
mergeDirectCleanup d oldsha newsha = do
	(items, cleanup) <- inRepo $ DiffTree.diffTreeRecursive oldsha newsha
	forM_ items updated
	void $ liftIO $ cleanup
	liftIO $ removeDirectoryRecursive d
  where
	updated item = do
		go DiffTree.srcsha DiffTree.srcmode moveout moveout_raw
		go DiffTree.dstsha DiffTree.dstmode movein movein_raw
	  where
		go getsha getmode a araw
			| getsha item == nullSha = noop
			| isSymLink (getmode item) =
				maybe (araw f) (\k -> void $ a k f)
					=<< catKey (getsha item)
			| otherwise = araw f
		f = DiffTree.file item

	moveout k f = removeDirect k f

	{- Files deleted by the merge are removed from the work tree.
	 - Empty work tree directories are removed, per git behavior. -}
	moveout_raw f = liftIO $ do
		nukeFile f
		void $ catchMaybeIO $ removeDirectory $ parentDir f
	
	{- The symlink is created from the key, rather than moving in the
	 - symlink created in the temp directory by the merge. This because
	 - a conflicted merge will write to some other file in the temp
	 - directory.
	 -
 	 - Symlinks are replaced with their content, if it's available. -}
	movein k f = do
		l <- calcGitLink f k
		liftIO $ replaceFile f $ const $
			createSymbolicLink l f
		toDirect k f
	
	{- Any new, modified, or renamed files were written to the temp
	 - directory by the merge, and are moved to the real work tree. -}
	movein_raw f = liftIO $ do
		createDirectoryIfMissing True $ parentDir f
		void $ catchMaybeIO $ rename (d </> f) f

{- If possible, converts a symlink in the working tree into a direct
 - mode file. -}
toDirect :: Key -> FilePath -> Annex ()
toDirect k f = maybe noop id =<< toDirectGen k f

toDirectGen :: Key -> FilePath -> Annex (Maybe (Annex ()))
toDirectGen k f = do
	loc <- inRepo $ gitAnnexLocation k
	createContentDir loc -- thaws directory too
	locs <- filter (/= normalise f) <$> addAssociatedFile k f
	case locs of
		[] -> ifM (liftIO $ doesFileExist loc)
			( return $ Just $ do
				{- Move content from annex to direct file. -}
				updateCache k loc
				thawContent loc
				liftIO $ replaceFile f $ moveFile loc
			, return Nothing
			)
		(loc':_) -> ifM (liftIO $ not . isSymbolicLink <$> getSymbolicLinkStatus loc')
			{- Another direct file has the content; copy it. -}
			( return $ Just $ do
				liftIO $ replaceFile f $
					void . copyFileExternal loc'
			, return Nothing
			)

{- Removes a direct mode file, while retaining its content. -}
removeDirect :: Key -> FilePath -> Annex ()
removeDirect k f = do
	locs <- removeAssociatedFile k f
	when (null locs) $ do
		r <- liftIO $ catchMaybeIO $ getSymbolicLinkStatus f
		case r of
			Just s
				| not (isSymbolicLink s) ->
					moveAnnex k f
			_ -> noop
	liftIO $ do
		nukeFile f
		void $ catchMaybeIO $ removeDirectory $ parentDir f

{- Called when a direct mode file has been changed. Its old content may be
 - lost. -}
changedDirect :: Key -> FilePath -> Annex ()
changedDirect oldk f = do
	locs <- removeAssociatedFile oldk f
	whenM (pure (null locs) <&&> not <$> inAnnex oldk) $
		logStatus oldk InfoMissing
