{- git-annex direct mode
 -
 - Copyright 2012, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Direct where

import Common.Annex
import qualified Annex
import qualified Git
import qualified Git.LsFiles
import qualified Git.Merge
import qualified Git.DiffTree as DiffTree
import qualified Git.Config
import qualified Git.Ref
import qualified Git.Branch
import Git.Sha
import Git.FilePath
import Git.Types
import Config
import Annex.CatFile
import qualified Annex.Queue
import Logs.Location
import Backend
import Types.KeySource
import Annex.Content
import Annex.Content.Direct
import Annex.Link
import Utility.InodeCache
import Utility.CopyFile
import Annex.Perms
import Annex.ReplaceFile
import Annex.Exception
import Annex.VariantFile

{- Uses git ls-files to find files that need to be committed, and stages
 - them into the index. Returns True if some changes were staged. -}
stageDirect :: Annex Bool
stageDirect = do
	Annex.Queue.flush
	top <- fromRepo Git.repoPath
	(l, cleanup) <- inRepo $ Git.LsFiles.stagedOthersDetails [top]
	forM_ l go
	void $ liftIO cleanup
	staged <- Annex.Queue.size
	Annex.Queue.flush
	return $ staged /= 0
  where
	{- Determine what kind of modified or deleted file this is, as
	 - efficiently as we can, by getting any key that's associated
	 - with it in git, as well as its stat info. -}
	go (file, Just sha, Just mode) = do
		shakey <- catKey sha mode
		mstat <- liftIO $ catchMaybeIO $ getSymbolicLinkStatus file
		filekey <- isAnnexLink file
		case (shakey, filekey, mstat, toInodeCache =<< mstat) of
			(_, Just key, _, _)
				| shakey == filekey -> noop
				{- A changed symlink. -}
				| otherwise -> stageannexlink file key
			(Just key, _, _, Just cache) -> do
				{- All direct mode files will show as
				 - modified, so compare the cache to see if
				 - it really was. -}
				oldcache <- recordedInodeCache key
				case oldcache of
					[] -> modifiedannexed file key cache
					_ -> unlessM (elemInodeCaches cache oldcache) $
						modifiedannexed file key cache
			(Just key, _, Nothing, _) -> deletedannexed file key
			(Nothing, _, Nothing, _) -> deletegit file
			(_, _, Just _, _) -> addgit file
	go _ = noop

	modifiedannexed file oldkey cache = do
		void $ removeAssociatedFile oldkey file
		void $ addDirect file cache
	
	deletedannexed file key = do
		void $ removeAssociatedFile key file
		deletegit file
	
	stageannexlink file key = do
		l <- inRepo $ gitAnnexLink file key
		stageSymlink file =<< hashSymlink l
		void $ addAssociatedFile key file

	addgit file = Annex.Queue.addCommand "add" [Param "-f"] [file]

	deletegit file = Annex.Queue.addCommand "rm" [Param "-qf"] [file]

{- Run before a commit to update direct mode bookeeping to reflect the
 - staged changes being committed. -}
preCommitDirect :: Annex Bool
preCommitDirect = do
	(diffs, clean) <- inRepo $ DiffTree.diffIndex Git.Ref.headRef
	makeabs <- flip fromTopFilePath <$> gitRepo
	forM_ diffs (go makeabs)
	liftIO clean
  where
	go makeabs diff = do
		withkey (DiffTree.srcsha diff) (DiffTree.srcmode diff) removeAssociatedFile
		withkey (DiffTree.dstsha diff) (DiffTree.dstmode diff) addAssociatedFile
	  where
		withkey sha mode a = when (sha /= nullSha) $ do
			k <- catKey sha mode
			case k of
				Nothing -> noop
				Just key -> void $ a key $
					makeabs $ DiffTree.file diff

{- Adds a file to the annex in direct mode. Can fail, if the file is
 - modified or deleted while it's being added. -}
addDirect :: FilePath -> InodeCache -> Annex Bool
addDirect file cache = do
	showStart "add" file
	let source = KeySource
		{ keyFilename = file
		, contentLocation = file
		, inodeCache = Just cache
		}
	got =<< genKey source =<< chooseBackend file
  where
	got Nothing = do
		showEndFail
		return False
	got (Just (key, _)) = ifM (sameInodeCache file [cache])
		( do
			l <- inRepo $ gitAnnexLink file key
			stageSymlink file =<< hashSymlink l
			addInodeCache key cache
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
 -}
mergeDirect :: FilePath -> Git.Ref -> Git.Repo -> IO Bool
mergeDirect d branch g = do
	whenM (doesDirectoryExist d) $
		removeDirectoryRecursive d
	createDirectoryIfMissing True d
	let g' = g { location = Local { gitdir = Git.localGitDir g, worktree = Just d } }
	Git.Merge.mergeNonInteractive branch g'

{- Cleans up after a direct mode merge. The merge must have been committed,
 - and the commit sha passed in, along with the old sha of the tree
 - before the merge. Uses git diff-tree to find files that changed between
 - the two shas, and applies those changes to the work tree.
 -
 - There are really only two types of changes: An old item can be deleted,
 - or a new item added. Two passes are made, first deleting and then
 - adding. This is to handle cases where eg, a file is deleted and a
 - directory is added. (The diff-tree output may list these in the opposite
 - order, but we cannot add the directory until the file with the
 - same name is removed.)
 -}
mergeDirectCleanup :: FilePath -> Git.Ref -> Git.Ref -> Annex ()
mergeDirectCleanup d oldsha newsha = do
	(items, cleanup) <- inRepo $ DiffTree.diffTreeRecursive oldsha newsha
	makeabs <- flip fromTopFilePath <$> gitRepo
	let fsitems = zip (map (makeabs . DiffTree.file) items) items
	forM_ fsitems $
		go makeabs DiffTree.srcsha DiffTree.srcmode moveout moveout_raw
	forM_ fsitems $
		go makeabs DiffTree.dstsha DiffTree.dstmode movein movein_raw
	void $ liftIO cleanup
	liftIO $ removeDirectoryRecursive d
  where
	go makeabs getsha getmode a araw (f, item)
		| getsha item == nullSha = noop
		| otherwise = void $
			tryAnnex . maybe (araw item makeabs f) (\k -> void $ a item makeabs k f)
				=<< catKey (getsha item) (getmode item)

	moveout _ _ = removeDirect

	{- Files deleted by the merge are removed from the work tree.
	 - Empty work tree directories are removed, per git behavior. -}
	moveout_raw _ _ f = liftIO $ do
		nukeFile f
		void $ tryIO $ removeDirectory $ parentDir f
	
	{- If the file is already present, with the right content for the
	 - key, it's left alone. 
	 -
	 - If the file is already present, and does not exist in the
	 - oldsha branch, preserve this local file.
	 -
	 - Otherwise, create the symlink and then if possible, replace it
	 - with the content. -}
	movein item makeabs k f = unlessM (goodContent k f) $ do
		preserveUnannexed item makeabs f oldsha
		l <- inRepo $ gitAnnexLink f k
		replaceFile f $ makeAnnexLink l
		toDirect k f
	
	{- Any new, modified, or renamed files were written to the temp
	 - directory by the merge, and are moved to the real work tree. -}
	movein_raw item makeabs f = do
		preserveUnannexed item makeabs f oldsha
		liftIO $ do
			createDirectoryIfMissing True $ parentDir f
			void $ tryIO $ rename (d </> getTopFilePath (DiffTree.file item)) f

{- If the file that's being moved in is already present in the work
 - tree, but did not exist in the oldsha branch, preserve this
 - local, unannexed file (or directory), as "variant-local".
 -
 - It's also possible that the file that's being moved in
 - is in a directory that collides with an exsting, non-annexed
 - file (not a directory), which should be preserved.
 -}
preserveUnannexed :: DiffTree.DiffTreeItem -> (TopFilePath -> FilePath) -> FilePath -> Ref -> Annex ()
preserveUnannexed item makeabs absf oldsha = do
	whenM (liftIO (collidingitem absf) <&&> unannexed absf) $
		liftIO $ findnewname absf 0
	checkdirs (DiffTree.file item)
  where
	checkdirs from = do
		let p = parentDir (getTopFilePath from)
		let d = asTopFilePath p
		unless (null p) $ do
			let absd = makeabs d
			whenM (liftIO (colliding_nondir absd) <&&> unannexed absd) $
				liftIO $ findnewname absd 0
			checkdirs d
			
	collidingitem f = isJust
		<$> catchMaybeIO (getSymbolicLinkStatus f)
	colliding_nondir f = maybe False (not . isDirectory)
		<$> catchMaybeIO (getSymbolicLinkStatus f)

	unannexed f = (isNothing <$> isAnnexLink f)
		<&&> (isNothing <$> catFileDetails oldsha f)

	findnewname :: FilePath -> Int -> IO ()
	findnewname f n = do
		let localf = mkVariant f 
			("local" ++ if n > 0 then show n else "")
		ifM (collidingitem localf)
			( findnewname f (n+1)
			, rename f localf
				`catchIO` const (findnewname f (n+1))
			)

{- If possible, converts a symlink in the working tree into a direct
 - mode file. If the content is not available, leaves the symlink
 - unchanged. -}
toDirect :: Key -> FilePath -> Annex ()
toDirect k f = fromMaybe noop =<< toDirectGen k f

toDirectGen :: Key -> FilePath -> Annex (Maybe (Annex ()))
toDirectGen k f = do
	loc <- calcRepo $ gitAnnexLocation k
	ifM (liftIO $ doesFileExist loc)
		( return $ Just $ fromindirect loc
		, do
			{- Copy content from another direct file. -}
			absf <- liftIO $ absPath f
			dlocs <- filterM (goodContent k) =<<
				filterM (\l -> isNothing <$> getAnnexLinkTarget l) =<<
				(filter (/= absf) <$> addAssociatedFile k f)
			case dlocs of
				[] -> return Nothing
				(dloc:_) -> return $ Just $ fromdirect dloc
		)
  where
  	fromindirect loc = do
		{- Move content from annex to direct file. -}
		updateInodeCache k loc
		void $ addAssociatedFile k f
		modifyContent loc $ do
			thawContent loc
			replaceFile f $ liftIO . moveFile loc
	fromdirect loc = do
		replaceFile f $
			liftIO . void . copyFileExternal loc
		updateInodeCache k f

{- Removes a direct mode file, while retaining its content in the annex
 - (unless its content has already been changed). -}
removeDirect :: Key -> FilePath -> Annex ()
removeDirect k f = do
	void $ removeAssociatedFileUnchecked k f
	unlessM (inAnnex k) $
		ifM (goodContent k f)
			( moveAnnex k f
			, logStatus k InfoMissing
			)
	liftIO $ do
		nukeFile f
		void $ tryIO $ removeDirectory $ parentDir f

{- Called when a direct mode file has been changed. Its old content may be
 - lost. -}
changedDirect :: Key -> FilePath -> Annex ()
changedDirect oldk f = do
	locs <- removeAssociatedFile oldk f
	whenM (pure (null locs) <&&> not <$> inAnnex oldk) $
		logStatus oldk InfoMissing

{- Enable/disable direct mode. -}
setDirect :: Bool -> Annex ()
setDirect wantdirect = do
	if wantdirect
		then do
			switchHEAD
			setbare
		else do
			setbare
			switchHEADBack
	setConfig (annexConfig "direct") val
	Annex.changeGitConfig $ \c -> c { annexDirect = wantdirect }
  where
	val = Git.Config.boolConfig wantdirect
	setbare = setConfig (ConfigKey Git.Config.coreBare) val

{- Since direct mode sets core.bare=true, incoming pushes could change
 - the currently checked out branch. To avoid this problem, HEAD
 - is changed to a internal ref that nothing is going to push to.
 -
 - For refs/heads/master, use refs/heads/annex/direct/master;
 - this way things that show HEAD (eg shell prompts) will
 - hopefully show just "master". -}
directBranch :: Ref -> Ref
directBranch orighead = case split "/" $ fromRef orighead of
	("refs":"heads":"annex":"direct":_) -> orighead
	("refs":"heads":rest) ->
		Ref $ "refs/heads/annex/direct/" ++ intercalate "/" rest
	_ -> Ref $ "refs/heads/" ++ fromRef (Git.Ref.base orighead)

{- Converts a directBranch back to the original branch.
 -
 - Any other ref is left unchanged.
 -}
fromDirectBranch :: Ref -> Ref
fromDirectBranch directhead = case split "/" $ fromRef directhead of
	("refs":"heads":"annex":"direct":rest) -> 
		Ref $ "refs/heads/" ++ intercalate "/" rest
	_ -> directhead

switchHEAD :: Annex ()
switchHEAD = maybe noop switch =<< inRepo Git.Branch.currentUnsafe
  where
	switch orighead = do
		let newhead = directBranch orighead
		maybe noop (inRepo . Git.Branch.update newhead)
			=<< inRepo (Git.Ref.sha orighead)
		inRepo $ Git.Branch.checkout newhead

switchHEADBack :: Annex ()
switchHEADBack = maybe noop switch =<< inRepo Git.Branch.currentUnsafe
  where
	switch currhead = do
		let orighead = fromDirectBranch currhead
		v <- inRepo $ Git.Ref.sha currhead
		case v of
			Just headsha
				| orighead /= currhead -> do
					inRepo $ Git.Branch.update orighead headsha
					inRepo $ Git.Branch.checkout orighead
					inRepo $ Git.Branch.delete currhead
			_ -> inRepo $ Git.Branch.checkout orighead
