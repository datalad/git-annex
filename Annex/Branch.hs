{- management of the git-annex branch
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Branch (
	fullname,
	name,
	hasOrigin,
	hasSibling,
	siblingBranches,
	create,
	update,
	forceUpdate,
	updateTo,
	get,
	change,
	commit,
	files,
) where

import qualified Data.ByteString.Lazy.Char8 as L

import Common.Annex
import Annex.BranchState
import Annex.Journal
import qualified Git
import qualified Git.Command
import qualified Git.Ref
import qualified Git.Branch
import qualified Git.UnionMerge
import qualified Git.UpdateIndex
import Git.HashObject
import Git.Types
import Git.FilePath
import Annex.CatFile
import Annex.Perms
import qualified Annex
import Utility.Env

{- Name of the branch that is used to store git-annex's information. -}
name :: Git.Ref
name = Git.Ref "git-annex"

{- Fully qualified name of the branch. -}
fullname :: Git.Ref
fullname = Git.Ref $ "refs/heads/" ++ show name

{- Branch's name in origin. -}
originname :: Git.Ref
originname = Git.Ref $ "origin/" ++ show name

{- Does origin/git-annex exist? -}
hasOrigin :: Annex Bool
hasOrigin = inRepo $ Git.Ref.exists originname

{- Does the git-annex branch or a sibling foo/git-annex branch exist? -}
hasSibling :: Annex Bool
hasSibling = not . null <$> siblingBranches

{- List of git-annex (refs, branches), including the main one and any
 - from remotes. Duplicate refs are filtered out. -}
siblingBranches :: Annex [(Git.Ref, Git.Branch)]
siblingBranches = inRepo $ Git.Ref.matchingUniq name

{- Creates the branch, if it does not already exist. -}
create :: Annex ()
create = void getBranch

{- Returns the ref of the branch, creating it first if necessary. -}
getBranch :: Annex Git.Ref
getBranch = maybe (hasOrigin >>= go >>= use) return =<< branchsha
  where
	go True = do
		inRepo $ Git.Command.run
			[Param "branch", Param $ show name, Param $ show originname]
		fromMaybe (error $ "failed to create " ++ show name)
			<$> branchsha
	go False = withIndex' True $
		inRepo $ Git.Branch.commit "branch created" fullname []
	use sha = do
		setIndexSha sha
		return sha
	branchsha = inRepo $ Git.Ref.sha fullname

{- Ensures that the branch and index are up-to-date; should be
 - called before data is read from it. Runs only once per git-annex run. -}
update :: Annex ()
update = runUpdateOnce $ void $ updateTo =<< siblingBranches

{- Forces an update even if one has already been run. -}
forceUpdate :: Annex Bool
forceUpdate = updateTo =<< siblingBranches

{- Merges the specified Refs into the index, if they have any changes not
 - already in it. The Branch names are only used in the commit message;
 - it's even possible that the provided Branches have not been updated to
 - point to the Refs yet.
 - 
 - The branch is fast-forwarded if possible, otherwise a merge commit is
 - made.
 -
 - Before Refs are merged into the index, it's important to first stage the
 - journal into the index. Otherwise, any changes in the journal would
 - later get staged, and might overwrite changes made during the merge.
 - This is only done if some of the Refs do need to be merged.
 -
 - Returns True if any refs were merged in, False otherwise.
 -}
updateTo :: [(Git.Ref, Git.Branch)] -> Annex Bool
updateTo pairs = do
	-- ensure branch exists, and get its current ref
	branchref <- getBranch
	dirty <- journalDirty
	(refs, branches) <- unzip <$> filterM isnewer pairs
	if null refs
		{- Even when no refs need to be merged, the index
		 - may still be updated if the branch has gotten ahead 
		 - of the index. -}
		then whenM (needUpdateIndex branchref) $ lockJournal $ do
			forceUpdateIndex branchref
			{- When there are journalled changes
			 - as well as the branch being updated,
			 - a commit needs to be done. -}
			when dirty $
				go branchref True [] []
		else lockJournal $ go branchref dirty refs branches
	return $ not $ null refs
  where
	isnewer (r, _) = inRepo $ Git.Branch.changed fullname r
	go branchref dirty refs branches = withIndex $ do
		cleanjournal <- if dirty then stageJournal else return noop
		let merge_desc = if null branches
			then "update"
			else "merging " ++
				unwords (map Git.Ref.describe branches) ++ 
				" into " ++ show name
		unless (null branches) $ do
			showSideAction merge_desc
			mergeIndex refs
		ff <- if dirty
			then return False
			else inRepo $ Git.Branch.fastForward fullname refs
		if ff
			then updateIndex branchref
			else commitBranch branchref merge_desc
				(nub $ fullname:refs)
		liftIO cleanjournal

{- Gets the content of a file, which may be in the journal, or in the index
 - (and committed to the branch).
 - 
 - Updates the branch if necessary, to ensure the most up-to-date available
 - content is available.
 -
 - Returns an empty string if the file doesn't exist yet. -}
get :: FilePath -> Annex String
get file = do
	update
	get' file

{- Like get, but does not merge the branch, so the info returned may not
 - reflect changes in remotes.
 - (Changing the value this returns, and then merging is always the
 - same as using get, and then changing its value.) -}
getStale :: FilePath -> Annex String
getStale = get'

get' :: FilePath -> Annex String
get' file = go =<< getJournalFile file
  where
	go (Just journalcontent) = return journalcontent
	go Nothing = withIndex $ L.unpack <$> catFile fullname file

{- Applies a function to modifiy the content of a file.
 -
 - Note that this does not cause the branch to be merged, it only
 - modifes the current content of the file on the branch.
 -}
change :: FilePath -> (String -> String) -> Annex ()
change file a = lockJournal $ a <$> getStale file >>= set file

{- Records new content of a file into the journal -}
set :: FilePath -> String -> Annex ()
set = setJournalFile

{- Stages the journal, and commits staged changes to the branch. -}
commit :: String -> Annex ()
commit message = whenM journalDirty $ lockJournal $ do
	cleanjournal <- stageJournal
	ref <- getBranch
	withIndex $ commitBranch ref message [fullname]
	liftIO cleanjournal

{- Commits the staged changes in the index to the branch.
 - 
 - Ensures that the branch's index file is first updated to the state
 - of the branch at branchref, before running the commit action. This
 - is needed because the branch may have had changes pushed to it, that
 - are not yet reflected in the index.
 -
 - Also safely handles a race that can occur if a change is being pushed
 - into the branch at the same time. When the race happens, the commit will
 - be made on top of the newly pushed change, but without the index file
 - being updated to include it. The result is that the newly pushed
 - change is reverted. This race is detected and another commit made
 - to fix it.
 - 
 - The branchref value can have been obtained using getBranch at any
 - previous point, though getting it a long time ago makes the race
 - more likely to occur.
 -}
commitBranch :: Git.Ref -> String -> [Git.Ref] -> Annex ()
commitBranch branchref message parents = do
	showStoringStateAction
	commitBranch' branchref message parents
commitBranch' :: Git.Ref -> String -> [Git.Ref] -> Annex ()
commitBranch' branchref message parents = do
	updateIndex branchref
	committedref <- inRepo $ Git.Branch.commit message fullname parents
	setIndexSha committedref
	parentrefs <- commitparents <$> catObject committedref
	when (racedetected branchref parentrefs) $
		fixrace committedref parentrefs
  where
	-- look for "parent ref" lines and return the refs
	commitparents = map (Git.Ref . snd) . filter isparent .
		map (toassoc . L.unpack) . L.lines
	toassoc = separate (== ' ')
	isparent (k,_) = k == "parent"
		
	{- The race can be detected by checking the commit's
	 - parent, which will be the newly pushed branch,
	 - instead of the expected ref that the index was updated to. -}
	racedetected expectedref parentrefs
		| expectedref `elem` parentrefs = False -- good parent
		| otherwise = True -- race!
		
	{- To recover from the race, union merge the lost refs
	 - into the index, and recommit on top of the bad commit. -}
	fixrace committedref lostrefs = do
		mergeIndex lostrefs
		commitBranch committedref racemessage [committedref]
		
	racemessage = message ++ " (recovery from race)"

{- Lists all files on the branch. There may be duplicates in the list. -}
files :: Annex [FilePath]
files = do
	update
	withIndex $ do
		bfiles <- inRepo $ Git.Command.pipeNullSplitZombie
			[ Params "ls-tree --name-only -r -z"
			, Param $ show fullname
			]
		jfiles <- getJournalledFiles
		return $ jfiles ++ bfiles

{- Populates the branch's index file with the current branch contents.
 - 
 - This is only done when the index doesn't yet exist, and the index 
 - is used to build up changes to be commited to the branch, and merge
 - in changes from other branches.
 -}
genIndex :: Git.Repo -> IO ()
genIndex g = Git.UpdateIndex.streamUpdateIndex g
	[Git.UpdateIndex.lsTree fullname g]

{- Merges the specified refs into the index.
 - Any changes staged in the index will be preserved. -}
mergeIndex :: [Git.Ref] -> Annex ()
mergeIndex branches = do
	h <- catFileHandle
	inRepo $ \g -> Git.UnionMerge.mergeIndex h g branches

{- Runs an action using the branch's index file. -}
withIndex :: Annex a -> Annex a
withIndex = withIndex' False
withIndex' :: Bool -> Annex a -> Annex a
withIndex' bootstrapping a = do
	f <- fromRepo gitAnnexIndex
	g <- gitRepo
#ifdef __ANDROID__
	{- This should not be necessary on Android, but there is some
	 - weird getEnvironment breakage. See
	 - https://github.com/neurocyte/ghc-android/issues/7
	 - Use getEnv to get some key environment variables that
	 - git expects to have. -}
	let keyenv = words "USER PATH GIT_EXEC_PATH HOSTNAME HOME"
	let getEnvPair k = maybe Nothing (\v -> Just (k, v)) <$> getEnv k
	e <- liftIO $ catMaybes <$> forM keyenv getEnvPair
#else
	e <- liftIO getEnvironment
#endif
	let g' = g { gitEnv = Just $ ("GIT_INDEX_FILE", f):e }

	Annex.changeState $ \s -> s { Annex.repo = g' }
	checkIndexOnce $ unlessM (liftIO $ doesFileExist f) $ do
		unless bootstrapping create
		liftIO $ createDirectoryIfMissing True $ takeDirectory f
		unless bootstrapping $ inRepo genIndex
	r <- a
	Annex.changeState $ \s -> s { Annex.repo = (Annex.repo s) { gitEnv = gitEnv g} }

	return r

{- Updates the branch's index to reflect the current contents of the branch.
 - Any changes staged in the index will be preserved.
 -
 - Compares the ref stored in the lock file with the current
 - ref of the branch to see if an update is needed.
 -}
updateIndex :: Git.Ref -> Annex ()
updateIndex branchref = whenM (needUpdateIndex branchref) $
	forceUpdateIndex branchref

forceUpdateIndex :: Git.Ref -> Annex ()
forceUpdateIndex branchref = do
	withIndex $ mergeIndex [fullname]
	setIndexSha branchref

{- Checks if the index needs to be updated. -}
needUpdateIndex :: Git.Ref -> Annex Bool
needUpdateIndex branchref = do
	lock <- fromRepo gitAnnexIndexLock
	lockref <- Git.Ref . firstLine <$>
		liftIO (catchDefaultIO "" $ readFileStrict lock)
	return (lockref /= branchref)

{- Record that the branch's index has been updated to correspond to a
 - given ref of the branch. -}
setIndexSha :: Git.Ref -> Annex ()
setIndexSha ref = do
	lock <- fromRepo gitAnnexIndexLock
	liftIO $ writeFile lock $ show ref ++ "\n"
	setAnnexPerm lock

{- Stages the journal into the index and returns an action that will
 - clean up the staged journal files, which should only be run once
 - the index has been committed to the branch. Should be run within
 - lockJournal, to prevent others from modifying the journal. -}
stageJournal :: Annex (IO ())
stageJournal = withIndex $ do
	g <- gitRepo
	let dir = gitAnnexJournalDir g
	fs <- getJournalFiles
	liftIO $ do
		h <- hashObjectStart g
		Git.UpdateIndex.streamUpdateIndex g
			[genstream dir h fs]
		hashObjectStop h
	return $ liftIO $ mapM_ (removeFile . (dir </>)) fs
  where
	genstream dir h fs streamer = forM_ fs $ \file -> do
		let path = dir </> file
		sha <- hashFile h path
		streamer $ Git.UpdateIndex.updateIndexLine
			sha FileBlob (asTopFilePath $ fileJournal file)
