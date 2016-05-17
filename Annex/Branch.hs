{- management of the git-annex branch
 -
 - Copyright 2011-2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

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
	getHistorical,
	change,
	maybeChange,
	commit,
	forceCommit,
	files,
	withIndex,
	performTransitions,
) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Bits.Utils
import Control.Concurrent (threadDelay)

import Annex.Common
import Annex.BranchState
import Annex.Journal
import Annex.GitOverlay
import qualified Git
import qualified Git.Command
import qualified Git.Ref
import qualified Git.RefLog
import qualified Git.Sha
import qualified Git.Branch
import qualified Git.UnionMerge
import qualified Git.UpdateIndex
import Git.LsTree (lsTreeParams)
import qualified Git.HashObject
import Annex.HashObject
import Git.Types
import Git.FilePath
import Annex.CatFile
import Annex.Perms
import Logs
import Logs.Transitions
import Logs.Trust.Pure
import Logs.Difference.Pure
import Annex.ReplaceFile
import qualified Annex.Queue
import Annex.Branch.Transitions
import qualified Annex
import Annex.Hook

{- Name of the branch that is used to store git-annex's information. -}
name :: Git.Ref
name = Git.Ref "git-annex"

{- Fully qualified name of the branch. -}
fullname :: Git.Ref
fullname = Git.Ref $ "refs/heads/" ++ fromRef name

{- Branch's name in origin. -}
originname :: Git.Ref
originname = Git.Ref $ "origin/" ++ fromRef name

{- Does origin/git-annex exist? -}
hasOrigin :: Annex Bool
hasOrigin = inRepo $ Git.Ref.exists originname

{- Does the git-annex branch or a sibling foo/git-annex branch exist? -}
hasSibling :: Annex Bool
hasSibling = not . null <$> siblingBranches

{- List of git-annex (refs, branches), including the main one and any
 - from remotes. Duplicate refs are filtered out. -}
siblingBranches :: Annex [(Git.Ref, Git.Branch)]
siblingBranches = inRepo $ Git.Ref.matchingUniq [name]

{- Creates the branch, if it does not already exist. -}
create :: Annex ()
create = void getBranch

{- Returns the ref of the branch, creating it first if necessary. -}
getBranch :: Annex Git.Ref
getBranch = maybe (hasOrigin >>= go >>= use) return =<< branchsha
  where
	go True = do
		inRepo $ Git.Command.run
			[Param "branch", Param $ fromRef name, Param $ fromRef originname]
		fromMaybe (error $ "failed to create " ++ fromRef name)
			<$> branchsha
	go False = withIndex' True $
		inRepo $ Git.Branch.commitAlways Git.Branch.AutomaticCommit "branch created" fullname []
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
 - Also handles performing any Transitions that have not yet been
 - performed, in either the local branch, or the Refs.
 -
 - Returns True if any refs were merged in, False otherwise.
 -}
updateTo :: [(Git.Ref, Git.Branch)] -> Annex Bool
updateTo pairs = do
	-- ensure branch exists, and get its current ref
	branchref <- getBranch
	dirty <- journalDirty
	ignoredrefs <- getIgnoredRefs
	(refs, branches) <- unzip <$> filterM (isnewer ignoredrefs) pairs
	if null refs
		{- Even when no refs need to be merged, the index
		 - may still be updated if the branch has gotten ahead 
		 - of the index. -}
		then whenM (needUpdateIndex branchref) $ lockJournal $ \jl -> do
			forceUpdateIndex jl branchref
			{- When there are journalled changes
			 - as well as the branch being updated,
			 - a commit needs to be done. -}
			when dirty $
				go branchref True [] [] jl
		else lockJournal $ go branchref dirty refs branches
	return $ not $ null refs
  where
	isnewer ignoredrefs (r, _)
		| S.member r ignoredrefs = return False
		| otherwise = inRepo $ Git.Branch.changed fullname r
	go branchref dirty refs branches jl = withIndex $ do
		cleanjournal <- if dirty then stageJournal jl else return noop
		let merge_desc = if null branches
			then "update"
			else "merging " ++
				unwords (map Git.Ref.describe branches) ++ 
				" into " ++ fromRef name
		localtransitions <- parseTransitionsStrictly "local"
			<$> getLocal transitionsLog
		unless (null branches) $ do
			showSideAction merge_desc
			mapM_ checkBranchDifferences refs
			mergeIndex jl refs
		let commitrefs = nub $ fullname:refs
		ifM (handleTransitions jl localtransitions commitrefs)
			( runAnnexHook postUpdateAnnexHook
			, do
				ff <- if dirty
					then return False
					else inRepo $ Git.Branch.fastForward fullname refs
				if ff
					then updateIndex jl branchref
					else commitIndex jl branchref merge_desc commitrefs
			)
		liftIO cleanjournal

{- Gets the content of a file, which may be in the journal, or in the index
 - (and committed to the branch).
 - 
 - Updates the branch if necessary, to ensure the most up-to-date available
 - content is returned.
 -
 - Returns an empty string if the file doesn't exist yet. -}
get :: FilePath -> Annex String
get file = do
	update
	getLocal file

{- Like get, but does not merge the branch, so the info returned may not
 - reflect changes in remotes.
 - (Changing the value this returns, and then merging is always the
 - same as using get, and then changing its value.) -}
getLocal :: FilePath -> Annex String
getLocal file = go =<< getJournalFileStale file
  where
	go (Just journalcontent) = return journalcontent
	go Nothing = getRaw file

getRaw :: FilePath -> Annex String
getRaw = getRef fullname

getHistorical :: RefDate -> FilePath -> Annex String
getHistorical date file =
	-- This check avoids some ugly error messages when the reflog
	-- is empty.
	ifM (null <$> inRepo (Git.RefLog.get' [Param (fromRef fullname), Param "-n1"]))
		( error ("No reflog for " ++ fromRef fullname)
		, getRef (Git.Ref.dateRef fullname date) file
		)

getRef :: Ref -> FilePath -> Annex String
getRef ref file = withIndex $ decodeBS <$> catFile ref file

{- Applies a function to modifiy the content of a file.
 -
 - Note that this does not cause the branch to be merged, it only
 - modifes the current content of the file on the branch.
 -}
change :: FilePath -> (String -> String) -> Annex ()
change file f = lockJournal $ \jl -> f <$> getLocal file >>= set jl file

{- Applies a function which can modify the content of a file, or not. -}
maybeChange :: FilePath -> (String -> Maybe String) -> Annex ()
maybeChange file f = lockJournal $ \jl -> do
	v <- getLocal file
	case f v of
		Just v' | v' /= v -> set jl file v'
		_ -> noop

{- Records new content of a file into the journal -}
set :: JournalLocked -> FilePath -> String -> Annex ()
set = setJournalFile

{- Stages the journal, and commits staged changes to the branch. -}
commit :: String -> Annex ()
commit = whenM journalDirty . forceCommit

{- Commits the current index to the branch even without any journalled
 - changes. -}
forceCommit :: String -> Annex ()
forceCommit message = lockJournal $ \jl -> do
	cleanjournal <- stageJournal jl
	ref <- getBranch
	withIndex $ commitIndex jl ref message [fullname]
	liftIO cleanjournal

{- Commits the staged changes in the index to the branch.
 - 
 - Ensures that the branch's index file is first updated to merge the state
 - of the branch at branchref, before running the commit action. This
 - is needed because the branch may have had changes pushed to it, that
 - are not yet reflected in the index.
 - 
 - The branchref value can have been obtained using getBranch at any
 - previous point, though getting it a long time ago makes the race
 - more likely to occur.
 -
 - Note that changes may be pushed to the branch at any point in time!
 - So, there's a race. If the commit is made using the newly pushed tip of
 - the branch as its parent, and that ref has not yet been merged into the
 - index, then the result is that the commit will revert the pushed
 - changes, since they have not been merged into the index. This race
 - is detected and another commit made to fix it.
 -
 - (It's also possible for the branch to be overwritten,
 - losing the commit made here. But that's ok; the data is still in the
 - index and will get committed again later.)
 -}
commitIndex :: JournalLocked -> Git.Ref -> String -> [Git.Ref] -> Annex ()
commitIndex jl branchref message parents = do
	showStoringStateAction
	commitIndex' jl branchref message message 0 parents
commitIndex' :: JournalLocked -> Git.Ref -> String -> String -> Integer -> [Git.Ref] -> Annex ()
commitIndex' jl branchref message basemessage retrynum parents = do
	updateIndex jl branchref
	committedref <- inRepo $ Git.Branch.commitAlways Git.Branch.AutomaticCommit message fullname parents
	setIndexSha committedref
	parentrefs <- commitparents <$> catObject committedref
	when (racedetected branchref parentrefs) $
		fixrace committedref parentrefs
  where
	-- look for "parent ref" lines and return the refs
	commitparents = map (Git.Ref . snd) . filter isparent .
		map (toassoc . decodeBS) . L.split newline
	newline = c2w8 '\n'
	toassoc = separate (== ' ')
	isparent (k,_) = k == "parent"
		
	{- The race can be detected by checking the commit's
	 - parent, which will be the newly pushed branch,
	 - instead of the expected ref that the index was updated to. -}
	racedetected expectedref parentrefs
		| expectedref `elem` parentrefs = False -- good parent
		| otherwise = True -- race!
		
	{- To recover from the race, union merge the lost refs
	 - into the index. -}
	fixrace committedref lostrefs = do
		showSideAction "recovering from race"
		let retrynum' = retrynum+1
		-- small sleep to let any activity that caused
		-- the race settle down
		liftIO $ threadDelay (100000 + fromInteger retrynum')
		mergeIndex jl lostrefs
		let racemessage = basemessage ++ " (recovery from race #" ++ show retrynum' ++ "; expected commit parent " ++ show branchref ++ " but found " ++ show lostrefs ++ " )"
		commitIndex' jl committedref racemessage basemessage retrynum' [committedref]

{- Lists all files on the branch. There may be duplicates in the list. -}
files :: Annex [FilePath]
files = do
	update
	(++)
		<$> branchFiles
		<*> getJournalledFilesStale

{- Files in the branch, not including any from journalled changes,
 - and without updating the branch. -}
branchFiles :: Annex [FilePath]
branchFiles = withIndex $ inRepo $ Git.Command.pipeNullSplitZombie $
	lsTreeParams fullname [Param "--name-only"]

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
mergeIndex :: JournalLocked -> [Git.Ref] -> Annex ()
mergeIndex jl branches = do
	prepareModifyIndex jl
	hashhandle <- hashObjectHandle
	ch <- catFileHandle
	inRepo $ \g -> Git.UnionMerge.mergeIndex hashhandle ch g branches

{- Removes any stale git lock file, to avoid git falling over when
 - updating the index.
 -
 - Since all modifications of the index are performed inside this module,
 - and only when the journal is locked, the fact that the journal has to be
 - locked when this is called ensures that no other process is currently
 - modifying the index. So any index.lock file must be stale, caused
 - by git running when the system crashed, or the repository's disk was
 - removed, etc.
 -}
prepareModifyIndex :: JournalLocked -> Annex ()
prepareModifyIndex _jl = do
	index <- fromRepo gitAnnexIndex
	void $ liftIO $ tryIO $ removeFile $ index ++ ".lock"

{- Runs an action using the branch's index file. -}
withIndex :: Annex a -> Annex a
withIndex = withIndex' False
withIndex' :: Bool -> Annex a -> Annex a
withIndex' bootstrapping a = do
	f <- fromRepo gitAnnexIndex
	withIndexFile f $ do
		checkIndexOnce $ unlessM (liftIO $ doesFileExist f) $ do
			unless bootstrapping create
			createAnnexDirectory $ takeDirectory f
			unless bootstrapping $ inRepo genIndex
		a

{- Updates the branch's index to reflect the current contents of the branch.
 - Any changes staged in the index will be preserved.
 -
 - Compares the ref stored in the lock file with the current
 - ref of the branch to see if an update is needed.
 -}
updateIndex :: JournalLocked -> Git.Ref -> Annex ()
updateIndex jl branchref = whenM (needUpdateIndex branchref) $
	forceUpdateIndex jl branchref

forceUpdateIndex :: JournalLocked -> Git.Ref -> Annex ()
forceUpdateIndex jl branchref = do
	withIndex $ mergeIndex jl [fullname]
	setIndexSha branchref

{- Checks if the index needs to be updated. -}
needUpdateIndex :: Git.Ref -> Annex Bool
needUpdateIndex branchref = do
	f <- fromRepo gitAnnexIndexStatus
	committedref <- Git.Ref . firstLine <$>
		liftIO (catchDefaultIO "" $ readFileStrict f)
	return (committedref /= branchref)

{- Record that the branch's index has been updated to correspond to a
 - given ref of the branch. -}
setIndexSha :: Git.Ref -> Annex ()
setIndexSha ref = do
	f <- fromRepo gitAnnexIndexStatus
	liftIO $ writeFile f $ fromRef ref ++ "\n"
	setAnnexFilePerm f
	runAnnexHook postUpdateAnnexHook

{- Stages the journal into the index and returns an action that will
 - clean up the staged journal files, which should only be run once
 - the index has been committed to the branch.
 -
 - Before staging, this removes any existing git index file lock.
 - This is safe to do because stageJournal is the only thing that
 - modifies this index file, and only one can run at a time, because
 - the journal is locked. So any existing git index file lock must be
 - stale, and the journal must contain any data that was in the process
 - of being written to the index file when it crashed.
 -}
stageJournal :: JournalLocked -> Annex (IO ())
stageJournal jl = withIndex $ do
	prepareModifyIndex jl
	g <- gitRepo
	let dir = gitAnnexJournalDir g
	(jlogf, jlogh) <- openjlog
	liftIO $ fileEncoding jlogh
	h <- hashObjectHandle
	withJournalHandle $ \jh ->
		Git.UpdateIndex.streamUpdateIndex g
			[genstream dir h jh jlogh]
	return $ cleanup dir jlogh jlogf
  where
	genstream dir h jh jlogh streamer = do
		v <- readDirectory jh
		case v of
			Nothing -> return ()
			Just file -> do
				unless (dirCruft file) $ do
					let path = dir </> file
					sha <- Git.HashObject.hashFile h path
					hPutStrLn jlogh file
					streamer $ Git.UpdateIndex.updateIndexLine
						sha FileBlob (asTopFilePath $ fileJournal file)
				genstream dir h jh jlogh streamer
	-- Clean up the staged files, as listed in the temp log file.
	-- The temp file is used to avoid needing to buffer all the
	-- filenames in memory.
	cleanup dir jlogh jlogf = do
		hFlush jlogh
		hSeek jlogh AbsoluteSeek 0
		stagedfs <- lines <$> hGetContents jlogh
		mapM_ (removeFile . (dir </>)) stagedfs
		hClose jlogh
		nukeFile jlogf
	openjlog = do
		tmpdir <- fromRepo gitAnnexTmpMiscDir
		createAnnexDirectory tmpdir
		liftIO $ openTempFile tmpdir "jlog"

{- This is run after the refs have been merged into the index,
 - but before the result is committed to the branch.
 - (Which is why it's passed the contents of the local branches's
 - transition log before that merge took place.)
 -
 - When the refs contain transitions that have not yet been done locally,
 - the transitions are performed on the index, and a new branch
 - is created from the result.
 -
 - When there are transitions recorded locally that have not been done
 - to the remote refs, the transitions are performed in the index,
 - and committed to the existing branch. In this case, the untransitioned
 - remote refs cannot be merged into the branch (since transitions
 - throw away history), so they are added to the list of refs to ignore,
 - to avoid re-merging content from them again.
 -}
handleTransitions :: JournalLocked -> Transitions -> [Git.Ref] -> Annex Bool
handleTransitions jl localts refs = do
	m <- M.fromList <$> mapM getreftransition refs
	let remotets = M.elems m
	if all (localts ==) remotets
		then return False
		else do
			let allts = combineTransitions (localts:remotets)
			let (transitionedrefs, untransitionedrefs) =
				partition (\r -> M.lookup r m == Just allts) refs
			performTransitionsLocked jl allts (localts /= allts) transitionedrefs
			ignoreRefs untransitionedrefs
			return True
  where
	getreftransition ref = do
		ts <- parseTransitionsStrictly "remote" . decodeBS
			<$> catFile ref transitionsLog
		return (ref, ts)

ignoreRefs :: [Git.Ref] -> Annex ()
ignoreRefs rs = do
	old <- getIgnoredRefs
	let s = S.unions [old, S.fromList rs]
	f <- fromRepo gitAnnexIgnoredRefs
	replaceFile f $ \tmp -> liftIO $ writeFile tmp $
		unlines $ map fromRef $ S.elems s

getIgnoredRefs :: Annex (S.Set Git.Ref)
getIgnoredRefs = S.fromList . mapMaybe Git.Sha.extractSha . lines <$> content
  where
	content = do
		f <- fromRepo gitAnnexIgnoredRefs
		liftIO $ catchDefaultIO "" $ readFile f

{- Performs the specified transitions on the contents of the index file,
 - commits it to the branch, or creates a new branch.
 -}
performTransitions :: Transitions -> Bool -> [Ref] -> Annex ()
performTransitions ts neednewlocalbranch transitionedrefs = lockJournal $ \jl ->
	performTransitionsLocked jl ts neednewlocalbranch transitionedrefs
performTransitionsLocked :: JournalLocked -> Transitions -> Bool -> [Ref] -> Annex ()
performTransitionsLocked jl ts neednewlocalbranch transitionedrefs = do
	-- For simplicity & speed, we're going to use the Annex.Queue to
	-- update the git-annex branch, while it usually holds changes
	-- for the head branch. Flush any such changes.
	Annex.Queue.flush
	withIndex $ do
		prepareModifyIndex jl
		run $ mapMaybe getTransitionCalculator $ transitionList ts
		Annex.Queue.flush
		if neednewlocalbranch
			then do
				committedref <- inRepo $ Git.Branch.commitAlways Git.Branch.AutomaticCommit message fullname transitionedrefs
				setIndexSha committedref
			else do
				ref <- getBranch
				commitIndex jl ref message (nub $ fullname:transitionedrefs)
  where
	message
		| neednewlocalbranch && null transitionedrefs = "new branch for transition " ++ tdesc
		| otherwise = "continuing transition " ++ tdesc
	tdesc = show $ map describeTransition $ transitionList ts

	{- The changes to make to the branch are calculated and applied to
	 - the branch directly, rather than going through the journal,
	 - which would be innefficient. (And the journal is not designed
	 - to hold changes to every file in the branch at once.)
	 -
	 - When a file in the branch is changed by transition code,
	 - that value is remembered and fed into the code for subsequent
	 - transitions.
	 -}
	run [] = noop
	run changers = do
		trustmap <- calcTrustMap <$> getRaw trustLog
		fs <- branchFiles
		forM_ fs $ \f -> do
			content <- getRaw f
			apply changers f content trustmap
	apply [] _ _ _ = return ()
	apply (changer:rest) file content trustmap =
		case changer file content trustmap of
			RemoveFile -> do
				Annex.Queue.addUpdateIndex
					=<< inRepo (Git.UpdateIndex.unstageFile file)
				-- File is deleted; can't run any other
				-- transitions on it.
				return ()
			ChangeFile content' -> do
				sha <- hashBlob content'
				Annex.Queue.addUpdateIndex $ Git.UpdateIndex.pureStreamer $
					Git.UpdateIndex.updateIndexLine sha FileBlob (asTopFilePath file)
				apply rest file content' trustmap
			PreserveFile ->
				apply rest file content trustmap

checkBranchDifferences :: Git.Ref -> Annex ()
checkBranchDifferences ref = do
	theirdiffs <- allDifferences . parseDifferencesLog . decodeBS
		<$> catFile ref differenceLog
	mydiffs <- annexDifferences <$> Annex.getGitConfig
	when (theirdiffs /= mydiffs) $
		error "Remote repository is tuned in incompatable way; cannot be merged with local repository."
