{- adjusted branch
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Annex.AdjustedBranch (
	Adjustment(..),
	OrigBranch,
	AdjBranch(..),
	originalToAdjusted,
	adjustedToOriginal,
	fromAdjustedBranch,
	getAdjustment,
	enterAdjustedBranch,
	adjustBranch,
	adjustToCrippledFileSystem,
	updateAdjustedBranch,
	propigateAdjustedCommits,
	checkAdjustedClone,
	isGitVersionSupported,
	checkVersionSupported,
) where

import Annex.Common
import qualified Annex
import Git
import Git.Types
import qualified Git.Branch
import qualified Git.Ref
import qualified Git.Command
import qualified Git.Tree
import qualified Git.DiffTree
import qualified Git.Merge
import Git.Tree (TreeItem(..))
import Git.Sha
import Git.Env
import Git.Index
import Git.FilePath
import qualified Git.LockFile
import qualified Git.Version
import Annex.Version
import Annex.CatFile
import Annex.Link
import Annex.AutoMerge
import Annex.Content
import Annex.Perms
import Annex.GitOverlay
import Utility.Tmp
import qualified Database.Keys

import qualified Data.Map as M

data Adjustment
	= UnlockAdjustment
	| LockAdjustment
	| FixAdjustment
	| UnFixAdjustment
	| HideMissingAdjustment
	| ShowMissingAdjustment
	deriving (Show, Eq)

reverseAdjustment :: Adjustment -> Adjustment
reverseAdjustment UnlockAdjustment = LockAdjustment
reverseAdjustment LockAdjustment = UnlockAdjustment
reverseAdjustment HideMissingAdjustment = ShowMissingAdjustment
reverseAdjustment ShowMissingAdjustment = HideMissingAdjustment
reverseAdjustment FixAdjustment = UnFixAdjustment
reverseAdjustment UnFixAdjustment = FixAdjustment

{- How to perform various adjustments to a TreeItem. -}
adjustTreeItem :: Adjustment -> TreeItem -> Annex (Maybe TreeItem)
adjustTreeItem UnlockAdjustment = ifSymlink adjustToPointer noAdjust
adjustTreeItem LockAdjustment = ifSymlink noAdjust adjustToSymlink
adjustTreeItem FixAdjustment = ifSymlink adjustToSymlink noAdjust
adjustTreeItem UnFixAdjustment = ifSymlink (adjustToSymlink' gitAnnexLinkCanonical) noAdjust
adjustTreeItem HideMissingAdjustment = \ti@(TreeItem _ _ s) -> do
	mk <- catKey s
	case mk of
		Just k -> ifM (inAnnex k)
			( return (Just ti)
			, return Nothing
			)
		Nothing -> return (Just ti)
adjustTreeItem ShowMissingAdjustment = noAdjust

ifSymlink :: (TreeItem -> Annex a) -> (TreeItem -> Annex a) -> TreeItem -> Annex a
ifSymlink issymlink notsymlink ti@(TreeItem _f m _s)
	| toBlobType m == Just SymlinkBlob = issymlink ti
	| otherwise = notsymlink ti

noAdjust :: TreeItem -> Annex (Maybe TreeItem)
noAdjust = return . Just

adjustToPointer :: TreeItem -> Annex (Maybe TreeItem)
adjustToPointer ti@(TreeItem f _m s) = do
	mk <- catKey s
	case mk of
		Just k -> do
			Database.Keys.addAssociatedFile k f
			Just . TreeItem f (fromBlobType FileBlob)
				<$> hashPointerFile k
		Nothing -> return (Just ti)

adjustToSymlink :: TreeItem -> Annex (Maybe TreeItem)
adjustToSymlink = adjustToSymlink' gitAnnexLink

adjustToSymlink' :: (FilePath -> Key -> Git.Repo -> GitConfig -> IO FilePath) -> TreeItem -> Annex (Maybe TreeItem)
adjustToSymlink' gitannexlink ti@(TreeItem f _m s) = do
	mk <- catKey s
	case mk of
		Just k -> do
			absf <- inRepo $ \r -> absPath $
				fromTopFilePath f r
			linktarget <- calcRepo $ gitannexlink absf k
			Just . TreeItem f (fromBlobType SymlinkBlob)
				<$> hashSymlink linktarget
		Nothing -> return (Just ti)

type OrigBranch = Branch
newtype AdjBranch = AdjBranch { adjBranch :: Branch }

-- This is a hidden branch ref, that's used as the basis for the AdjBranch,
-- since pushes can overwrite the OrigBranch at any time. So, changes
-- are propigated from the AdjBranch to the head of the BasisBranch.
newtype BasisBranch = BasisBranch Ref

-- The basis for refs/heads/adjusted/master(unlocked) is
-- refs/basis/adjusted/master(unlocked).
basisBranch :: AdjBranch -> BasisBranch
basisBranch (AdjBranch adjbranch) = BasisBranch $
	Ref ("refs/basis/" ++ fromRef (Git.Ref.base adjbranch))

adjustedBranchPrefix :: String
adjustedBranchPrefix = "refs/heads/adjusted/"

serialize :: Adjustment -> String
serialize UnlockAdjustment = "unlocked"
serialize LockAdjustment = "locked"
serialize HideMissingAdjustment = "present"
serialize ShowMissingAdjustment = "showmissing"
serialize FixAdjustment = "fixed"
serialize UnFixAdjustment = "unfixed"

deserialize :: String -> Maybe Adjustment
deserialize "unlocked" = Just UnlockAdjustment
deserialize "locked" = Just UnlockAdjustment
deserialize "present" = Just HideMissingAdjustment
deserialize "fixed" = Just FixAdjustment
deserialize "unfixed" = Just UnFixAdjustment
deserialize _ = Nothing

originalToAdjusted :: OrigBranch -> Adjustment -> AdjBranch
originalToAdjusted orig adj = AdjBranch $ Ref $
	adjustedBranchPrefix ++ base ++ '(' : serialize adj ++ ")"
  where
	base = fromRef (Git.Ref.basename orig)

adjustedToOriginal :: Branch -> Maybe (Adjustment, OrigBranch)
adjustedToOriginal b
	| adjustedBranchPrefix `isPrefixOf` bs = do
		let (base, as) = separate (== '(') (drop prefixlen bs)
		adj <- deserialize (takeWhile (/= ')') as)
		Just (adj, Git.Ref.under "refs/heads" (Ref base))
	| otherwise = Nothing
  where
	bs = fromRef b
	prefixlen = length adjustedBranchPrefix

getAdjustment :: Branch -> Maybe Adjustment
getAdjustment = fmap fst . adjustedToOriginal

fromAdjustedBranch :: Branch -> OrigBranch
fromAdjustedBranch b = maybe b snd (adjustedToOriginal b)

originalBranch :: Annex (Maybe OrigBranch)
originalBranch = fmap fromAdjustedBranch <$> inRepo Git.Branch.current

{- Enter an adjusted version of current branch (or, if already in an
 - adjusted version of a branch, changes the adjustment of the original
 - branch).
 -
 - Can fail, if no branch is checked out, or if the adjusted branch already
 - exists, or perhaps if staged changes conflict with the adjusted branch.
 -}
enterAdjustedBranch :: Adjustment -> Annex Bool
enterAdjustedBranch adj = go =<< originalBranch
  where
	go (Just origbranch) = do
		let adjbranch = adjBranch $ originalToAdjusted origbranch adj
		ifM (inRepo (Git.Ref.exists adjbranch) <&&> (not <$> Annex.getState Annex.force))
			( do
				mapM_ (warning . unwords)
					[ [ "adjusted branch"
					  , Git.Ref.describe adjbranch
					  , "already exists."
					  ]
					, [ "Aborting because that branch may have changes that have not yet reached"
					  , Git.Ref.describe origbranch
					  ]
					, [ "You can check out the adjusted branch manually to enter it,"
					  , "or delete the adjusted branch and re-run this command."
					  ]
					]
				return False
			, do
				AdjBranch b <- preventCommits $ const $ 
					adjustBranch adj origbranch
				showOutput -- checkout can have output in large repos
				inRepo $ Git.Command.runBool
					[ Param "checkout"
					, Param $ fromRef $ Git.Ref.base b
					]
			)
	go Nothing = do
		warning "not on any branch!"
		return False

adjustToCrippledFileSystem :: Annex ()
adjustToCrippledFileSystem = do
	warning "Entering an adjusted branch where files are unlocked as this filesystem does not support locked files."
	whenM (isNothing <$> originalBranch) $
		void $ inRepo $ Git.Branch.commitCommand Git.Branch.AutomaticCommit
			[ Param "--quiet"
			, Param "--allow-empty"
			, Param "-m"
			, Param "commit before entering adjusted unlocked branch"
			]
	unlessM (enterAdjustedBranch UnlockAdjustment) $
		warning "Failed to enter adjusted branch!"

setBasisBranch :: BasisBranch -> Ref -> Annex ()
setBasisBranch (BasisBranch basis) new = 
	inRepo $ Git.Branch.update' basis new

setAdjustedBranch :: String -> AdjBranch -> Ref -> Annex ()
setAdjustedBranch msg (AdjBranch b) r = inRepo $ Git.Branch.update msg b r

adjustBranch :: Adjustment -> OrigBranch -> Annex AdjBranch
adjustBranch adj origbranch = do
	-- Start basis off with the current value of the origbranch.
	setBasisBranch basis origbranch
	sha <- adjustCommit adj basis
	setAdjustedBranch "entering adjusted branch" adjbranch sha
	return adjbranch
  where
	adjbranch = originalToAdjusted origbranch adj
	basis = basisBranch adjbranch

adjustCommit :: Adjustment -> BasisBranch -> Annex Sha
adjustCommit adj basis = do
	treesha <- adjustTree adj basis
	commitAdjustedTree treesha basis

adjustTree :: Adjustment -> BasisBranch -> Annex Sha
adjustTree adj (BasisBranch basis) = do
	let toadj = adjustTreeItem adj
	treesha <- Git.Tree.adjustTree toadj [] [] basis =<< Annex.gitRepo
	return treesha

type CommitsPrevented = Git.LockFile.LockHandle

{- Locks git's index file, preventing git from making a commit, merge, 
 - or otherwise changing the HEAD ref while the action is run.
 -
 - Throws an IO exception if the index file is already locked.
 -}
preventCommits :: (CommitsPrevented -> Annex a) -> Annex a
preventCommits = bracket setup cleanup
  where
	setup = do
		lck <- fromRepo indexFileLock
		liftIO $ Git.LockFile.openLock lck
	cleanup = liftIO . Git.LockFile.closeLock

{- Commits a given adjusted tree, with the provided parent ref.
 -
 - This should always yield the same value, even if performed in different 
 - clones of a repo, at different times. The commit message and other
 - metadata is based on the parent.
 -}
commitAdjustedTree :: Sha -> BasisBranch -> Annex Sha
commitAdjustedTree treesha parent@(BasisBranch b) =
	commitAdjustedTree' treesha parent [b]

commitAdjustedTree' :: Sha -> BasisBranch -> [Ref] -> Annex Sha
commitAdjustedTree' treesha (BasisBranch basis) parents =
	go =<< catCommit basis
  where
	go Nothing = inRepo mkcommit
	go (Just basiscommit) = inRepo $ commitWithMetaData
		(commitAuthorMetaData basiscommit)
		(commitCommitterMetaData basiscommit)
		mkcommit
	mkcommit = Git.Branch.commitTree Git.Branch.AutomaticCommit
		adjustedBranchCommitMessage parents treesha

adjustedBranchCommitMessage :: String
adjustedBranchCommitMessage = "git-annex adjusted branch"

{- Update the currently checked out adjusted branch, merging the provided
 - branch into it. Note that the provided branch should be a non-adjusted
 - branch. -}
updateAdjustedBranch :: Branch -> (OrigBranch, Adjustment) -> [Git.Merge.MergeConfig] -> Git.Branch.CommitMode -> Annex Bool
updateAdjustedBranch tomerge (origbranch, adj) mergeconfig commitmode = catchBoolIO $
	join $ preventCommits go
  where
	adjbranch@(AdjBranch currbranch) = originalToAdjusted origbranch adj
	basis = basisBranch adjbranch

	go commitsprevented =
		ifM (inRepo $ Git.Branch.changed currbranch tomerge)
			( do
				(updatedorig, _) <- propigateAdjustedCommits'
					origbranch adj commitsprevented
				changestomerge updatedorig
			, nochangestomerge
			)

	nochangestomerge = return $ return True

	{- Since the adjusted branch changes files, merging tomerge
	 - directly into it would likely result in unncessary merge
	 - conflicts. To avoid those conflicts, instead merge tomerge into
	 - updatedorig. The result of the merge can the be
	 - adjusted to yield the final adjusted branch.
	 -
	 - In order to do a merge into a ref that is not checked out,
	 - set the work tree to a temp directory, and set GIT_DIR
	 - to another temp directory, in which HEAD contains the
	 - updatedorig sha. GIT_COMMON_DIR is set to point to the real
	 - git directory, and so git can read and write objects from there,
	 - but will use GIT_DIR for HEAD and index.
	 -
	 - (Doing the merge this way also lets it run even though the main
	 - index file is currently locked.)
	 -}
	changestomerge (Just updatedorig) = do
		misctmpdir <- fromRepo gitAnnexTmpMiscDir
		void $ createAnnexDirectory misctmpdir
		tmpwt <- fromRepo gitAnnexMergeDir
		withTmpDirIn misctmpdir "git" $ \tmpgit -> withWorkTreeRelated tmpgit $
			withemptydir tmpwt $ withWorkTree tmpwt $ do
				liftIO $ writeFile (tmpgit </> "HEAD") (fromRef updatedorig)
				showAction $ "Merging into " ++ fromRef (Git.Ref.base origbranch)
				-- The --no-ff is important; it makes git
				-- merge not care that the work tree is empty.
				merged <- inRepo (Git.Merge.merge' [Param "--no-ff"] tomerge mergeconfig commitmode)
					<||> (resolveMerge (Just updatedorig) tomerge True <&&> commitResolvedMerge commitmode)
				if merged
					then do
						!mergecommit <- liftIO $ extractSha <$> readFile (tmpgit </> "HEAD")
						-- This is run after the commit lock is dropped.
						return $ postmerge mergecommit
					else return $ return False
	changestomerge Nothing = return $ return False
	
	withemptydir d a = bracketIO setup cleanup (const a)
	  where
		setup = do
			whenM (doesDirectoryExist d) $
				removeDirectoryRecursive d
			createDirectoryIfMissing True d
		cleanup _ = removeDirectoryRecursive d

	{- A merge commit has been made between the basisbranch and 
	 - tomerge. Update the basisbranch and origbranch to point
	 - to that commit, adjust it to get the new adjusted branch,
	 - and check it out.
	 -
	 - But, there may be unstaged work tree changes that conflict, 
	 - so the check out is done by making a normal merge of
	 - the new adjusted branch.
	 -}
	postmerge (Just mergecommit) = do
		setBasisBranch basis mergecommit
		inRepo $ Git.Branch.update' origbranch mergecommit
		adjtree <- adjustTree adj (BasisBranch mergecommit)
		adjmergecommit <- commitAdjustedTree adjtree (BasisBranch mergecommit)
		-- Make currbranch be the parent, so that merging
		-- this commit will be a fast-forward.
		adjmergecommitff <- commitAdjustedTree' adjtree (BasisBranch mergecommit) [currbranch]
		showAction "Merging into adjusted branch"
		ifM (autoMergeFrom adjmergecommitff (Just currbranch) mergeconfig commitmode)
			( reparent adjtree adjmergecommit =<< getcurrentcommit
			, return False
			)
	postmerge Nothing = return False

	-- Now that the merge into the adjusted branch is complete,
	-- take the tree from that merge, and attach it on top of the
	-- adjmergecommit, if it's different.
	reparent adjtree adjmergecommit (Just currentcommit) = do
		if (commitTree currentcommit /= adjtree)
			then do
				c <- inRepo $ Git.Branch.commitTree Git.Branch.AutomaticCommit
					("Merged " ++ fromRef tomerge) [adjmergecommit]
					(commitTree currentcommit)
				inRepo $ Git.Branch.update "updating adjusted branch" currbranch c
				propigateAdjustedCommits origbranch adj
			else inRepo $ Git.Branch.update "updating adjusted branch" currbranch adjmergecommit
		return True
	reparent _ _ Nothing = return False

	getcurrentcommit = do
		v <- inRepo Git.Branch.currentUnsafe
		case v of
			Nothing -> return Nothing
			Just c -> catCommit c

{- Check for any commits present on the adjusted branch that have not yet
 - been propigated to the basis branch, and propigate them to the basis
 - branch and from there on to the orig branch.
 -
 - After propigating the commits back to the basis banch,
 - rebase the adjusted branch on top of the updated basis branch.
 -}
propigateAdjustedCommits :: OrigBranch -> Adjustment -> Annex ()
propigateAdjustedCommits origbranch adj = 
	preventCommits $ \commitsprevented ->
		join $ snd <$> propigateAdjustedCommits' origbranch adj commitsprevented
		
{- Returns sha of updated basis branch, and action which will rebase
 - the adjusted branch on top of the updated basis branch. -}
propigateAdjustedCommits'
	:: OrigBranch
	-> Adjustment
	-> CommitsPrevented
	-> Annex (Maybe Sha, Annex ())
propigateAdjustedCommits' origbranch adj _commitsprevented = do
	ov <- inRepo $ Git.Ref.sha basis
	case ov of
		Just origsha -> do
			cv <- catCommit currbranch
			case cv of
				Just currcommit -> do
					v <- newcommits >>= go origsha False
					case v of
						Left e -> do
							warning e
							return (Nothing, return ())
						Right newparent -> return
							( Just newparent
							, rebase currcommit newparent
							)
				Nothing -> return (Nothing, return ())
		Nothing -> return (Nothing, return ())
  where
	(BasisBranch basis) = basisBranch adjbranch
	adjbranch@(AdjBranch currbranch) = originalToAdjusted origbranch adj
	newcommits = inRepo $ Git.Branch.changedCommits basis currbranch
		-- Get commits oldest first, so they can be processed
		-- in order made.
		[Param "--reverse"]
	go parent _ [] = do
		setBasisBranch (BasisBranch basis) parent
		inRepo $ Git.Branch.update' origbranch parent
		return (Right parent)
	go parent pastadjcommit (sha:l) = do
		mc <- catCommit sha
		case mc of
			Just c
				| commitMessage c == adjustedBranchCommitMessage ->
					go parent True l
				| pastadjcommit -> do
					v <- reverseAdjustedCommit parent adj (sha, c) origbranch
					case v of
						Left e -> return (Left e)
						Right commit -> go commit pastadjcommit l
			_ -> go parent pastadjcommit l
	rebase currcommit newparent = do
		-- Reuse the current adjusted tree, and reparent it
		-- on top of the newparent.
		commitAdjustedTree (commitTree currcommit) (BasisBranch newparent)
			>>= inRepo . Git.Branch.update rebaseOnTopMsg currbranch

rebaseOnTopMsg :: String
rebaseOnTopMsg = "rebasing adjusted branch on top of updated original branch"

{- Reverses an adjusted commit, and commit with provided commitparent,
 - yielding a commit sha.
 -
 - Adjusts the tree of the commitparent, changing only the files that the
 - commit changed, and reverse adjusting those changes.
 -
 - The commit message, and the author and committer metadata are
 - copied over from the basiscommit. However, any gpg signature
 - will be lost, and any other headers are not copied either. -}
reverseAdjustedCommit :: Sha -> Adjustment -> (Sha, Commit) -> OrigBranch -> Annex (Either String Sha)
reverseAdjustedCommit commitparent adj (csha, basiscommit) origbranch
	| length (commitParent basiscommit) > 1 = return $
		Left $ "unable to propigate merge commit " ++ show csha ++ " back to " ++ show origbranch
	| otherwise = do
		treesha <- reverseAdjustedTree commitparent adj csha
		revadjcommit <- inRepo $ commitWithMetaData
			(commitAuthorMetaData basiscommit)
			(commitCommitterMetaData basiscommit) $
				Git.Branch.commitTree Git.Branch.AutomaticCommit
					(commitMessage basiscommit) [commitparent] treesha
		return (Right revadjcommit)

{- Adjusts the tree of the basis, changing only the files that the
 - commit changed, and reverse adjusting those changes.
 -
 - commitDiff does not support merge commits, so the csha must not be a
 - merge commit. -}
reverseAdjustedTree :: Sha -> Adjustment -> Sha -> Annex Sha
reverseAdjustedTree basis adj csha = do
	(diff, cleanup) <- inRepo (Git.DiffTree.commitDiff csha)
	let (adds, others) = partition (\dti -> Git.DiffTree.srcsha dti == nullSha) diff
	let (removes, changes) = partition (\dti -> Git.DiffTree.dstsha dti == nullSha) others
	adds' <- catMaybes <$>
		mapM (adjustTreeItem reverseadj) (map diffTreeToTreeItem adds)
	treesha <- Git.Tree.adjustTree
		(propchanges changes)
		adds'
		(map Git.DiffTree.file removes)
		basis
		=<< Annex.gitRepo
	void $ liftIO cleanup
	return treesha
  where
	reverseadj = reverseAdjustment adj
	propchanges changes ti@(TreeItem f _ _) =
		case M.lookup (norm f) m of
			Nothing -> return (Just ti) -- not changed
			Just change -> adjustTreeItem reverseadj change
	  where
		m = M.fromList $ map (\i@(TreeItem f' _ _) -> (norm f', i)) $
			map diffTreeToTreeItem changes
		norm = normalise . getTopFilePath

diffTreeToTreeItem :: Git.DiffTree.DiffTreeItem -> TreeItem
diffTreeToTreeItem dti = TreeItem
	(Git.DiffTree.file dti)
	(Git.DiffTree.dstmode dti)
	(Git.DiffTree.dstsha dti)

{- Cloning a repository that has an adjusted branch checked out will
 - result in the clone having the same adjusted branch checked out -- but
 - the origbranch won't exist in the clone, nor will the basis.
 - Create them. -}
checkAdjustedClone :: Annex ()
checkAdjustedClone = go =<< inRepo Git.Branch.current
  where
	go Nothing = return ()
	go (Just currbranch) = case adjustedToOriginal currbranch of
		Nothing -> return ()
		Just (adj, origbranch) -> do
			let remotebranch = Git.Ref.underBase "refs/remotes/origin" origbranch
			let basis@(BasisBranch bb) = basisBranch (originalToAdjusted origbranch adj)
			unlessM (inRepo $ Git.Ref.exists bb) $
				setBasisBranch basis remotebranch
			unlessM (inRepo $ Git.Ref.exists origbranch) $
				inRepo $ Git.Branch.update' origbranch remotebranch

-- git 2.2.0 needed for GIT_COMMON_DIR which is needed
-- by updateAdjustedBranch to use withWorkTreeRelated.
isGitVersionSupported :: IO Bool
isGitVersionSupported = not <$> Git.Version.older "2.2.0"

checkVersionSupported :: Annex ()
checkVersionSupported = do
	unlessM versionSupportsAdjustedBranch $
		error "Adjusted branches are only supported in v6 or newer repositories."
	unlessM (liftIO isGitVersionSupported) $
		error "Your version of git is too old; upgrade it to 2.2.0 or newer to use adjusted branches."
