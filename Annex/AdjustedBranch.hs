{- adjusted branch
 -
 - Copyright 2016-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Annex.AdjustedBranch (
	Adjustment(..),
	LinkAdjustment(..),
	PresenceAdjustment(..),
	LockUnlockPresentAdjustment(..),
	adjustmentHidesFiles,
	adjustmentIsStable,
	OrigBranch,
	AdjBranch(..),
	originalToAdjusted,
	adjustedToOriginal,
	fromAdjustedBranch,
	getAdjustment,
	enterAdjustedBranch,
	adjustedBranchRefresh,
	adjustedBranchRefreshFull,
	adjustBranch,
	adjustTree,
	adjustToCrippledFileSystem,
	commitForAdjustedBranch,
	propigateAdjustedCommits,
	propigateAdjustedCommits',
	commitAdjustedTree,
	commitAdjustedTree',
	BasisBranch(..),
	basisBranch,
	setBasisBranch,
	preventCommits,
	AdjustedClone(..),
	checkAdjustedClone,
	checkVersionSupported,
	isGitVersionSupported,
) where

import Annex.Common
import Types.AdjustedBranch
import Annex.AdjustedBranch.Name
import qualified Annex
import Git
import Git.Types
import qualified Git.Branch
import qualified Git.Ref
import qualified Git.Command
import qualified Git.Tree
import qualified Git.DiffTree
import Git.Tree (TreeItem(..))
import Git.Sha
import Git.Env
import Git.Index
import Git.FilePath
import qualified Git.LockFile
import qualified Git.Version
import Annex.CatFile
import Annex.Link
import Annex.Content.Presence
import Annex.CurrentBranch
import Types.CleanupActions
import qualified Database.Keys
import Config
import Logs.View (is_branchView)
import Logs.AdjustedBranchUpdate
import Utility.FileMode
import qualified Utility.RawFilePath as R
import qualified Utility.FileIO as F

import Data.Time.Clock.POSIX
import qualified Data.Map as M
import System.PosixCompat.Files (fileMode)

class AdjustTreeItem t where
	-- How to perform various adjustments to a TreeItem.
	adjustTreeItem :: t -> TreeItem -> Annex (Maybe TreeItem)
	-- Will adjusting a given tree always yield the same adjusted tree?
	adjustmentIsStable :: t -> Bool

instance AdjustTreeItem Adjustment where
	adjustTreeItem (LinkAdjustment l) t = adjustTreeItem l t
	adjustTreeItem (PresenceAdjustment p Nothing) t = adjustTreeItem p t
	adjustTreeItem (PresenceAdjustment p (Just l)) t =
		adjustTreeItem p t >>= \case
			Nothing -> return Nothing
			Just t' -> adjustTreeItem l t'
	adjustTreeItem (LockUnlockPresentAdjustment l) t = adjustTreeItem l t

	adjustmentIsStable (LinkAdjustment l) = adjustmentIsStable l
	adjustmentIsStable (PresenceAdjustment p _) = adjustmentIsStable p
	adjustmentIsStable (LockUnlockPresentAdjustment l) = adjustmentIsStable l

instance AdjustTreeItem LinkAdjustment where
	adjustTreeItem UnlockAdjustment =
		ifSymlink adjustToPointer noAdjust
	adjustTreeItem LockAdjustment =
		ifSymlink noAdjust adjustToSymlink
	adjustTreeItem FixAdjustment =
		ifSymlink adjustToSymlink noAdjust
	adjustTreeItem UnFixAdjustment =
		ifSymlink (adjustToSymlink' gitAnnexLinkCanonical) noAdjust
	
	adjustmentIsStable _ = True

instance AdjustTreeItem PresenceAdjustment where
	adjustTreeItem HideMissingAdjustment = 
		ifPresent noAdjust hideAdjust
	adjustTreeItem ShowMissingAdjustment =
		noAdjust

	adjustmentIsStable HideMissingAdjustment = False
	adjustmentIsStable ShowMissingAdjustment = True

instance AdjustTreeItem LockUnlockPresentAdjustment where
	adjustTreeItem UnlockPresentAdjustment = 
		ifPresent adjustToPointer adjustToSymlink
	adjustTreeItem LockPresentAdjustment =
		-- Turn all pointers back to symlinks, whether the content
		-- is present or not. This is done because the content
		-- availability may have changed and the branch not been
		-- re-adjusted to keep up, so there may be pointers whose
		-- content is not present.
		ifSymlink noAdjust adjustToSymlink

	adjustmentIsStable UnlockPresentAdjustment = False
	adjustmentIsStable LockPresentAdjustment = True

ifSymlink
	:: (TreeItem -> Annex a)
	-> (TreeItem -> Annex a)
	-> TreeItem
	-> Annex a
ifSymlink issymlink notsymlink ti@(TreeItem _f m _s)
	| toTreeItemType m == Just TreeSymlink = issymlink ti
	| otherwise = notsymlink ti

ifPresent
	:: (TreeItem -> Annex (Maybe TreeItem))
	-> (TreeItem -> Annex (Maybe TreeItem))
	-> TreeItem
	-> Annex (Maybe TreeItem)
ifPresent ispresent notpresent ti@(TreeItem _ _ s) =
	catKey s >>= \case
		Just k -> ifM (inAnnex k) (ispresent ti, notpresent ti)
		Nothing -> return (Just ti)

noAdjust :: TreeItem -> Annex (Maybe TreeItem)
noAdjust = return . Just

hideAdjust :: TreeItem -> Annex (Maybe TreeItem)
hideAdjust _ = return Nothing

adjustToPointer :: TreeItem -> Annex (Maybe TreeItem)
adjustToPointer ti@(TreeItem f _m s) = catKey s >>= \case
	Just k -> do
		Database.Keys.addAssociatedFile k f
		exe <- catchDefaultIO False $
			(isExecutable . fileMode) <$> 
				(liftIO . R.getFileStatus . fromOsPath
					=<< calcRepo (gitAnnexLocation k))
		let mode = fromTreeItemType $ 
			if exe then TreeExecutable else TreeFile
		Just . TreeItem f mode <$> hashPointerFile k
	Nothing -> return (Just ti)

adjustToSymlink :: TreeItem -> Annex (Maybe TreeItem)
adjustToSymlink = adjustToSymlink' gitAnnexLink

adjustToSymlink' :: (OsPath -> Key -> Git.Repo -> GitConfig -> IO OsPath) -> TreeItem -> Annex (Maybe TreeItem)
adjustToSymlink' gitannexlink ti@(TreeItem f _m s) = catKey s >>= \case
	Just k -> do
		absf <- inRepo $ \r -> absPath $ fromTopFilePath f r
		linktarget <- calcRepo $ gitannexlink absf k
		Just . TreeItem f (fromTreeItemType TreeSymlink)
			<$> hashSymlink (fromOsPath linktarget)
	Nothing -> return (Just ti)

-- This is a hidden branch ref, that's used as the basis for the AdjBranch,
-- since pushes can overwrite the OrigBranch at any time. So, changes
-- are propagated from the AdjBranch to the head of the BasisBranch.
newtype BasisBranch = BasisBranch Ref

-- The basis for refs/heads/adjusted/master(unlocked) is
-- refs/basis/adjusted/master(unlocked).
basisBranch :: AdjBranch -> BasisBranch
basisBranch (AdjBranch adjbranch) = BasisBranch $
	Ref ("refs/basis/" <> fromRef' (Git.Ref.base adjbranch))

getAdjustment :: Branch -> Maybe Adjustment
getAdjustment = fmap fst . adjustedToOriginal

fromAdjustedBranch :: Branch -> OrigBranch
fromAdjustedBranch b = maybe b snd (adjustedToOriginal b)

{- Enter an adjusted version of current branch (or, if already in an
 - adjusted version of a branch, changes the adjustment of the original
 - branch).
 -
 - Can fail, if no branch is checked out, or if the adjusted branch already
 - exists, or if staged changes prevent a checkout.
 -}
enterAdjustedBranch :: Adjustment -> Annex Bool
enterAdjustedBranch adj = inRepo Git.Branch.current >>= \case
	Just currbranch -> case getAdjustment currbranch of
		Just curradj | curradj == adj ->
			updateAdjustedBranch adj (AdjBranch currbranch)
				(fromAdjustedBranch currbranch)
		_ -> go currbranch
	Nothing -> do
		warning "not on any branch!"
		return False
  where
	go currbranch = do
		let origbranch = fromAdjustedBranch currbranch
		let adjbranch = adjBranch $ originalToAdjusted origbranch adj
		ifM (inRepo (Git.Ref.exists adjbranch) <&&> (not <$> Annex.getRead Annex.force) <&&> pure (not (is_branchView origbranch)))
			( do
				mapM_ (warning . UnquotedString . unwords)
					[ [ "adjusted branch"
					  , Git.Ref.describe adjbranch
					  , "already exists."
					  ]
					, [ "Aborting because that branch may have changes that have not yet reached"
					  , Git.Ref.describe origbranch
					  ]
					, [ "You can check out the adjusted branch manually to enter it,"
					  , "or add the --force option to overwrite the old branch."
					  ]
					]
				return False
			, do
				starttime <- liftIO getPOSIXTime
				b <- preventCommits $ const $ 
					adjustBranch adj origbranch
				ok <- checkoutAdjustedBranch b False
				when ok $
					recordAdjustedBranchUpdateFinished starttime
				return ok
			)

checkoutAdjustedBranch :: AdjBranch -> Bool -> Annex Bool
checkoutAdjustedBranch (AdjBranch b) quietcheckout = do
	-- checkout can have output in large repos
	unless quietcheckout
		showOutput
	inRepo $ Git.Command.runBool $
		[ Param "checkout"
		, Param $ fromRef $ Git.Ref.base b
		, if quietcheckout then Param "--quiet" else Param "--progress"
		]

{- Already in a branch with this adjustment, but the user asked to enter it
 - again. This should have the same result as propagating any commits
 - back to the original branch, checking out the original branch, deleting
 - and rebuilding the adjusted branch, and then checking it out.
 - But, it can be implemented more efficiently than that.
 -}
updateAdjustedBranch :: Adjustment -> AdjBranch -> OrigBranch -> Annex Bool
updateAdjustedBranch adj (AdjBranch currbranch) origbranch
	| not (adjustmentIsStable adj) = do
		(b, origheadfile, newheadfile) <- preventCommits $ \commitlck -> do
			-- Avoid losing any commits that the adjusted branch
			-- has that have not yet been propagated back to the
			-- origbranch.
			_ <- propigateAdjustedCommits' True origbranch adj commitlck
			
			origheadfile <- inRepo $ F.readFile' . Git.Ref.headFile
			origheadsha <- inRepo (Git.Ref.sha currbranch)
			
			b <- adjustBranch adj origbranch

			-- Git normally won't do anything when asked to check
			-- out the currently checked out branch, even when its
			-- ref has changed. Work around this by writing a raw
			-- sha to .git/HEAD.
			newheadfile <- case origheadsha of
				Just s -> do
					inRepo $ \r -> do
						let newheadfile = fromRef' s
						F.writeFile' (Git.Ref.headFile r) newheadfile
						return (Just newheadfile)
				_ -> return Nothing
	
			return (b, origheadfile, newheadfile)
	
		-- Make git checkout quiet to avoid warnings about
		-- disconnected branch tips being lost.
		ok <- checkoutAdjustedBranch b True

		-- Avoid leaving repo with detached head.
		unless ok $ case newheadfile of
			Nothing -> noop
			Just v -> preventCommits $ \_commitlck -> inRepo $ \r -> do
				v' <- F.readFile' (Git.Ref.headFile r)
				when (v == v') $
					F.writeFile' (Git.Ref.headFile r) origheadfile

		return ok
	| otherwise = preventCommits $ \commitlck -> do
		-- Done for consistency.
		_ <- propigateAdjustedCommits' True origbranch adj commitlck
		-- No need to actually update the branch because the
		-- adjustment is stable.
		return True

{- Passed an action that, if it succeeds may get or drop a key.
 - When the adjusted branch needs to be refreshed to reflect
 - those changes, it's handled here.
 -}
adjustedBranchRefresh :: Annex a -> Annex a
adjustedBranchRefresh a = do
	r <- a
	go
	return r
  where
	go = getCurrentBranch >>= \case
		(Just origbranch, Just adj) ->
			unless (adjustmentIsStable adj) $ do
				recordAdjustedBranchUpdateNeeded
				n <- annexAdjustedBranchRefresh <$> Annex.getGitConfig
				unless (n == 0) $ ifM (checkcounter n)
					-- This is slow, it would be better to incrementally
					-- adjust the AssociatedFile, and only call this once
					-- at shutdown to handle cases where not all
					-- AssociatedFiles are known.
					( adjustedBranchRefreshFull' adj origbranch
					, Annex.addCleanupAction AdjustedBranchUpdate $
						adjustedBranchRefreshFull' adj origbranch
					)
		_ -> return ()
	
	checkcounter n
		-- Special case, 1 (or true) refreshes only at shutdown.
		| n == 1 = pure False
		| otherwise = Annex.withState $ \s -> 
			let !c = Annex.adjustedbranchrefreshcounter s + 1
			    !enough = c >= pred n
			    !c' = if enough then 0 else c
			    !s' = s { Annex.adjustedbranchrefreshcounter = c' }
			    in pure (s', enough)

{- Slow, but more dependable version of adjustedBranchRefresh that
 - does not rely on all AssociatedFiles being known. -}
adjustedBranchRefreshFull :: Adjustment -> OrigBranch -> Annex ()
adjustedBranchRefreshFull adj origbranch =
	whenM isAdjustedBranchUpdateNeeded $ do
		adjustedBranchRefreshFull' adj origbranch

adjustedBranchRefreshFull' :: Adjustment -> OrigBranch -> Annex ()
adjustedBranchRefreshFull' adj origbranch = do
	-- Restage pointer files so modifications to them due to get/drop
	-- do not prevent checking out the updated adjusted branch.
	restagePointerFiles =<< Annex.gitRepo
	starttime <- liftIO getPOSIXTime
	let adjbranch = originalToAdjusted origbranch adj
	ifM (updateAdjustedBranch adj adjbranch origbranch)
		( recordAdjustedBranchUpdateFinished starttime
		, warning "Updating adjusted branch failed."
		)

adjustToCrippledFileSystem :: Annex ()
adjustToCrippledFileSystem = do
	warning "Entering an adjusted branch where files are unlocked as this filesystem does not support locked files."
	checkVersionSupported
	whenM (isNothing <$> inRepo Git.Branch.current) $
		commitForAdjustedBranch []
	inRepo Git.Branch.current >>= \case
		Just currbranch -> case getAdjustment currbranch of
			Just curradj | curradj == adj -> return ()
			_ -> do
				let adjbranch = originalToAdjusted currbranch adj
				ifM (inRepo (Git.Ref.exists $ adjBranch adjbranch))
					( unlessM (checkoutAdjustedBranch adjbranch False) $
						failedenter
					, unlessM (enterAdjustedBranch adj) $
						failedenter
					)
		Nothing -> failedenter
  where
	adj = LinkAdjustment UnlockAdjustment
	failedenter = warning "Failed to enter adjusted branch!"

{- Commit before entering adjusted branch. Only needs to be done
 - when the current branch does not have any commits yet.
 -
 - If something is already staged, it will be committed, but otherwise
 - an empty commit will be made.
 -}
commitForAdjustedBranch :: [CommandParam] -> Annex ()
commitForAdjustedBranch ps = do
	cmode <- annexCommitMode <$> Annex.getGitConfig
	let cquiet = Git.Branch.CommitQuiet True
	void $ inRepo $ Git.Branch.commitCommand cmode cquiet $
		[ Param "--allow-empty"
		, Param "-m"
		, Param "commit before entering adjusted branch"
		] ++ ps

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
	treesha <- Git.Tree.adjustTree
		toadj 
		[] 
		(\_old new -> new)
		[]
		basis =<< Annex.gitRepo
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
		lck <- fromRepo $ indexFileLock . indexFile
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
	go Nothing = do
		cmode <- annexCommitMode <$> Annex.getGitConfig
		inRepo $ mkcommit cmode
	go (Just basiscommit) = do
		cmode <- annexCommitMode <$> Annex.getGitConfig
		inRepo $ commitWithMetaData
			(commitAuthorMetaData basiscommit)
			(commitCommitterMetaData basiscommit)
			(mkcommit cmode)
	-- Make sure that the exact message is used in the commit,
	-- since that message is looked for later.
	-- After git-annex 10.20240227, it's possible to use
	-- commitTree instead of this, but this is being kept
	-- for some time, for compatibility with older versions.
	mkcommit cmode = Git.Branch.commitTreeExactMessage cmode
		adjustedBranchCommitMessage parents treesha

{- This message should never be changed. -}
adjustedBranchCommitMessage :: String
adjustedBranchCommitMessage = "git-annex adjusted branch"

{- Allow for a trailing newline after the message. -}
hasAdjustedBranchCommitMessage :: Commit -> Bool
hasAdjustedBranchCommitMessage c = 
	dropWhileEnd (\x -> x == '\n' || x == '\r') (commitMessage c) 
		== adjustedBranchCommitMessage

findAdjustingCommit :: AdjBranch -> Annex (Maybe Commit)
findAdjustingCommit (AdjBranch b) = go =<< catCommit b
  where
	go Nothing = return Nothing
	go (Just c)
		| hasAdjustedBranchCommitMessage c = return (Just c)
		| otherwise = case commitParent c of
			[p] -> go =<< catCommit p
			_ -> return Nothing

{- Check for any commits present on the adjusted branch that have not yet
 - been propagated to the basis branch, and propagate them to the basis
 - branch and from there on to the orig branch.
 -
 - After propagating the commits back to the basis branch,
 - rebase the adjusted branch on top of the updated basis branch.
 -}
propigateAdjustedCommits :: OrigBranch -> Adjustment -> Annex ()
propigateAdjustedCommits origbranch adj = 
	preventCommits $ \commitsprevented ->
		join $ snd <$> propigateAdjustedCommits' True origbranch adj commitsprevented
		
{- Returns sha of updated basis branch, and action which will rebase
 - the adjusted branch on top of the updated basis branch. -}
propigateAdjustedCommits'
	:: Bool
	-> OrigBranch
	-> Adjustment
	-> CommitsPrevented
	-> Annex (Maybe Sha, Annex ())
propigateAdjustedCommits' warnwhendiverged origbranch adj _commitsprevented =
	inRepo (Git.Ref.sha basis) >>= \case
		Just origsha -> catCommit currbranch >>= \case
			Just currcommit -> do
				newparent <- newcommits >>= go origsha origsha False
				return
					( Just newparent
					, rebase currcommit newparent
					)
			Nothing -> return (Nothing, return ())
		Nothing -> do
			warning $ UnquotedString $ 
				"Cannot find basis ref " ++ fromRef basis ++ "; not propagating adjusted commits to original branch " ++ fromRef origbranch
			return (Nothing, return ())
  where
	(BasisBranch basis) = basisBranch adjbranch
	adjbranch@(AdjBranch currbranch) = originalToAdjusted origbranch adj
	newcommits = inRepo $ Git.Branch.changedCommits basis currbranch
		-- Get commits oldest first, so they can be processed
		-- in order made.
		[Param "--reverse"]
	go origsha parent _ [] = do
		setBasisBranch (BasisBranch basis) parent
		inRepo (Git.Ref.sha origbranch) >>= \case
			Just origbranchsha | origbranchsha /= origsha ->
				when warnwhendiverged $
					warning $ UnquotedString $ 
						"Original branch " ++ fromRef origbranch ++ " has diverged from current adjusted branch " ++ fromRef currbranch
			_ -> inRepo $ Git.Branch.update' origbranch parent
		return parent
	go origsha parent pastadjcommit (sha:l) = catCommit sha >>= \case
		Just c
			| hasAdjustedBranchCommitMessage c ->
				go origsha parent True l
			| pastadjcommit -> do
				commit <- reverseAdjustedCommit parent adj (sha, c) origbranch
				go origsha commit pastadjcommit l
		_ -> go origsha parent pastadjcommit l
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
reverseAdjustedCommit :: Sha -> Adjustment -> (Sha, Commit) -> OrigBranch -> Annex Sha
reverseAdjustedCommit commitparent adj (csha, basiscommit) origbranch
	| length (commitParent basiscommit) > 1 = giveup $
		"unable to propagate merge commit " ++ show csha ++ " back to " ++ show origbranch
	| otherwise = do
		cmode <- annexCommitMode <$> Annex.getGitConfig
		treesha <- reverseAdjustedTree commitparent adj csha
		revadjcommit <- inRepo $ commitWithMetaData
			(commitAuthorMetaData basiscommit)
			(commitCommitterMetaData basiscommit) $
				Git.Branch.commitTree cmode
					[commitMessage basiscommit]
					[commitparent] treesha
		return revadjcommit

{- Adjusts the tree of the basis, changing only the files that the
 - commit changed, and reverse adjusting those changes.
 -
 - commitDiff does not support merge commits, so the csha must not be a
 - merge commit. -}
reverseAdjustedTree :: Sha -> Adjustment -> Sha -> Annex Sha
reverseAdjustedTree basis adj csha = do
	(diff, cleanup) <- inRepo (Git.DiffTree.commitDiff csha)
	let (adds, others) = partition (\dti -> Git.DiffTree.srcsha dti `elem` nullShas) diff
	let (removes, changes) = partition (\dti -> Git.DiffTree.dstsha dti `elem` nullShas) others
	adds' <- catMaybes <$>
		mapM (adjustTreeItem reverseadj) (map diffTreeToTreeItem adds)
	treesha <- Git.Tree.adjustTree
		(propchanges changes)
		adds'
		(\_old new -> new)
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

data AdjustedClone = InAdjustedClone | NotInAdjustedClone

{- Cloning a repository that has an adjusted branch checked out will
 - result in the clone having the same adjusted branch checked out -- but
 - the origbranch won't exist in the clone, nor will the basis. So
 - to properly set up the adjusted branch, the origbranch and basis need
 - to be set.
 - 
 - We can't trust that the origin's origbranch matches up with the currently
 - checked out adjusted branch; the origin could have the two branches
 - out of sync (eg, due to another branch having been pushed to the origin's
 - origbranch), or due to a commit on its adjusted branch not having been
 - propagated back to origbranch.
 -
 - So, find the adjusting commit on the currently checked out adjusted
 - branch, and use the parent of that commit as the basis, and set the
 - origbranch to it.
 -}
checkAdjustedClone :: Annex AdjustedClone
checkAdjustedClone = ifM isBareRepo
	( return NotInAdjustedClone
	, go =<< inRepo Git.Branch.current
	)
  where
	go Nothing = return NotInAdjustedClone
	go (Just currbranch) = case adjustedToOriginal currbranch of
		Nothing -> return NotInAdjustedClone
		Just (adj, origbranch) -> do
			let basis@(BasisBranch bb) = basisBranch (originalToAdjusted origbranch adj)
			unlessM (inRepo $ Git.Ref.exists bb) $ do
				aps <- fmap commitParent <$> findAdjustingCommit (AdjBranch currbranch)
				case aps of
					Just [p] -> do
						unlessM (inRepo $ Git.Ref.exists origbranch) $
							inRepo $ Git.Branch.update' origbranch p
						setBasisBranch basis p
					_ -> giveup $ "Unable to clean up from clone of adjusted branch; perhaps you should check out " ++ Git.Ref.describe origbranch
			return InAdjustedClone

checkVersionSupported :: Annex ()
checkVersionSupported =
	unlessM (liftIO isGitVersionSupported) $
		giveup "Your version of git is too old; upgrade it to 2.2.0 or newer to use adjusted branches."

-- git 2.2.0 needed for GIT_COMMON_DIR which is needed
-- by updateAdjustedBranch to use withWorkTreeRelated.
isGitVersionSupported :: IO Bool
isGitVersionSupported = not <$> Git.Version.older "2.2.0"
