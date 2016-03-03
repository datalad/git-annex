{- adjusted version of main branch
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.AdjustedBranch (
	Adjustment(..),
	OrigBranch,
	AdjBranch,
	originalToAdjusted,
	adjustedToOriginal,
	fromAdjustedBranch,
	enterAdjustedBranch,
	updateAdjustedBranch,
	propigateAdjustedCommits,
) where

import Annex.Common
import qualified Annex
import Git
import Git.Types
import qualified Git.Branch
import qualified Git.Ref
import qualified Git.Command
import qualified Git.Tree
import Git.Tree (TreeItem(..))
import Git.Env
import Git.Index
import Git.FilePath
import qualified Git.LockFile
import Annex.CatFile
import Annex.Link
import Git.HashObject
import Annex.AutoMerge
import qualified Database.Keys

data Adjustment = UnlockAdjustment
	deriving (Show)

data Direction = Forward | Reverse

adjustTreeItem :: Adjustment -> Direction -> HashObjectHandle -> TreeItem -> Annex (Maybe TreeItem)
adjustTreeItem UnlockAdjustment Forward h ti@(TreeItem f m s)
	| toBlobType m == Just SymlinkBlob = do
		mk <- catKey s
		case mk of
			Just k -> do
				Database.Keys.addAssociatedFile k f
				Just . TreeItem f (fromBlobType FileBlob)
					<$> hashPointerFile' h k
			Nothing -> return (Just ti)
	| otherwise = return (Just ti)
adjustTreeItem UnlockAdjustment Reverse h ti@(TreeItem f m s)
	-- XXX does not remember when files were originally unlocked; locks
	-- everything
	| toBlobType m /= Just SymlinkBlob = do
		mk <- catKey s
		case mk of
			Just k -> do
				absf <- inRepo $ \r -> absPath $
					fromTopFilePath f r
				linktarget <- calcRepo $ gitAnnexLink absf k
				Just . TreeItem f (fromBlobType SymlinkBlob)
					<$> hashSymlink' h linktarget
			Nothing -> return (Just ti)
	| otherwise = return (Just ti)

type OrigBranch = Branch
type AdjBranch = Branch

adjustedBranchPrefix :: String
adjustedBranchPrefix = "refs/heads/adjusted/"

serialize :: Adjustment -> String
serialize UnlockAdjustment = "unlocked"

deserialize :: String -> Maybe Adjustment
deserialize "unlocked" = Just UnlockAdjustment
deserialize _ = Nothing

originalToAdjusted :: OrigBranch -> Adjustment -> AdjBranch
originalToAdjusted orig adj = Ref $
	adjustedBranchPrefix ++ base ++ '(' : serialize adj ++ ")"
  where
	base = fromRef (Git.Ref.basename orig)

adjustedToOriginal :: AdjBranch -> Maybe (Adjustment, OrigBranch)
adjustedToOriginal b
	| adjustedBranchPrefix `isPrefixOf` bs = do
		let (base, as) = separate (== '(') (drop prefixlen bs)
		adj <- deserialize (takeWhile (/= ')') as)
		Just (adj, Git.Ref.under "refs/heads" (Ref base))
	| otherwise = Nothing
  where
	bs = fromRef b
	prefixlen = length adjustedBranchPrefix

fromAdjustedBranch :: Branch -> OrigBranch
fromAdjustedBranch b = maybe b snd (adjustedToOriginal b)

originalBranch :: Annex (Maybe OrigBranch)
originalBranch = fmap fromAdjustedBranch <$> inRepo Git.Branch.current

{- Enter an adjusted version of current branch (or, if already in an
 - adjusted version of a branch, changes the adjustment of the original
 - branch).
 -
 - Can fail, if no branch is checked out, or perhaps if staged changes
 - conflict with the adjusted branch.
 -}
enterAdjustedBranch :: Adjustment -> Annex ()
enterAdjustedBranch adj = go =<< originalBranch
  where
	go (Just origbranch) = do
		adjbranch <- preventCommits $ adjustBranch adj Forward origbranch
		inRepo $ Git.Command.run
			[ Param "checkout"
			, Param $ fromRef $ Git.Ref.base $ adjbranch
			]
	go Nothing = error "not on any branch!"

adjustBranch :: Adjustment -> Direction -> OrigBranch -> Annex AdjBranch
adjustBranch adj direction origbranch = do
	sha <- adjust adj direction origbranch
	inRepo $ Git.Branch.update adjbranch sha
	return adjbranch
  where
	adjbranch = originalToAdjusted origbranch adj

adjust :: Adjustment -> Direction -> Ref -> Annex Sha
adjust adj direction orig = do
	treesha <- adjustTree adj direction orig
	commitAdjustedTree treesha orig

adjustTree :: Adjustment -> Direction -> Ref -> Annex Sha
adjustTree adj direction orig = do
	h <- inRepo hashObjectStart
	treesha <- Git.Tree.adjustTree (adjustTreeItem adj direction h) orig
		=<< Annex.gitRepo
	liftIO $ hashObjectStop h
	return treesha

{- Locks git's index file, preventing git from making a commit, merge, 
 - or otherwise changing the HEAD ref while the action is run.
 -
 - Throws an IO exception if the index file is already locked.
 -}
preventCommits :: Annex a -> Annex a
preventCommits = bracket setup cleanup . const
  where
	setup = do
		lck <- fromRepo indexFileLock
		liftIO $ Git.LockFile.openLock lck
	cleanup lckhandle = liftIO $ Git.LockFile.closeLock lckhandle

{- Commits a given adjusted tree, with the provided parent ref.
 -
 - This should always yield the same value, even if performed in different 
 - clones of a repo, at different times. The commit message and other
 - metadata is based on the parent.
 -}
commitAdjustedTree :: Sha -> Ref -> Annex Sha
commitAdjustedTree treesha parent = go =<< catCommit parent
  where
	go Nothing = inRepo mkcommit
	go (Just parentcommit) = inRepo $ commitWithMetaData
		(commitAuthorMetaData parentcommit)
		(commitCommitterMetaData parentcommit)
		mkcommit
	mkcommit = Git.Branch.commitTree Git.Branch.AutomaticCommit
		adjustedBranchCommitMessage [parent] treesha

adjustedBranchCommitMessage :: String
adjustedBranchCommitMessage = "git-annex adjusted branch"

{- Update the currently checked out adjusted branch, merging the provided
 - branch into it. -}
updateAdjustedBranch :: Branch -> (OrigBranch, Adjustment) -> Git.Branch.CommitMode -> Annex Bool
updateAdjustedBranch tomerge (origbranch, adj) commitmode = 
	catchBoolIO $ preventCommits $ go =<< (,)
		<$> inRepo (Git.Ref.sha tomerge)
		<*> inRepo Git.Branch.current
  where
	go (Just mergesha, Just currbranch) = ifM (inRepo $ Git.Branch.changed currbranch mergesha)
		( do
			propigateAdjustedCommits origbranch (adj, currbranch)
			adjustedtomerge <- adjust adj Forward mergesha
			ifM (inRepo $ Git.Branch.changed currbranch adjustedtomerge)
				( ifM (autoMergeFrom adjustedtomerge (Just currbranch) commitmode)
					( recommit currbranch mergesha =<< catCommit currbranch
					, return False
					)
				, return True -- no changes to merge
				)
		, return True -- no changes to merge
		)
	go _ = return False
	{- Once a merge commit has been made, re-do it, removing
	 - the old version of the adjusted branch as a parent, and
	 - making the only parent be the branch that was merged in.
	 -
	 - Doing this ensures that the same commit Sha is
	 - always arrived at for a given commit from the merged in branch.
	 -}
	recommit currbranch parent (Just commit) = do
		commitsha <- commitAdjustedTree (commitTree commit) parent
		inRepo $ Git.Branch.update currbranch commitsha
		propigateAdjustedCommits origbranch (adj, currbranch)
		return True
	recommit _ _ Nothing = return False

{- Check for any commits present on the adjusted branch that have not yet
 - been propigated to the orig branch, and propigate them.
 -
 - After propigating the commits back to the orig banch,
 - rebase the adjusted branch on top of the updated orig branch.
 -}
propigateAdjustedCommits :: OrigBranch -> (Adjustment, AdjBranch) -> Annex ()
propigateAdjustedCommits origbranch (adj, currbranch) = do
	v <- inRepo $ Git.Ref.sha (Git.Ref.under "refs/heads" origbranch)
	case v of
		Just origsha -> preventCommits $ 
			go origsha False =<< newcommits
		Nothing -> return ()
  where
	newcommits = inRepo $ Git.Branch.changedCommits origbranch currbranch
		-- Get commits oldest first, so they can be processed
		-- in order made.
		[Param "--reverse"]
	go newhead _ [] = do
		inRepo $ Git.Branch.update origbranch newhead
		-- TODO rebase adjusted branch
	go parent pastadjcommit (sha:l) = do
		mc <- catCommit sha
		case mc of
			Just c
				| commitMessage c == adjustedBranchCommitMessage ->
					go parent True l
				| pastadjcommit -> do
					commit <- reverseAdjustedCommit parent adj c
					go commit pastadjcommit l
			_ -> go parent pastadjcommit l

{- Reverses an adjusted commit, yielding a commit sha.
 -
 - Note that the commit message, and the author and committer metadata are
 - copied over. However, any gpg signature will be lost, and any other
 - headers are not copied either. -}
reverseAdjustedCommit :: Sha -> Adjustment -> Commit -> Annex Sha
reverseAdjustedCommit parent adj c = do
	treesha <- adjustTree adj Reverse (commitTree c)
	inRepo $ commitWithMetaData
		(commitAuthorMetaData c)
		(commitCommitterMetaData c) $
		Git.Branch.commitTree Git.Branch.AutomaticCommit
			(commitMessage c) [parent] treesha
