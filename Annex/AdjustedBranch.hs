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
	adjustedToOriginal,
	fromAdjustedBranch,
	enterAdjustedBranch,
	updateAdjustedBranch,
	propigateAdjustedCommits,
) where

import Annex.Common
import qualified Annex
import Git.Types
import qualified Git.Branch
import qualified Git.Ref
import qualified Git.Command
import Git.Tree
import Git.Env
import Git.Index
import qualified Git.LockFile
import Annex.CatFile
import Annex.Link
import Git.HashObject
import Annex.AutoMerge
import qualified Database.Keys

data Adjustment = UnlockAdjustment
	deriving (Show)

adjustTreeItem :: Adjustment -> HashObjectHandle -> TreeItem -> Annex (Maybe TreeItem)
adjustTreeItem UnlockAdjustment h ti@(TreeItem f m s)
	| toBlobType m == Just SymlinkBlob = do
		mk <- catKey s
		case mk of
			Just k -> do
				Database.Keys.addAssociatedFile k f
				Just . TreeItem f (fromBlobType FileBlob)
					<$> hashPointerFile' h k
			Nothing -> return (Just ti)
	| otherwise = return (Just ti)

type OrigBranch = Branch
type AdjBranch = Branch

adjustedBranchPrefix :: String
adjustedBranchPrefix = "refs/heads/adjusted/"

serialize :: Adjustment -> String
serialize UnlockAdjustment = "unlock"

deserialize :: String -> Maybe Adjustment
deserialize "unlock" = Just UnlockAdjustment
deserialize _ = Nothing

originalToAdjusted :: OrigBranch -> Adjustment -> AdjBranch
originalToAdjusted orig adj = Git.Ref.under base orig
  where
	base = adjustedBranchPrefix ++ serialize adj

adjustedToOriginal :: AdjBranch -> Maybe (Adjustment, OrigBranch)
adjustedToOriginal b
	| adjustedBranchPrefix `isPrefixOf` bs = do
		adj <- deserialize (takeWhile (/= '/') (drop prefixlen bs))
		Just (adj, Git.Ref.basename b)
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
	go (Just origbranch) = preventCommits $ do
		adjbranch <- adjustBranch adj origbranch
		inRepo $ Git.Command.run
			[ Param "checkout"
			, Param $ fromRef $ Git.Ref.base $ adjbranch
			]
	go Nothing = error "not on any branch!"

adjustBranch :: Adjustment -> OrigBranch -> Annex AdjBranch
adjustBranch adj origbranch = do
	sha <- adjust adj origbranch
	inRepo $ Git.Branch.update adjbranch sha
	return adjbranch
  where
	adjbranch = originalToAdjusted origbranch adj

adjust :: Adjustment -> Ref -> Annex Sha
adjust adj orig = do
	h <- inRepo hashObjectStart
	treesha <- adjustTree (adjustTreeItem adj h) orig =<< Annex.gitRepo
	liftIO $ hashObjectStop h
	commitAdjustedTree treesha orig

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
	mkcommit = Git.Branch.commitTree
		Git.Branch.AutomaticCommit "adjusted branch" [parent] treesha

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
			propigateAdjustedCommits origbranch adj
			adjustedtomerge <- adjust adj mergesha
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
		propigateAdjustedCommits origbranch adj
		return True
	recommit _ _ Nothing = return False

{- Check for any commits present on the adjusted branch that have not yet
 - been propigated to the orig branch, and propigate them. -}
propigateAdjustedCommits :: OrigBranch -> Adjustment -> Annex ()
propigateAdjustedCommits originbranch adj = return () -- TODO
