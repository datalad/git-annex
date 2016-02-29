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
	enterAdjustedBranch,
	updateAdjustedBranch,
) where

import Annex.Common
import qualified Annex
import Git.Types
import qualified Git.Branch
import qualified Git.Ref
import qualified Git.Command
import Git.Tree
import Git.Env
import Annex.CatFile
import Annex.Link
import Git.HashObject

data Adjustment = UnlockAdjustment
	deriving (Show)

adjustTreeItem :: Adjustment -> HashObjectHandle -> TreeItem -> Annex (Maybe TreeItem)
adjustTreeItem UnlockAdjustment h ti@(TreeItem f m s)
	| toBlobType m == Just SymlinkBlob = do
		mk <- catKey s
		case mk of
			Just k -> Just . TreeItem f (fromBlobType FileBlob)
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

originalBranch :: Annex (Maybe OrigBranch)
originalBranch = fmap getorig <$> inRepo Git.Branch.current
  where
	getorig currbranch = maybe currbranch snd (adjustedToOriginal currbranch)

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
		adjbranch <- adjustBranch adj origbranch
		inRepo $ Git.Command.run
			[ Param "checkout"
			, Param $ fromRef $ Git.Ref.base $ adjbranch
			]
	go Nothing = error "not on any branch!"

adjustBranch :: Adjustment -> OrigBranch -> Annex AdjBranch
adjustBranch adj origbranch = do
	h <- inRepo hashObjectStart
	treesha <- adjustTree (adjustTreeItem adj h) origbranch =<< Annex.gitRepo
	liftIO $ hashObjectStop h
	commitsha <- commitAdjustedTree treesha origbranch
	inRepo $ Git.Branch.update adjbranch commitsha
	return adjbranch
  where
	adjbranch = originalToAdjusted origbranch adj

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
updateAdjustedBranch tomerge (origbranch, adj) commitmode = do
	error "updateAdjustedBranch"
