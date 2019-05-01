{- git-annex remote tracking branches
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.RemoteTrackingBranch
	( RemoteTrackingBranch
	, mkRemoteTrackingBranch
	, fromRemoteTrackingBranch
	, setRemoteTrackingBranch
	, makeRemoteTrackingBranchMergeCommit
	, makeRemoteTrackingBranchMergeCommit'
	) where

import Annex.Common
import Git.Types
import qualified Git.Ref
import qualified Git.Branch
import qualified Git.History
import qualified Types.Remote as Remote

newtype RemoteTrackingBranch = RemoteTrackingBranch
	{ fromRemoteTrackingBranch :: Ref }
	deriving (Show, Eq)

{- Makes a remote tracking branch corresponding to a local branch. 
 - Note that the local branch does not need to exist yet. -}
mkRemoteTrackingBranch :: Remote -> Branch -> RemoteTrackingBranch
mkRemoteTrackingBranch remote ref = RemoteTrackingBranch $
	Git.Ref.underBase ("refs/remotes/" ++ Remote.name remote) ref

{- Set remote tracking branch to point to a commit. -}
setRemoteTrackingBranch :: RemoteTrackingBranch -> Sha -> Annex ()
setRemoteTrackingBranch tb commit = 
	inRepo $ Git.Branch.update' (fromRemoteTrackingBranch tb) commit

{- Makes a merge commit that preserves the import history of the
 - RemoteTrackingBranch, while grafting new git history into it.
 -
 - The second parent of the merge commit is the past history of the
 - RemoteTrackingBranch as imported from a remote. When importing a
 - history of trees from a remote, commits can be sythesized from
 - them, but such commits won't have the same sha due to eg date differing.
 - But since we know that the second parent consists entirely of such
 - import commits, they can be reused when updating the
 - RemoteTrackingBranch.
 -
 - The commitsha should have the treesha as its tree.
 -}
makeRemoteTrackingBranchMergeCommit :: RemoteTrackingBranch -> Sha -> Sha -> Annex Sha
makeRemoteTrackingBranchMergeCommit tb commitsha treesha =
	-- Check if the tracking branch exists.
	inRepo (Git.Ref.sha (fromRemoteTrackingBranch tb)) >>= \case
		Nothing -> return commitsha
		Just _ -> inRepo (Git.History.getHistoryToDepth 1 (fromRemoteTrackingBranch tb)) >>= \case
			Nothing -> return commitsha
			Just (Git.History.History hc _) -> case Git.History.historyCommitParents hc of
				[_, importhistory] ->
					makeRemoteTrackingBranchMergeCommit' commitsha importhistory treesha
				-- Earlier versions of git-annex did not
				-- make the merge commit, or perhaps
				-- something else changed where the
				-- tracking branch pointed.
				_ -> return commitsha

makeRemoteTrackingBranchMergeCommit' :: Sha -> Sha -> Sha -> Annex Sha
makeRemoteTrackingBranchMergeCommit' commitsha importedhistory treesha = 
	inRepo $ Git.Branch.commitTree
			Git.Branch.AutomaticCommit
			"remote tracking branch"
			[commitsha, importedhistory]
			treesha
