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
	) where

import Annex.Common
import Git.Types
import qualified Git.Ref
import qualified Git.Branch
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
