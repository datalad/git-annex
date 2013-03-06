{- git-annex uuid-tagged pushes
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.TaggedPush where

import Common.Annex
import qualified Remote
import qualified Annex.Branch
import qualified Git
import qualified Git.Ref
import qualified Git.Command

{- Converts a git branch into a branch that is tagged with a UUID, typically
 - the UUID of the repo that will be pushing it.
 -
 - Pushing to branches on the remote that have out uuid in them is ugly,
 - but it reserves those branches for pushing by us, and so our pushes will
 - never conflict with other pushes.
 -
 - To avoid cluttering up the branch display, the branch is put under
 - refs/synced/, rather than the usual refs/remotes/
 -}
toTaggedBranch :: UUID -> Git.Branch -> Git.Branch
toTaggedBranch u b = Git.Ref $ concat
	[ s
	, ":"
	, "refs/synced/" ++ fromUUID u ++ "/" ++ s
	]
  where
	s = show $ Git.Ref.base b

branchTaggedBy :: Git.Branch -> Maybe UUID
branchTaggedBy b = case split "/" $ show b of
	("refs":"synced":u:_base) -> Just $ toUUID u
	_ -> Nothing

taggedPush :: UUID -> Git.Ref -> Remote -> Git.Repo -> IO Bool
taggedPush u branch remote = Git.Command.runBool
        [ Param "push"
        , Param $ Remote.name remote
        , Param $ show $ toTaggedBranch u Annex.Branch.name
        , Param $ show $ toTaggedBranch u branch
        ]
