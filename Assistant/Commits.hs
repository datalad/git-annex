{- git-annex assistant commit tracking
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Commits where

import Assistant.Common
import Assistant.Types.Commits
import Utility.TSet

import Control.Concurrent.STM

{- Gets all unhandled commits.
 - Blocks until at least one commit is made. -}
getCommits :: Assistant [Commit]
getCommits = (atomically . getTSet) <<~ commitChan

{- Puts unhandled commits back into the channel.
 - Note: Original order is not preserved. -}
refillCommits :: [Commit] -> Assistant ()
refillCommits cs = (atomically . flip putTSet cs) <<~ commitChan

{- Records a commit in the channel. -}
recordCommit :: Assistant ()
recordCommit = (atomically . flip putTSet1 Commit) <<~ commitChan
