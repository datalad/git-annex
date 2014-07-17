{- git-annex assistant commit tracking
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Commits where

import Assistant.Common
import Assistant.Types.Commits
import Utility.TList

import Control.Concurrent.STM

{- Gets all unhandled commits.
 - Blocks until at least one commit is made. -}
getCommits :: Assistant [Commit]
getCommits = (atomically . getTList) <<~ commitChan

{- Records a commit in the channel. -}
recordCommit :: Assistant ()
recordCommit = (atomically . flip consTList Commit) <<~ commitChan
