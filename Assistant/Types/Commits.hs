{- git-annex assistant commit tracking
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.Commits where

import Utility.TSet

type CommitChan = TSet Commit

data Commit = Commit

newCommitChan :: IO CommitChan
newCommitChan = newTSet
