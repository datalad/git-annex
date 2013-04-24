{- git-annex assistant commit tracking
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.Commits where

import Utility.TList

import Control.Concurrent.STM

type CommitChan = TList Commit

data Commit = Commit

newCommitChan :: IO CommitChan
newCommitChan = atomically newTList
