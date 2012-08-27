{- git-annex assistant commit tracking
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Commits where

import Utility.TSet

import Data.Time.Clock

type CommitChan = TSet Commit

data Commit = Commit UTCTime
	deriving (Show)

newCommitChan :: IO CommitChan
newCommitChan = newTSet

{- Gets all unhandled commits.
 - Blocks until at least one commit is made. -}
getCommits :: CommitChan -> IO [Commit]
getCommits = getTSet

{- Puts unhandled commits back into the channel.
 - Note: Original order is not preserved. -}
refillCommits :: CommitChan -> [Commit] -> IO ()
refillCommits = putTSet

{- Records a commit in the channel. -}
recordCommit :: CommitChan -> Commit -> IO ()
recordCommit = putTSet1
