{- git-annex BranchState data type
 -
 - Copyright 2011-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.BranchState where

import Common
import qualified Git
import Types.Transitions

import qualified Data.ByteString.Lazy as L

data BranchState = BranchState
	{ branchUpdated :: Bool
	-- ^ has the branch been updated this run? (Or an update tried and
	-- failed due to permissions.)
	, indexChecked :: Bool
	-- ^ has the index file been checked to exist?
	, journalIgnorable :: Bool
	-- ^ can reading the journal be skipped, while still getting
	-- sufficiently up-to-date information from the branch?
	, unmergedRefs :: [Git.Sha]
	-- ^ when the branch was not able to be updated due to permissions,
	-- these other git refs contain unmerged information and need to be
	-- queried, along with the index and the journal.
	, unhandledTransitions :: [TransitionCalculator]
	-- ^ when the branch was not able to be updated due to permissions,
	-- this is transitions that need to be applied when making queries.
	, cachedFileContents :: [(OsPath, L.ByteString)]
	-- ^ contents of a few files recently read from the branch
	, needInteractiveAccess :: Bool
	-- ^ do new changes written to the journal or branch by another
	-- process need to be noticed while the current process is running?
	-- (This makes the journal always be read, and avoids using the
	-- cache.)
	, alternateJournal :: Maybe OsPath
	-- ^ use this directory for all journals, rather than the
	-- gitAnnexJournalDir and gitAnnexPrivateJournalDir.
	}

startBranchState :: BranchState
startBranchState = BranchState False False False [] [] [] False Nothing
