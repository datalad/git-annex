{- git-annex BranchState data type
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.BranchState where

data BranchState = BranchState
	{ branchUpdated :: Bool
	-- ^ has the branch been updated this run?
	, indexChecked :: Bool
	-- ^ has the index file been checked to exist?
	, journalIgnorable :: Bool
	-- ^ can reading the journal be skipped, while still getting
	-- sufficiently up-to-date information from the branch?
	, journalNeverIgnorable :: Bool
	-- ^ should the journal always be read even if it would normally
	-- be safe to skip it?
	}

startBranchState :: BranchState
startBranchState = BranchState False False False False
