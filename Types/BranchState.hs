{- git-annex BranchState data type
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.BranchState where

import Common

import qualified Data.ByteString.Lazy as L

data BranchState = BranchState
	{ branchUpdated :: Bool
	-- ^ has the branch been updated this run?
	, indexChecked :: Bool
	-- ^ has the index file been checked to exist?
	, journalIgnorable :: Bool
	-- ^ can reading the journal be skipped, while still getting
	-- sufficiently up-to-date information from the branch?
	, cachedFileContents :: [(RawFilePath, L.ByteString)]
	-- ^ contents of a few files recently read from the branch
	, needInteractiveAccess :: Bool
	-- ^ do new changes written to the journal or branch by another
	-- process need to be noticed while the current process is running?
	-- (This makes the journal always be read, and avoids using the
	-- cache.)
	}

startBranchState :: BranchState
startBranchState = BranchState False False False [] False
