{- git-annex BranchState data type
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.BranchState where

data BranchState = BranchState {
	branchUpdated :: Bool, -- has the branch been updated this run?
	indexChecked :: Bool, -- has the index file been checked to exist?

	-- the content of one file is cached
	cachedFile :: Maybe FilePath,
	cachedContent :: String
}

startBranchState :: BranchState
startBranchState = BranchState False False Nothing ""
