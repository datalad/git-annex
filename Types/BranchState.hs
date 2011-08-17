{- git-annex BranchState data type
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.BranchState where

import System.IO

data BranchState = BranchState {
	branchUpdated :: Bool, -- has the branch been updated this run?

	-- (from, to) handles used to talk to a git-cat-file process
	catFileHandles :: Maybe (Handle, Handle),

	-- the content of one file is cached
	cachedFile :: Maybe FilePath,
	cachedContent :: String
}

startBranchState :: BranchState
startBranchState = BranchState False Nothing Nothing ""
