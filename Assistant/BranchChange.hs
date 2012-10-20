{- git-annex assistant git-annex branch change tracking
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.BranchChange where

import Control.Concurrent.MSampleVar

type BranchChangeHandle = MSampleVar ()

newBranchChangeHandle :: IO BranchChangeHandle
newBranchChangeHandle = newEmptySV

branchChanged :: BranchChangeHandle -> IO ()
branchChanged = flip writeSV ()

waitBranchChange :: BranchChangeHandle -> IO ()
waitBranchChange = readSV
