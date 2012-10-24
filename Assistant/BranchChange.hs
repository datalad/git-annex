{- git-annex assistant git-annex branch change tracking
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.BranchChange where

import Control.Concurrent.MSampleVar
import Assistant.Common

newtype BranchChangeHandle = BranchChangeHandle (MSampleVar ())

newBranchChangeHandle :: IO BranchChangeHandle
newBranchChangeHandle = BranchChangeHandle <$> newEmptySV

branchChanged :: BranchChangeHandle -> IO ()
branchChanged (BranchChangeHandle h) = writeSV h ()

waitBranchChange :: BranchChangeHandle -> IO ()
waitBranchChange (BranchChangeHandle h) = readSV h
