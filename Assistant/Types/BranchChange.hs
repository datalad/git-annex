{- git-annex assistant git-annex branch change tracking
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Assistant.Types.BranchChange where

import Control.Concurrent.MSampleVar

newtype BranchChangeHandle = BranchChangeHandle (MSampleVar ())

newBranchChangeHandle :: IO BranchChangeHandle
newBranchChangeHandle = BranchChangeHandle <$> newEmptySV

fromBranchChangeHandle :: BranchChangeHandle -> MSampleVar ()
fromBranchChangeHandle (BranchChangeHandle v) = v
