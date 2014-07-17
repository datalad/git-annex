{- git-annex assistant git-annex branch change tracking
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.BranchChange where

import Control.Concurrent.MSampleVar
import Common.Annex

newtype BranchChangeHandle = BranchChangeHandle (MSampleVar ())

newBranchChangeHandle :: IO BranchChangeHandle
newBranchChangeHandle = BranchChangeHandle <$> newEmptySV

fromBranchChangeHandle :: BranchChangeHandle -> MSampleVar ()
fromBranchChangeHandle (BranchChangeHandle v) = v
