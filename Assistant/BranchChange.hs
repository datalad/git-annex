{- git-annex assistant git-annex branch change tracking
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.BranchChange where

import Assistant.Common
import Assistant.Types.BranchChange

import Control.Concurrent.MSampleVar

branchChanged :: Assistant ()
branchChanged = flip writeSV () <<~ (fromBranchChangeHandle . branchChangeHandle)

waitBranchChange :: Assistant ()
waitBranchChange = readSV <<~ (fromBranchChangeHandle . branchChangeHandle)
