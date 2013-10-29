{- git-annex assistant remote problem detection
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.RepoProblem where

import Types
import Utility.TList

import Control.Concurrent.STM

type RepoProblemChan = TList UUID

newRepoProblemChan :: IO RepoProblemChan
newRepoProblemChan = atomically newTList
