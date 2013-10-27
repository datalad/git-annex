{- git-annex assistant remote problem detection
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.RemoteProblem where

import Types
import Utility.TList

import Control.Concurrent.STM

type RemoteProblemChan = TList Remote

newRemoteProblemChan :: IO RemoteProblemChan
newRemoteProblemChan = atomically newTList
