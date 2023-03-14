{- git-annex assistant repository problem tracking
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Assistant.Types.RepoProblem where

import Types
import Utility.TList

import Control.Concurrent.STM
import Data.Function

data RepoProblem = RepoProblem
	{ problemUUID :: UUID
	, afterFix :: IO ()
	}

{- The afterFix actions are assumed to all be equivalent. -}
sameRepoProblem :: RepoProblem -> RepoProblem -> Bool
sameRepoProblem = (==) `on` problemUUID

type RepoProblemChan = TList RepoProblem

newRepoProblemChan :: IO RepoProblemChan
newRepoProblemChan = atomically newTList
