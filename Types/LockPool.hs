{- git-annex lock pool data types
 - 
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.LockPool (
	LockPool,
	LockHandle
) where

import qualified Data.Map as M
import Utility.LockFile

type LockPool = M.Map FilePath LockHandle
