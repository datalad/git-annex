{- git-annex lock cache data types
 - 
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.LockCache (
	LockCache,
	LockHandle
) where

import qualified Data.Map as M
import Utility.LockPool

type LockCache = M.Map FilePath LockHandle
