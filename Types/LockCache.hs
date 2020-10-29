{- git-annex lock cache data types
 - 
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.LockCache (
	LockCache,
	LockHandle
) where

import Utility.LockPool (LockHandle)

import qualified Data.Map as M
import System.FilePath.ByteString (RawFilePath)

type LockCache = M.Map RawFilePath LockHandle
