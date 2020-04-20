{- git-cat file handles pools
 -
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.CatFileHandles (
	CatFileHandles(..),
	catFileHandlesNonConcurrent,
	catFileHandlesPool,
) where

import Control.Concurrent.STM
import qualified Data.Map as M

import Utility.ResourcePool
import Git.CatFile (CatFileHandle)

data CatFileHandles
	= CatFileHandlesNonConcurrent CatMap
	| CatFileHandlesPool (TMVar CatMap)

type CatMap = M.Map FilePath (ResourcePool CatFileHandle)

catFileHandlesNonConcurrent :: CatFileHandles
catFileHandlesNonConcurrent = CatFileHandlesNonConcurrent M.empty

catFileHandlesPool :: IO CatFileHandles
catFileHandlesPool = CatFileHandlesPool <$> newTMVarIO M.empty
