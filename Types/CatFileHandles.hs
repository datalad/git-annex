{- git-cat file handles pools
 -
 - Copyright 2020-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.CatFileHandles (
	CatFileHandles(..),
	catFileHandlesNonConcurrent,
	catFileHandlesPool,
	CatMap(..),
	emptyCatMap,
) where

import Control.Concurrent.STM
import qualified Data.Map as M

import Utility.ResourcePool
import Git.CatFile (CatFileHandle, CatFileMetaDataHandle)

data CatFileHandles
	= CatFileHandlesNonConcurrent CatMap
	| CatFileHandlesPool (TMVar CatMap)

data CatMap = CatMap
	{ catFileMap :: M.Map FilePath (ResourcePool CatFileHandle)
	, catFileMetaDataMap :: M.Map FilePath (ResourcePool CatFileMetaDataHandle)
	}

emptyCatMap :: CatMap
emptyCatMap = CatMap M.empty M.empty

catFileHandlesNonConcurrent :: CatFileHandles
catFileHandlesNonConcurrent = CatFileHandlesNonConcurrent emptyCatMap

catFileHandlesPool :: IO CatFileHandles
catFileHandlesPool = CatFileHandlesPool <$> newTMVarIO emptyCatMap
