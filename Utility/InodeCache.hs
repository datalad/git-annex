{- Caching a file's inode, size, and modification time to see when it's changed.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.InodeCache where

import Common
import System.PosixCompat.Types
import Utility.QuickCheck

data InodeCachePrim = InodeCachePrim FileID FileOffset EpochTime
	deriving (Show, Eq, Ord)

newtype InodeCache = InodeCache InodeCachePrim
	deriving (Show)

{- Inode caches can be compared in two different ways, either weakly
 - or strongly. -}
data InodeComparisonType = Weakly | Strongly
	deriving (Eq, Ord)

{- Strong comparison, including inodes. -}
compareStrong :: InodeCache -> InodeCache -> Bool
compareStrong (InodeCache x) (InodeCache y) = x == y

{- Weak comparison of the inode caches, comparing the size and mtime,
 - but not the actual inode.  Useful when inodes have changed, perhaps
 - due to some filesystems being remounted. -}
compareWeak :: InodeCache -> InodeCache -> Bool
compareWeak (InodeCache (InodeCachePrim _ size1 mtime1)) (InodeCache (InodeCachePrim _ size2 mtime2)) =
	size1 == size2 && mtime1 == mtime2

compareBy :: InodeComparisonType -> InodeCache -> InodeCache -> Bool
compareBy Strongly = compareStrong
compareBy Weakly = compareWeak

{- For use in a Map; it's determined at creation time whether this
 - uses strong or weak comparison for Eq. -}
data InodeCacheKey = InodeCacheKey InodeComparisonType InodeCachePrim
	deriving (Ord)

instance Eq InodeCacheKey where
	(InodeCacheKey ctx x) == (InodeCacheKey cty y) =
		compareBy (maximum [ctx,cty]) (InodeCache x ) (InodeCache y)

inodeCacheToKey :: InodeComparisonType -> InodeCache -> InodeCacheKey 
inodeCacheToKey ct (InodeCache prim) = InodeCacheKey ct prim

inodeCacheToMtime :: InodeCache -> EpochTime
inodeCacheToMtime (InodeCache (InodeCachePrim _ _ mtime)) = mtime

showInodeCache :: InodeCache -> String
showInodeCache (InodeCache (InodeCachePrim inode size mtime)) = unwords
	[ show inode
	, show size
	, show mtime
	]

readInodeCache :: String -> Maybe InodeCache
readInodeCache s = case words s of
	(inode:size:mtime:_) ->
		let prim = InodeCachePrim
			<$> readish inode
			<*> readish size
			<*> readish mtime
		in InodeCache <$> prim
	_ -> Nothing

genInodeCache :: FilePath -> IO (Maybe InodeCache)
genInodeCache f = catchDefaultIO Nothing $ toInodeCache <$> getFileStatus f

toInodeCache :: FileStatus -> Maybe InodeCache
toInodeCache s
	| isRegularFile s = Just $ InodeCache $ InodeCachePrim
		(fileID s)
		(fileSize s)
		(modificationTime s)
	| otherwise = Nothing

instance Arbitrary InodeCache where
	arbitrary =
		let prim = InodeCachePrim
			<$> arbitrary
			<*> arbitrary
			<*> arbitrary
		in InodeCache <$> prim

prop_read_show_inodecache :: InodeCache -> Bool
prop_read_show_inodecache c = case readInodeCache (showInodeCache c) of
	Nothing -> False
	Just c' -> compareStrong c c'
