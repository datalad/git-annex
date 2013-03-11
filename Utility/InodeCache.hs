{- Caching a file's inode, size, and modification time to see when it's changed.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.InodeCache where

import Common
import System.Posix.Types
import Utility.QuickCheck

data InodeCache = InodeCache FileID FileOffset EpochTime
	deriving (Show)

compareStrong :: InodeCache -> InodeCache -> Bool
compareStrong (InodeCache inode1 size1 mtime1) (InodeCache inode2 size2 mtime2) =
	inode1 == inode2 && size1 == size2 && mtime1 == mtime2

{- Weak comparison of the inode caches, comparing the size and mtime, but
 - not the actual inode.  Useful when inodes have changed, perhaps
 - due to some filesystems being remounted. -}
compareWeak :: InodeCache -> InodeCache -> Bool
compareWeak (InodeCache _ size1 mtime1) (InodeCache _ size2 mtime2) =
	size1 == size2 && mtime1 == mtime2

showInodeCache :: InodeCache -> String
showInodeCache (InodeCache inode size mtime) = unwords
	[ show inode
	, show size
	, show mtime
	]

readInodeCache :: String -> Maybe InodeCache
readInodeCache s = case words s of
	(inode:size:mtime:_) -> InodeCache
		<$> readish inode
		<*> readish size
		<*> readish mtime
	_ -> Nothing

genInodeCache :: FilePath -> IO (Maybe InodeCache)
genInodeCache f = catchDefaultIO Nothing $ toInodeCache <$> getFileStatus f

toInodeCache :: FileStatus -> Maybe InodeCache
toInodeCache s
	| isRegularFile s = Just $ InodeCache
		(fileID s)
		(fileSize s)
		(modificationTime s)
	| otherwise = Nothing

instance Arbitrary InodeCache where
	arbitrary = InodeCache
		<$> arbitrary
		<*> arbitrary
		<*> arbitrary

prop_read_show_inodecache :: InodeCache -> Bool
prop_read_show_inodecache c = case readInodeCache (showInodeCache c) of
	Nothing -> False
	Just c' -> compareStrong c c'
