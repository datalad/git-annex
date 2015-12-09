{- Caching a file's inode, size, and modification time
 - to see when it's changed.
 -
 - Copyright 2013-2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utility.InodeCache (
	InodeCache,
	InodeComparisonType(..),

	compareStrong,
	compareWeak,
	compareBy,

	readInodeCache,
	showInodeCache,
	genInodeCache,
	toInodeCache,

	InodeCacheKey,
	inodeCacheToKey,
	inodeCacheToMtime,

	SentinalFile(..),
	SentinalStatus(..),
	TSDelta,
	noTSDelta,
	writeSentinalFile,
	checkSentinalFile,
	sentinalFileExists,

	prop_read_show_inodecache
) where

import Common
import System.PosixCompat.Types
import Utility.QuickCheck

#ifdef mingw32_HOST_OS
import Data.Word (Word64)
#endif

data InodeCachePrim = InodeCachePrim FileID Integer EpochTime
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
 - due to some filesystems being remounted.
 -
 - The weak mtime comparison treats any mtimes that are within 2 seconds
 - of one-anther as the same. This is because FAT has only a 2 second
 - resolution. When a FAT filesystem is used on Linux, higher resolution
 - timestamps are cached and used by Linux, but this is lost on unmount,
 - so after a remount, the timestamp can appear to have changed.
 -}
compareWeak :: InodeCache -> InodeCache -> Bool
compareWeak (InodeCache (InodeCachePrim _ size1 mtime1)) (InodeCache (InodeCachePrim _ size2 mtime2)) =
	size1 == size2 && (abs (mtime1 - mtime2) < 2)

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

genInodeCache :: FilePath -> TSDelta -> IO (Maybe InodeCache)
genInodeCache f delta = catchDefaultIO Nothing $
	toInodeCache delta f =<< getFileStatus f

toInodeCache :: TSDelta -> FilePath -> FileStatus -> IO (Maybe InodeCache)
toInodeCache (TSDelta getdelta) f s
	| isRegularFile s = do
		delta <- getdelta
		sz <- getFileSize' f s
		return $ Just $ InodeCache $ InodeCachePrim
			(fileID s)
			sz
			(modificationTime s + delta)
	| otherwise = pure Nothing

{- Some filesystem get new random inodes each time they are mounted.
 - To detect this and other problems, a sentinal file can be created.
 - Its InodeCache at the time of its creation is written to the cache file,
 - so changes can later be detected. -}
data SentinalFile = SentinalFile
	{ sentinalFile :: FilePath
	, sentinalCacheFile :: FilePath
	}
	deriving (Show)

{- On Windows, the mtime of a file appears to change when the time zone is
 - changed. To deal with this, a TSDelta can be used; the delta is added to
 - the mtime when generating an InodeCache. The current delta can be found
 - by looking at the SentinalFile. Effectively, this makes all InodeCaches
 - use the same time zone that was in use when the sential file was
 - originally written. -}
newtype TSDelta = TSDelta (IO EpochTime)

noTSDelta :: TSDelta
noTSDelta = TSDelta (pure 0)

writeSentinalFile :: SentinalFile -> IO ()
writeSentinalFile s = do
	writeFile (sentinalFile s) ""
	maybe noop (writeFile (sentinalCacheFile s) . showInodeCache)
		=<< genInodeCache (sentinalFile s) noTSDelta

data SentinalStatus = SentinalStatus
	{ sentinalInodesChanged :: Bool
	, sentinalTSDelta :: TSDelta
	} 

{- Checks if the InodeCache of the sentinal file is the same
 - as it was when it was originally created.
 -
 - On Windows, time stamp differences are ignored, since they change
 - with the timezone.
 -
 - When the sential file does not exist, InodeCaches canot reliably be
 - compared, so the assumption is that there is has been a change.
 -}
checkSentinalFile :: SentinalFile -> IO SentinalStatus
checkSentinalFile s = do
	mold <- loadoldcache
	case mold of
		Nothing -> return dummy
		(Just old) -> do
			mnew <- gennewcache
			case mnew of
				Nothing -> return dummy
				Just new -> return $ calc old new
  where
	loadoldcache = catchDefaultIO Nothing $
		readInodeCache <$> readFile (sentinalCacheFile s)
	gennewcache = genInodeCache (sentinalFile s) noTSDelta
	calc (InodeCache (InodeCachePrim oldinode oldsize oldmtime)) (InodeCache (InodeCachePrim newinode newsize newmtime)) =
		SentinalStatus (not unchanged) tsdelta
	  where
#ifdef mingw32_HOST_OS
		-- Since mtime can appear to change when the time zone is
		-- changed in windows, we cannot look at the mtime for the
		-- sentinal file.
		unchanged = oldinode == newinode && oldsize == newsize && (newmtime == newmtime)
		tsdelta = TSDelta $ do
			-- Run when generating an InodeCache, 
			-- to get the current delta.
			mnew <- gennewcache
			return $ case mnew of
				Just (InodeCache (InodeCachePrim _ _ currmtime)) ->
					oldmtime - currmtime
				Nothing -> 0
#else
		unchanged = oldinode == newinode && oldsize == newsize && oldmtime == newmtime
		tsdelta = noTSDelta
#endif
	dummy = SentinalStatus True noTSDelta

sentinalFileExists :: SentinalFile -> IO Bool
sentinalFileExists s = allM doesFileExist [sentinalCacheFile s, sentinalFile s]

instance Arbitrary InodeCache where
	arbitrary =
		let prim = InodeCachePrim
			<$> arbitrary
			<*> arbitrary
			<*> arbitrary
		in InodeCache <$> prim

#ifdef mingw32_HOST_OS
instance Arbitrary FileID where
	arbitrary = fromIntegral <$> (arbitrary :: Gen Word64)
#endif

prop_read_show_inodecache :: InodeCache -> Bool
prop_read_show_inodecache c = case readInodeCache (showInodeCache c) of
	Nothing -> False
	Just c' -> compareStrong c c'
