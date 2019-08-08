{- Caching a file's inode, size, and modification time
 - to see when it's changed.
 -
 - Copyright 2013-2018 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utility.InodeCache (
	InodeCache,
	InodeComparisonType(..),
	inodeCacheFileSize,

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
import Utility.TimeStamp
import Utility.QuickCheck

import System.PosixCompat.Types
import Data.Time.Clock.POSIX

#ifdef mingw32_HOST_OS
import Data.Word (Word64)
#else
import System.Posix.Files
#endif

data InodeCachePrim = InodeCachePrim FileID FileSize MTime
	deriving (Show, Eq, Ord)

newtype InodeCache = InodeCache InodeCachePrim
	deriving (Show)

inodeCacheFileSize :: InodeCache -> FileSize
inodeCacheFileSize (InodeCache (InodeCachePrim _ sz _)) = sz

{- Inode caches can be compared in two different ways, either weakly
 - or strongly. -}
data InodeComparisonType = Weakly | Strongly
	deriving (Eq, Ord, Show)

{- Strong comparison, including inodes. -}
compareStrong :: InodeCache -> InodeCache -> Bool
compareStrong (InodeCache x) (InodeCache y) = x == y

{- Weak comparison of the inode caches, comparing the size and mtime,
 - but not the actual inode.  Useful when inodes have changed, perhaps
 - due to some filesystems being remounted.
 -
 - The weak mtime comparison treats any mtimes that are within 2 seconds
 - of one-another as the same. This is because FAT has only a 2 second
 - resolution. When a FAT filesystem is used on Linux, higher resolution
 - timestamps maybe be are cached and used by Linux, but they are lost
 - on unmount, so after a remount, the timestamp can appear to have changed.
 -}
compareWeak :: InodeCache -> InodeCache -> Bool
compareWeak (InodeCache (InodeCachePrim _ size1 mtime1)) (InodeCache (InodeCachePrim _ size2 mtime2)) =
	size1 == size2 && (abs (lowResTime mtime1 - lowResTime mtime2) < 2)

compareBy :: InodeComparisonType -> InodeCache -> InodeCache -> Bool
compareBy Strongly = compareStrong
compareBy Weakly = compareWeak

{- For use in a Map; it's determined at creation time whether this
 - uses strong or weak comparison for Eq. -}
data InodeCacheKey = InodeCacheKey InodeComparisonType InodeCachePrim
	deriving (Ord, Show)

instance Eq InodeCacheKey where
	(InodeCacheKey ctx x) == (InodeCacheKey cty y) =
		compareBy (maximum [ctx,cty]) (InodeCache x ) (InodeCache y)

inodeCacheToKey :: InodeComparisonType -> InodeCache -> InodeCacheKey 
inodeCacheToKey ct (InodeCache prim) = InodeCacheKey ct prim

inodeCacheToMtime :: InodeCache -> POSIXTime
inodeCacheToMtime (InodeCache (InodeCachePrim _ _ mtime)) = highResTime mtime

{- For backwards compatability, support low-res mtime with no
 - fractional seconds. -}
data MTime = MTimeLowRes EpochTime | MTimeHighRes POSIXTime
	deriving (Show, Ord)

{- A low-res time compares equal to any high-res time in the same second. -}
instance Eq MTime where
	MTimeLowRes a == MTimeLowRes b = a == b
	MTimeHighRes a == MTimeHighRes b = a == b
	MTimeHighRes a == MTimeLowRes b = lowResTime a == b
	MTimeLowRes a == MTimeHighRes b = a == lowResTime b

class MultiResTime t where
	lowResTime :: t -> EpochTime
	highResTime :: t -> POSIXTime

instance MultiResTime EpochTime where
	lowResTime = id
	highResTime = realToFrac

instance MultiResTime POSIXTime where
	lowResTime = fromInteger . floor
	highResTime = id

instance MultiResTime MTime where
	lowResTime (MTimeLowRes t) = t
	lowResTime (MTimeHighRes t) = lowResTime t
	highResTime (MTimeLowRes t) = highResTime t
	highResTime (MTimeHighRes t) = t

showInodeCache :: InodeCache -> String
showInodeCache (InodeCache (InodeCachePrim inode size (MTimeHighRes mtime))) = 
	let (t, d) = separate (== '.') (takeWhile (/= 's') (show mtime))
	in unwords
		[ show inode
		, show size
		, t
		, d
		]
showInodeCache (InodeCache (InodeCachePrim inode size (MTimeLowRes mtime))) =
	unwords
		[ show inode
		, show size
		, show mtime
		]

readInodeCache :: String -> Maybe InodeCache
readInodeCache s = case words s of
	(inode:size:mtime:[]) -> do
		i <- readish inode
		sz <- readish size
		t <- readish mtime
		return $ InodeCache $ InodeCachePrim i sz (MTimeLowRes t)
	(inode:size:mtime:mtimedecimal:_) -> do
		i <- readish inode
		sz <- readish size
		t <- parsePOSIXTime $ mtime ++ '.' : mtimedecimal
		return $ InodeCache $ InodeCachePrim i sz (MTimeHighRes t)
	_ -> Nothing

genInodeCache :: FilePath -> TSDelta -> IO (Maybe InodeCache)
genInodeCache f delta = catchDefaultIO Nothing $
	toInodeCache delta f =<< getFileStatus f

toInodeCache :: TSDelta -> FilePath -> FileStatus -> IO (Maybe InodeCache)
toInodeCache (TSDelta getdelta) f s
	| isRegularFile s = do
		delta <- getdelta
		sz <- getFileSize' f s
#ifdef mingw32_HOST_OS
		mtime <- MTimeHighRes . utcTimeToPOSIXSeconds <$> getModificationTime f
#else
 		let mtime = (MTimeHighRes (modificationTimeHiRes s + highResTime delta))
#endif
		return $ Just $ InodeCache $ InodeCachePrim (fileID s) sz mtime
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
					lowResTime oldmtime - lowResTime currmtime
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

instance Arbitrary MTime where
	arbitrary = frequency
		-- timestamp is not usually negative
                [ (50, MTimeLowRes <$> (abs . fromInteger <$> arbitrary))
                , (50, MTimeHighRes <$> arbitrary)
		]

#ifdef mingw32_HOST_OS
instance Arbitrary FileID where
	arbitrary = fromIntegral <$> (arbitrary :: Gen Word64)
#endif

prop_read_show_inodecache :: InodeCache -> Bool
prop_read_show_inodecache c = case readInodeCache (showInodeCache c) of
	Nothing -> False
	Just c' -> compareStrong c c'
