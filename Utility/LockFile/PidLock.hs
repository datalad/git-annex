{- pid-based lock files
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.LockFile.PidLock (
	LockHandle,
	tryLock,
	waitLock,
	dropLock,
	LockStatus(..),
	getLockStatus,
	checkLocked,
	checkSaneLock,
) where

import Utility.PartialPrelude
import Utility.Exception
import Utility.Applicative
import Utility.Directory
import Utility.Monad
import Utility.Path
import Utility.FileMode
import Utility.LockFile.LockStatus
import Utility.ThreadScheduler
import qualified Utility.LockFile.Posix as Posix

import System.IO
import System.Posix
import Data.Maybe
import Data.List
import Network.BSD
import System.FilePath
import Data.Hash.MD5
import System.Directory
import Control.Applicative
import Prelude

type LockFile = FilePath

data LockHandle = LockHandle LockFile FileStatus SideLockHandle

type SideLockHandle = Maybe (LockFile, Posix.LockHandle)

data PidLock = PidLock
	{ lockingPid :: ProcessID
	, lockingHost :: HostName
	}
	deriving (Eq, Read, Show)

mkPidLock :: IO PidLock
mkPidLock = PidLock
	<$> getProcessID
	<*> getHostName

readPidLock :: LockFile -> IO (Maybe PidLock)
readPidLock lockfile = (readish =<<) <$> catchMaybeIO (readFile lockfile)

-- To avoid races when taking over a stale pid lock, a side lock is used.
-- This is a regular posix exclusive lock.
trySideLock :: LockFile -> (SideLockHandle -> IO a) -> IO a
trySideLock lockfile a = do
	sidelock <- sideLockFile lockfile
	mlck <- catchDefaultIO Nothing $ 
		withUmask nullFileMode $
			Posix.tryLockExclusive (Just mode) sidelock
	-- Check the lock we just took, in case we opened a side lock file
	-- belonging to another process that will have since deleted it.
	case mlck of
		Nothing -> a Nothing
		Just lck -> ifM (Posix.checkSaneLock sidelock lck)
			( a (Just (sidelock, lck))
			, a Nothing
			)
  where
	-- Let all users write to the lock file in /dev/shm or /tmp,
	-- so that other users can reuse it to take the lock.
	-- Since /dev/shm and /tmp are sticky dirs, a user cannot
	-- delete another user's lock file there, so could not
	-- delete a stale lock.
	mode = combineModes (readModes ++ writeModes)

dropSideLock :: SideLockHandle -> IO ()
dropSideLock Nothing = return ()
dropSideLock (Just (f, h)) = do
	-- Delete the file first, to ensure that any process that is trying
	-- to take the side lock will only succeed once the file is
	-- deleted, and so will be able to immediately see that it's taken
	-- a stale lock.
	_ <- tryIO $ removeFile f 
	Posix.dropLock h

-- The side lock is put in /dev/shm. This will work on most any
-- Linux system, even if its whole root filesystem doesn't support posix
-- locks. /tmp is used as a fallback.
sideLockFile :: LockFile -> IO LockFile
sideLockFile lockfile = do
	f <- absPath lockfile
	let base = intercalate "_" (splitDirectories (makeRelative "/" f))
	let shortbase = reverse $ take 32 $ reverse base
	let md5sum = if base == shortbase then "" else md5s (Str base)
	dir <- ifM (doesDirectoryExist "/dev/shm")
		( return "/dev/shm"
		, return "/tmp"
		)
	return $ dir </> md5sum ++ shortbase ++ ".lck"

-- | Tries to take a lock; does not block when the lock is already held.
--
-- Note that stale locks are automatically detected and broken.
-- However, if the lock file is on a networked file system, and was
-- created on a different host than the current host (determined by hostname),
-- this can't be done and stale locks may persist.
tryLock :: LockFile -> IO (Maybe LockHandle)
tryLock lockfile = trySideLock lockfile $ \sidelock -> do
	lockfile' <- absPath lockfile
	(tmp, h) <- openTempFile (takeDirectory lockfile') "locktmp"
	setFileMode tmp (combineModes readModes)
	hPutStr h . show =<< mkPidLock
	hClose h
	st <- getFileStatus tmp
	let failedlock = do
		dropLock $ LockHandle tmp st sidelock
		return Nothing
	let tooklock = return $ Just $ LockHandle lockfile' st sidelock
	ifM (linkToLock sidelock tmp lockfile')
		( do
			nukeFile tmp
			tooklock
		, do
			v <- readPidLock lockfile'
			hn <- getHostName
			case v of
				Just pl | isJust sidelock && hn == lockingHost pl -> do
					-- Since we have the sidelock,
					-- and are on the same host that
					-- the pidlock was taken on,
					-- we know that the pidlock is
					-- stale, and can take it over.
					rename tmp lockfile'
					tooklock
				_ -> failedlock
		)

-- Linux's open(2) man page recommends linking a pid lock into place,
-- as the most portable atomic operation that will fail if
-- it already exists. 
--
-- open(2) suggests that link can sometimes appear to fail
-- on NFS but have actually succeeded, and the way to find out is to stat
-- the file and check its link count etc.
linkToLock :: SideLockHandle -> FilePath -> FilePath -> IO Bool
linkToLock Nothing _ _ = return False
linkToLock (Just _) src dest = do
	_ <- tryIO $ createLink src dest
	ifM (catchBoolIO checklinked)
		( catchBoolIO $ not <$> checkInsaneLustre dest
		, return False
		)
  where
	checklinked = do
		x <- getSymbolicLinkStatus src
		y <- getSymbolicLinkStatus dest
		return $ and
			[ deviceID x == deviceID y
			, fileID x == fileID y
			, fileMode x == fileMode y
			, fileOwner x == fileOwner y
			, fileGroup x == fileGroup y
			, fileSize x == fileSize y
			, modificationTime x == modificationTime y
			, isRegularFile x == isRegularFile y
			, linkCount x == linkCount y
			, linkCount x == 2
			]

-- On a Lustre filesystem, link has been observed to incorrectly *succeed*,
-- despite the dest already existing. A subsequent stat of the dest
-- looked like it had been replaced with the src. The process proceeded to
-- run and then deleted the dest, and after the process was done, the
-- original file was observed to still be in place.
--
-- We can detect this insanity by getting the directory contents after
-- making the link, and checking to see if 2 copies of the dest file,
-- with the SAME FILENAME exist.
checkInsaneLustre :: FilePath -> IO Bool
checkInsaneLustre dest = do
	fs <- dirContents (takeDirectory dest)
	case length (filter (== dest) fs) of
		1 -> return False -- whew!
		0 -> return True -- wtf?
		_ -> do
			-- Try to clean up the extra copy we made
			-- that has the same name. Egads.
			_ <- tryIO $ removeFile dest
			return True

-- | Waits as necessary to take a lock.
--
-- Uses a 1 second wait-loop.
--
-- May wait until timeout if the lock file is stale and is on a network file
-- system, or on a system where the side lock cannot be taken.
waitLock :: Seconds -> LockFile -> IO LockHandle
waitLock (Seconds timeout) lockfile = go timeout
  where
	go n
		| n > 0 = maybe (threadDelaySeconds (Seconds 1) >> go (pred n)) return
			=<< tryLock lockfile
		| otherwise = do
			hPutStrLn stderr $ show timeout ++ " second timeout exceeded while waiting for pid lock file " ++ lockfile
			error $ "Gave up waiting for possibly stale pid lock file " ++ lockfile

dropLock :: LockHandle -> IO ()
dropLock (LockHandle lockfile _ sidelock) = do
	-- Drop side lock first, at which point the pid lock will be
	-- considered stale.
	dropSideLock sidelock
	nukeFile lockfile

getLockStatus :: LockFile -> IO LockStatus
getLockStatus = maybe StatusUnLocked (StatusLockedBy . lockingPid) <$$> readPidLock

checkLocked :: LockFile -> IO (Maybe Bool)
checkLocked lockfile = conv <$> getLockStatus lockfile
  where
	conv (StatusLockedBy _) = Just True
	conv _ = Just False

-- Checks that the lock file still exists, and is the same file that was
-- locked to get the LockHandle.
checkSaneLock :: LockFile -> LockHandle -> IO Bool
checkSaneLock lockfile (LockHandle _ st _) = 
	go =<< catchMaybeIO (getFileStatus lockfile)
  where
	go Nothing = return False
	go (Just st') = return $
		deviceID st == deviceID st' && fileID st == fileID st'
