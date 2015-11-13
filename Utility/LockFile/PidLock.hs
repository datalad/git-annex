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
import Control.Applicative
import Network.BSD
import System.FilePath

type LockFile = FilePath

data LockHandle = LockHandle FilePath Fd (Maybe Posix.LockHandle)

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
-- This is a regular posix exclusive lock. The side lock is put in
-- /dev/shm. This will work on most any Linux system, even if its whole
-- root filesystem doesn't support posix locks.
trySideLock :: LockFile -> (Maybe Posix.LockHandle -> IO a) -> IO a
trySideLock lockfile a = do
	f <- absPath lockfile
	let sidelock = "/dev/shm" </>
		intercalate "_" (splitDirectories (makeRelative "/" f)) ++ ".lck"
	mlck <- catchDefaultIO Nothing $ 
		withUmask nullFileMode $
			Posix.tryLockExclusive (Just mode) sidelock
	a mlck
  where
	-- Let all users write to the lock file in /dev/shm,
	-- so that other users can reuse it to take the lock.
	-- Since /dev/shm is sticky, a user cannot delete another user's
	-- lock file there, so could not delete a stale lock.
	mode = combineModes (readModes ++ writeModes)

-- | Tries to take a lock; does not block when the lock is already held.
--
-- The method used is atomic even on NFS without needing O_EXCL support.
--
-- Note that stale locks are automatically detected and broken.
-- However, if the lock file is on a networked file system, and was
-- created on a different host than the current host (determined by hostname),
-- this can't be done and stale locks may persist.
tryLock :: LockFile -> IO (Maybe LockHandle)
tryLock lockfile = trySideLock lockfile $ \sidelock -> do
	(tmp, h) <- openTempFile (takeDirectory lockfile) "locktmp"
	setFileMode tmp (combineModes readModes)
	hPutStr h . show =<< mkPidLock
	fd <- handleToFd h
	let failedlock = do
		closeFd fd
		nukeFile tmp
		return Nothing
	let tooklock = return $ Just $ LockHandle lockfile fd sidelock
	ifM (linkToLock tmp lockfile)
		( do
			nukeFile tmp
			tooklock
		, do
			v <- readPidLock lockfile
			hn <- getHostName
			case v of
				Just pl | isJust sidelock && hn == lockingHost pl -> do
					-- Since we have the sidelock,
					-- and are on the same host that
					-- the pidlock was taken on,
					-- we know that the pidlock is
					-- stale, and can take it over.
					rename tmp lockfile
					tooklock
				_ -> failedlock
		)

-- Linux man pages recommend linking a pid lock into place,
-- as the most portable atomic operation that will fail if
-- it already exists. However, on some network filesystems,
-- link will return success sometimes despite having failed,
-- so we have to stat both files to check if it actually worked.
linkToLock :: FilePath -> FilePath -> IO Bool
linkToLock src dest = ifM (isJust <$> catchMaybeIO (createLink src dest))
	( catchDefaultIO False checklink
	, return False
	)
  where
	checklink = do
		x <- getSymbolicLinkStatus src
		y <- getSymbolicLinkStatus dest
		return $ and
			[ deviceID x == deviceID y
			, fileID x == fileID y
			, fileMode x == fileMode y
			, linkCount x == linkCount y
			, fileOwner x == fileOwner y
			, fileGroup x == fileGroup y
			, specialDeviceID x == specialDeviceID y
			, fileSize x == fileSize y
			, modificationTime x == modificationTime y
			, isRegularFile x == isRegularFile y
			]

-- | Waits as necessary to take a lock.
--
-- Uses a 1 second wait-loop.
--
-- May wait untie timeout if the lock file is stale and is on a network file
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
dropLock (LockHandle lockfile fd sidelock) = do
	-- Drop side lock first, at which point the pid lock will be
	-- considered stale.
	-- The side lock file cannot be deleted because another process may
	-- have it open and be waiting to lock it.
	maybe noop Posix.dropLock sidelock
	closeFd fd
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
checkSaneLock lockfile (LockHandle _ fd _) =
	go =<< catchMaybeIO (getFileStatus lockfile)
  where
	go Nothing = return False
	go (Just st) = do
		fdst <- getFdStatus fd
		return $ deviceID fdst == deviceID st && fileID fdst == fileID st
