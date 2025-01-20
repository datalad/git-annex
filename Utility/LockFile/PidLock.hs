{- pid-based lock files
 -
 - Copyright 2015-2021 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE OverloadedStrings #-}

module Utility.LockFile.PidLock (
	PidLockFile,
	LockHandle,
	tryLock,
	waitLock,
	waitedLock,
	alreadyLocked,
	dropLock,
	LockStatus(..),
	getLockStatus,
	checkLocked,
	checkSaneLock,
	pidLockEnv,
	pidLockEnvValue,
) where

import Utility.PartialPrelude
import Utility.Exception
import Utility.Applicative
import Utility.Directory
import Utility.SystemDirectory
import Utility.Monad
import Utility.Path.AbsRel
import Utility.FileMode
import Utility.OpenFd
import Utility.LockFile.LockStatus
import Utility.ThreadScheduler
import Utility.Hash
import Utility.FileSystemEncoding
import Utility.Env
import Utility.Env.Set
import Utility.Tmp
import Utility.RawFilePath
import qualified Utility.LockFile.Posix as Posix

import System.IO
import System.Posix.Types
import System.Posix.IO.ByteString
import System.Posix.Files.ByteString
import System.Posix.Process
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified System.FilePath.ByteString as P
import Data.Maybe
import Data.List
import Network.BSD
import System.FilePath
import Control.Applicative
import Prelude

type PidLockFile = RawFilePath

data LockHandle
	= LockHandle PidLockFile FileStatus SideLockHandle
	| ParentLocked

type SideLockHandle = Maybe (RawFilePath, Posix.LockHandle)

data PidLock = PidLock
	{ lockingPid :: ProcessID
	, lockingHost :: HostName
	}
	deriving (Eq, Read, Show)

mkPidLock :: IO PidLock
mkPidLock = PidLock
	<$> getProcessID
	<*> getHostName

readPidLock :: PidLockFile -> IO (Maybe PidLock)
readPidLock lockfile = (readish =<<)
	<$> catchMaybeIO (readFile (fromRawFilePath lockfile))

-- To avoid races when taking over a stale pid lock, a side lock is used.
-- This is a regular posix exclusive lock.
trySideLock :: PidLockFile -> (SideLockHandle -> IO a) -> IO a
trySideLock lockfile a = do
	sidelock <- sideLockFile lockfile
	mlck <- catchDefaultIO Nothing $ 
		Posix.tryLockExclusive (Just modesetter) sidelock
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
	modesetter = ModeSetter mode (\f -> modifyFileMode f (const mode))

dropSideLock :: SideLockHandle -> IO ()
dropSideLock Nothing = return ()
dropSideLock (Just (f, h)) = do
	-- Delete the file first, to ensure that any process that is trying
	-- to take the side lock will only succeed once the file is
	-- deleted, and so will be able to immediately see that it's taken
	-- a stale lock.
	_ <- tryIO $ removeFile (fromRawFilePath f)
	Posix.dropLock h

-- The side lock is put in /dev/shm. This will work on most any
-- Linux system, even if its whole root filesystem doesn't support posix
-- locks. /tmp is used as a fallback.
sideLockFile :: PidLockFile -> IO RawFilePath
sideLockFile lockfile = do
	f <- fromRawFilePath <$> absPath lockfile
	let base = intercalate "_" (splitDirectories (makeRelative "/" f))
	let shortbase = reverse $ take 32 $ reverse base
	let md5sum = if base == shortbase
		then ""
		else toRawFilePath $ show (md5 (encodeBL base))
	dir <- ifM (doesDirectoryExist "/dev/shm")
		( return "/dev/shm"
		, return "/tmp"
		)
	return $ dir P.</> md5sum <> toRawFilePath shortbase <> ".lck"

-- | Tries to take a lock; does not block when the lock is already held.
--
-- Note that stale locks are automatically detected and broken.
-- However, if the lock file is on a networked file system, and was
-- created on a different host than the current host (determined by hostname),
-- this can't be done and stale locks may persist.
--
-- If a parent process is holding the lock, determined by a
-- "PIDLOCK_lockfile" environment variable, does not block either.
tryLock :: PidLockFile -> IO (Maybe LockHandle)
tryLock lockfile = do
	abslockfile <- absPath lockfile
	lockenv <- pidLockEnv abslockfile
	getEnv lockenv >>= \case
		Nothing -> trySideLock lockfile (go abslockfile)
		_ -> return (Just ParentLocked)
  where
	go abslockfile sidelock = do
		let abslockfile' = fromRawFilePath abslockfile
		(tmp, h) <- openTmpFileIn (takeDirectory abslockfile') "locktmp"
		let tmp' = toRawFilePath tmp
		setFileMode tmp' (combineModes readModes)
		hPutStr h . show =<< mkPidLock
		hClose h
		let failedlock = do
			dropSideLock sidelock
			removeWhenExistsWith removeLink tmp'
			return Nothing
		let tooklock st = return $ Just $ LockHandle abslockfile st sidelock
		linkToLock sidelock tmp' abslockfile >>= \case
			Just lckst -> do
				removeWhenExistsWith removeLink tmp'
				tooklock lckst
			Nothing -> do
				v <- readPidLock abslockfile
				hn <- getHostName
				tmpst <- getFileStatus tmp'
				case v of
					Just pl | isJust sidelock && hn == lockingHost pl -> do
						-- Since we have the sidelock,
						-- and are on the same host that
						-- the pidlock was taken on,
						-- we know that the pidlock is
						-- stale, and can take it over.
						rename tmp' abslockfile
						tooklock tmpst
					_ -> failedlock

-- Linux's open(2) man page recommends linking a pid lock into place,
-- as the most portable atomic operation that will fail if
-- it already exists. 
--
-- open(2) suggests that link can sometimes appear to fail
-- on NFS but have actually succeeded, and the way to find out is to stat
-- the file and check its link count etc.
--
-- However, not all filesystems support hard links. So, first probe
-- to see if they are supported. If not, use open with O_EXCL.
linkToLock :: SideLockHandle -> RawFilePath -> RawFilePath -> IO (Maybe FileStatus)
linkToLock Nothing _ _ = return Nothing
linkToLock (Just _) src dest = do
	let probe = src <> ".lnk"
	v <- tryIO $ createLink src probe
	removeWhenExistsWith removeLink probe
	case v of
		Right _ -> do
			_ <- tryIO $ createLink src dest
			ifM (catchBoolIO checklinked)
				( ifM (catchBoolIO $ not <$> checkInsaneLustre dest)
					( catchMaybeIO $ getFileStatus dest
					, return Nothing
					)
				, return Nothing
				)
		Left _ -> catchMaybeIO $ do
			let setup = do
				fd <- openFdWithMode dest WriteOnly
					(Just $ combineModes readModes)
					(defaultFileFlags {exclusive = True})
				fdToHandle fd
			let cleanup = hClose
			let go h = readFile (fromRawFilePath src) >>= hPutStr h
			bracket setup cleanup go
			getFileStatus dest
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
checkInsaneLustre :: RawFilePath -> IO Bool
checkInsaneLustre dest = do
	fs <- dirContents (P.takeDirectory dest)
	case length (filter (== dest) fs) of
		1 -> return False -- whew!
		0 -> return True -- wtf?
		_ -> do
			-- Try to clean up the extra copy we made
			-- that has the same name. Egads.
			_ <- tryIO $ removeLink dest
			return True

-- | Waits as necessary to take a lock.
--
-- Uses a 1 second wait-loop, retrying until a timeout.
--
-- After the first second waiting, runs the callback to display a message,
-- so the user knows why it's stalled.
waitLock :: MonadIO m => Seconds -> PidLockFile -> (String -> m ()) -> (Bool -> IO ()) -> m LockHandle
waitLock (Seconds timeout) lockfile displaymessage sem = go timeout
  where
	go n
		| n > 0 = liftIO (tryLock lockfile) >>= \case
			Nothing -> do
				when (n == pred timeout) $
					displaymessage $ "waiting for pid lock file " ++ fromRawFilePath lockfile ++ " which is held by another process (or may be stale)"
				liftIO $ threadDelaySeconds (Seconds 1)
				go (pred n)
			Just lckh -> do
				liftIO $ sem True
				return lckh
		| otherwise = do
			liftIO $ sem False
			waitedLock (Seconds timeout) lockfile displaymessage

waitedLock :: MonadIO m => Seconds -> PidLockFile -> (String -> m ()) -> m a
waitedLock (Seconds timeout) lockfile displaymessage = do
	displaymessage $ show timeout ++ " second timeout exceeded while waiting for pid lock file " ++ fromRawFilePath lockfile
	giveup $ "Gave up waiting for pid lock file " ++ fromRawFilePath lockfile

-- | Use when the pid lock has already been taken by another thread of the
-- same process.
alreadyLocked :: MonadIO m => PidLockFile -> m LockHandle
alreadyLocked lockfile = liftIO $ do
	abslockfile <- absPath lockfile
	st <- getFileStatus abslockfile
	return $ LockHandle abslockfile st Nothing

dropLock :: LockHandle -> IO ()
dropLock (LockHandle lockfile _ sidelock) = do
	-- Drop side lock first, at which point the pid lock will be
	-- considered stale.
	dropSideLock sidelock
	removeWhenExistsWith removeLink lockfile
dropLock ParentLocked = return ()

getLockStatus :: PidLockFile -> IO LockStatus
getLockStatus = maybe StatusUnLocked (StatusLockedBy . lockingPid) <$$> readPidLock

checkLocked :: PidLockFile -> IO (Maybe Bool)
checkLocked lockfile = conv <$> getLockStatus lockfile
  where
	conv (StatusLockedBy _) = Just True
	conv _ = Just False

-- Checks that the lock file still exists, and is the same file that was
-- locked to get the LockHandle.
checkSaneLock :: PidLockFile -> LockHandle -> IO Bool
checkSaneLock lockfile (LockHandle _ st _) = 
	go =<< catchMaybeIO (getFileStatus lockfile)
  where
	go Nothing = return False
	go (Just st') = return $
		deviceID st == deviceID st' && fileID st == fileID st'
checkSaneLock _ ParentLocked = return True

-- | A parent process that is using pid locking can set this to 1 before
-- starting a child, to communicate to the child that it's holding the pid
-- lock and that the child can skip trying to take it, and not block
-- on the pid lock its parent is holding.
--
-- The parent process should keep running as long as the child
-- process is running, since the child inherits the environment and will
-- not see unsetLockEnv.
pidLockEnv :: RawFilePath -> IO String
pidLockEnv lockfile = do
	abslockfile <- fromRawFilePath <$> absPath lockfile
	return $ "PIDLOCK_" ++ filter legalInEnvVar abslockfile

pidLockEnvValue :: String
pidLockEnvValue = "1"
