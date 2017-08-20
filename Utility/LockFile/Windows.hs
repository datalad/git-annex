{- Windows lock files
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.LockFile.Windows (
	lockShared,
	lockExclusive,
	dropLock,
	waitToLock,
	LockHandle
) where

import System.Win32.Types
import System.Win32.File
import Control.Concurrent

type LockFile = FilePath

type LockHandle = HANDLE

{- Tries to lock a file with a shared lock, which allows other processes to
 - also lock it shared. Fails if the file is exclusively locked. -}
lockShared :: LockFile -> IO (Maybe LockHandle)
lockShared = openLock fILE_SHARE_READ

{- Tries to take an exclusive lock on a file. Fails if another process has
 - a shared or exclusive lock.
 -
 - Note that exclusive locking also prevents the file from being opened for
 - read or write by any other process. So for advisory locking of a file's
 - content, a separate LockFile should be used. -}
lockExclusive :: LockFile -> IO (Maybe LockHandle)
lockExclusive = openLock fILE_SHARE_NONE

{- Windows considers just opening a file enough to lock it. This will
 - create the LockFile if it does not already exist.
 -
 - Will fail if the file is already open with an incompatible ShareMode.
 - Note that this may happen if an unrelated process, such as a virus
 - scanner, even looks at the file. See http://support.microsoft.com/kb/316609
 -
 - Note that createFile busy-waits to try to avoid failing when some other
 - process briefly has a file open. But that would make checking locks
 - much more expensive, so is not done here. Thus, the use of c_CreateFile.
 -
 - Also, passing Nothing for SECURITY_ATTRIBUTES ensures that the lock file
 - is not inherited by any child process.
 -}
openLock :: ShareMode -> LockFile -> IO (Maybe LockHandle)
openLock sharemode f = do
	h <- withTString f $ \c_f ->
		c_CreateFile c_f gENERIC_READ sharemode security_attributes
			oPEN_ALWAYS fILE_ATTRIBUTE_NORMAL (maybePtr Nothing)
	return $ if h == iNVALID_HANDLE_VALUE
		then Nothing
		else Just h
  where
	security_attributes = maybePtr Nothing

dropLock :: LockHandle -> IO ()
dropLock = closeHandle

{- If the initial lock fails, this is a BUSY wait, and does not
 - guarentee FIFO order of waiters. In other news, Windows is a POS. -}
waitToLock :: IO (Maybe lockhandle) -> IO lockhandle
waitToLock locker = takelock
  where
	takelock = go =<< locker
	go (Just lck) = return lck
	go Nothing = do
		threadDelay (500000) -- half a second
		takelock
