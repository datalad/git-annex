{- Windows lock files
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.WinLock (
	lockShared,
	lockExclusive,
	dropLock,
	waitToLock
) where

import Common

import System.Win32.Types
import System.Win32.File
import Control.Concurrent

{- Locking is exclusive, and prevents the file from being opened for read
 - or write by any other process. So for advisory locking of a file, a
 - different LockFile should be used. -}
type LockFile = FilePath

type LockHandle = HANDLE

{- Tries to lock a file with a shared lock, which allows other processes to
 - also lock it shared. Fails is the file is exclusively locked. -}
lockShared :: LockFile -> IO (Maybe LockHandle)
lockShared = openLock fILE_SHARE_READ

{- Tries to take an exclusive lock on a file. Fails if another process has
 - a shared or exclusive lock. -}
lockExclusive :: LockFile -> IO (Maybe LockHandle)
lockExclusive = openLock fILE_SHARE_NONE

{- Windows considers just opening a file enough to lock it. This will
 - create the LockFile if it does not already exist.
 -
 - Will fail if the file is already open with an incompatable ShareMode.
 - Note that this may happen if an unrelated process, such as a virus
 - scanner, even looks at the file. See http://support.microsoft.com/kb/316609
 -
 - Note that createFile busy-waits to try to avoid failing when some other
 - process briefly has a file open. But that would make checking locks
 - much more expensive, so is not done here. Thus, the use of c_CreateFile.
 -}
openLock :: ShareMode -> LockFile -> IO (Maybe LockHandle)
openLock sharemode f = do
	h <- withTString f $ \c_f ->
		c_CreateFile c_f gENERIC_READ sharemode (maybePtr Nothing)
			oPEN_ALWAYS fILE_ATTRIBUTE_NORMAL (maybePtr Nothing)
	return $ if h == iNVALID_HANDLE_VALUE
		then Nothing
		else Just h

dropLock :: LockHandle -> IO ()
dropLock = closeHandle

{- If the initial lock fails, this is a BUSY wait, and does not
 - guarentee FIFO order of waiters. In other news, Windows is a POS. -}
waitToLock :: IO (Maybe LockHandle) -> IO LockHandle
waitToLock locker = takelock
  where
	takelock = go =<< locker
	go (Just lck) = return lck
	go Nothing = do
		threadDelay (500000) -- half a second
		takelock
