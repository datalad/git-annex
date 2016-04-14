{- git lock files
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Git.LockFile where

import Common

#ifndef mingw32_HOST_OS
import System.Posix.Types
#else
import System.Win32.Types
import System.Win32.File
#endif

#ifndef mingw32_HOST_OS
data LockHandle = LockHandle FilePath Fd
#else
data LockHandle = LockHandle FilePath HANDLE
#endif

{- Uses the same exclusive locking that git does.
 - Throws an IO exception if the file is already locked.
 -
 - Note that git's locking method suffers from the problem that
 - a dangling lock can be left if a process is terminated at the wrong
 - time.
 -}
openLock :: FilePath -> IO LockHandle
openLock lck = openLock' lck `catchNonAsync` lckerr
  where
	lckerr e = do
		-- Same error message displayed by git.
		whenM (doesFileExist lck) $
			hPutStrLn stderr $ unlines
				[ "fatal: Unable to create '" ++ lck ++ "': " ++ show e
				, ""
				, "If no other git process is currently running, this probably means a"
				, "git process crashed in this repository earlier. Make sure no other git"
				, "process is running and remove the file manually to continue."
				]
		throwM e

openLock' :: FilePath -> IO LockHandle
openLock' lck = do
#ifndef mingw32_HOST_OS
	-- On unix, git simply uses O_EXCL
	h <- openFd lck ReadWrite (Just 0O666)
		(defaultFileFlags { exclusive = True })
	setFdOption h CloseOnExec True
#else
	-- It's not entirely clear how git manages locking on Windows,
	-- since it's buried in the portability layer, and different
	-- versions of git for windows use different portability layers.
	-- But, we can be fairly sure that holding the lock file open on
	-- windows is enough to prevent another process from opening it.
	--
	-- So, all that's needed is a way to open the file, that fails
	-- if the file already exists. Using CreateFile with CREATE_NEW 
	-- accomplishes that.
	h <- createFile lck gENERIC_WRITE fILE_SHARE_NONE Nothing
		cREATE_NEW fILE_ATTRIBUTE_NORMAL Nothing
#endif
	return (LockHandle lck h)

closeLock :: LockHandle -> IO ()
closeLock (LockHandle lck h) = do
#ifndef mingw32_HOST_OS
	closeFd h
#else
	closeHandle h
#endif
	removeFile lck
