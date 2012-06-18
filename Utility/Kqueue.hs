{- BSD kqueue file modification notification interface
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Utility.Kqueue ( waitChange ) where

import Common

import System.Posix.Types
import Foreign.C.Types
import Foreign.C.Error
import Foreign.Ptr
import Foreign.Marshal

foreign import ccall unsafe "libkqueue.h waitchange" c_waitchange
	:: Ptr Fd -> IO Fd

waitChange :: [Fd] -> IO (Maybe Fd)
waitChange fds = withArray fds $ \c_fds -> do
	ret <- c_waitchange c_fds
	ifM (safeErrno <$> getErrno)
		( return $ Just ret
		, return Nothing
		)
	where
		safeErrno (Errno v) = v == 0
