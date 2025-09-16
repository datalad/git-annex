{- git-annex multicast receive callback
 -
 - Copyright 2017-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Multicast where

import Common
import Annex.Path
import Utility.Env

#ifndef mingw32_HOST_OS
import System.Posix.IO
#else
import System.Process (createPipeFd)
#endif
import GHC.IO.Encoding (getLocaleEncoding)

multicastReceiveEnv :: String
multicastReceiveEnv = "GIT_ANNEX_MULTICAST_RECEIVE"

multicastCallbackEnv :: IO (OsPath, [(String, String)], Handle)
multicastCallbackEnv = do
	gitannex <- programPath
#ifndef mingw32_HOST_OS
	(rfd, wfd) <- noCreateProcessWhile $ do
		(rfd, wfd) <- createPipe
		setFdOption rfd CloseOnExec True
		return (rfd, wfd)
#else
	(rfd, wfd) <- createPipeFd
#endif
	rh <- fdToHandle rfd
	getLocaleEncoding >>= hSetEncoding rh
	environ <- addEntry multicastReceiveEnv (show wfd) <$> getEnvironment
	return (gitannex, environ, rh)

-- This is run when uftpd has received a file. Rather than move
-- the file into the annex here, which would require starting up the
-- Annex monad, parsing git config, and verifying the content, simply
-- output to the specified FD the filename. This keeps the time
-- that uftpd is not receiving the next file as short as possible.
runMulticastReceive :: [String] -> String -> IO ()
runMulticastReceive ("-I":_sessionid:fs) hs = case readish hs of
	Just fd -> do
		h <- fdToHandle fd
		getLocaleEncoding >>= hSetEncoding h
		mapM_ (hPutStrLn h) fs
		hClose h
	Nothing -> return ()
runMulticastReceive _ _ = return ()
