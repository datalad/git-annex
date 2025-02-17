{- git-annex multicast receive callback
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Multicast where

import Common
import Annex.Path
import Utility.Env

import System.Process
import GHC.IO.Handle.FD

multicastReceiveEnv :: String
multicastReceiveEnv = "GIT_ANNEX_MULTICAST_RECEIVE"

multicastCallbackEnv :: IO (OsPath, [(String, String)], Handle)
multicastCallbackEnv = do
	gitannex <- programPath
	-- This will even work on Windows
	(rfd, wfd) <- createPipeFd
	rh <- fdToHandle rfd
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
		mapM_ (hPutStrLn h) fs
		hClose h
	Nothing -> return ()
runMulticastReceive _ _ = return ()
