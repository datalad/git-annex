{- log files
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.LogFile where

import Common

#ifndef mingw32_HOST_OS
import System.Posix.Types
#endif

#ifndef mingw32_HOST_OS
openLog :: FilePath -> IO Fd
openLog logfile = do
	rotateLog logfile
	openFd logfile WriteOnly (Just stdFileMode)
		defaultFileFlags { append = True }
#endif

rotateLog :: FilePath -> IO ()
rotateLog logfile = go 0
  where
	go num
		| num > maxLogs = return ()
		| otherwise = whenM (doesFileExist currfile) $ do
			go (num + 1)
			rename currfile nextfile
	  where
		currfile = filename num
		nextfile = filename (num + 1)
		filename n
			| n == 0 = logfile
			| otherwise = rotatedLog logfile n

rotatedLog :: FilePath -> Int -> FilePath
rotatedLog logfile n = logfile ++ "." ++ show n

{- Lists most recent logs last. -}
listLogs :: FilePath -> IO [FilePath]
listLogs logfile = filterM doesFileExist $ reverse $ 
	logfile : map (rotatedLog logfile) [1..maxLogs]

maxLogs :: Int
maxLogs = 9

#ifndef mingw32_HOST_OS
redirLog :: Fd -> IO ()
redirLog logfd = do
	mapM_ (redir logfd) [stdOutput, stdError]
	closeFd logfd

redir :: Fd -> Fd -> IO ()
redir newh h = do
	closeFd h
	void $ dupTo newh h
#endif
