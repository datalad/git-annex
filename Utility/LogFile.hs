{- log files
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.LogFile where

import Common

import System.Posix.Types
import System.PosixCompat.Files

openLog :: FilePath -> IO Fd
#ifndef __WINDOWS__
openLog logfile = do
	rotateLog logfile
	openFd logfile WriteOnly (Just stdFileMode)
		defaultFileFlags { append = True }
#else
openLog = error "openLog TODO"
#endif

rotateLog :: FilePath -> IO ()
rotateLog logfile = go 0
  where
	go num
		| num > maxLogs = return ()
		| otherwise = whenM (doesFileExist currfile) $ do
			go (num + 1)
			renameFile currfile nextfile
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

redirLog :: Fd -> IO ()
#ifndef __WINDOWS__
redirLog logfd = do
	mapM_ (redir logfd) [stdOutput, stdError]
	closeFd logfd
#else
redirLog _ = error "redirLog TODO"
#endif

#ifndef __WINDOWS__
redir :: Fd -> Fd -> IO ()
redir newh h = do
	closeFd h
	void $ dupTo newh h
#else
redir _ _ = error "redir TODO"
#endif
