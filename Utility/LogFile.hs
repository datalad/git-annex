{- log files
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.LogFile (
	openLog,
	listLogs,
	maxLogs,
#ifndef mingw32_HOST_OS
	redirLog,
	redir,
#endif
) where

import Common

#ifndef mingw32_HOST_OS
import System.Posix.Types
import System.Posix.IO
#endif

openLog :: FilePath -> IO Handle
openLog logfile = do
	rotateLog logfile
	openFile logfile AppendMode

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
