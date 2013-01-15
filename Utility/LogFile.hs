{- log files
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.LogFile where

import Common

import System.Posix

openLog :: FilePath -> IO Fd
openLog logfile = do
	rotateLog logfile 0
	openFd logfile WriteOnly (Just stdFileMode)
		defaultFileFlags { append = True }

rotateLog :: FilePath -> Int -> IO ()
rotateLog logfile num
	| num > maxLogs = return ()
	| otherwise = whenM (doesFileExist currfile) $ do
		rotateLog logfile (num + 1)
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
