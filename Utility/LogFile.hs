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
	| num >= 10 = return ()
	| otherwise = whenM (doesFileExist currfile) $ do
		rotateLog logfile (num + 1)
		renameFile currfile nextfile
	where
		currfile = filename num
		nextfile = filename (num + 1)
		filename n
			| n == 0 = logfile
			| otherwise = logfile ++ "." ++ show n
