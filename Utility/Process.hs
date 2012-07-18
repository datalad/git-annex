{- System.Process enhancements
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Process where

import System.Process
import System.Exit
import System.IO

import Utility.Misc

{- Waits for a ProcessHandle, and throws an exception if the process
 - did not exit successfully. -}
forceSuccessProcess :: ProcessHandle -> String -> [String] -> IO ()
forceSuccessProcess pid cmd args = do
	code <- waitForProcess pid
	case code of
		ExitSuccess -> return ()
		ExitFailure n -> error $
			cmd ++ " " ++ show args ++ " exited " ++ show n

{- Like readProcess, but allows specifying the environment, and does
 - not mess with stdin. -}
readProcessEnv :: FilePath -> [String] -> Maybe [(String, String)] -> IO String
readProcessEnv cmd args environ = do
	(_, Just h, _, pid)
		<- createProcess (proc cmd args)
			{ std_in  = Inherit
			, std_out = CreatePipe
			, std_err = Inherit
			, env = environ
			}
	output  <- hGetContentsStrict h
	hClose h
	forceSuccessProcess pid cmd args
	return output
