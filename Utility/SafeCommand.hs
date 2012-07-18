{- safely running shell commands
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.SafeCommand where

import System.Exit
import System.Process
import Data.String.Utils
import Control.Applicative

{- A type for parameters passed to a shell command. A command can
 - be passed either some Params (multiple parameters can be included,
 - whitespace-separated, or a single Param (for when parameters contain
 - whitespace), or a File.
 -}
data CommandParam = Params String | Param String | File FilePath
	deriving (Eq, Show, Ord)

{- Used to pass a list of CommandParams to a function that runs
 - a command and expects Strings. -}
toCommand :: [CommandParam] -> [String]
toCommand = (>>= unwrap)
	where
		unwrap (Param s) = [s]
		unwrap (Params s) = filter (not . null) (split " " s)
		-- Files that start with a dash are modified to avoid
		-- the command interpreting them as options.
		unwrap (File s@('-':_)) = ["./" ++ s]
		unwrap (File s) = [s]

{- Run a system command, and returns True or False
 - if it succeeded or failed.
 -}
boolSystem :: FilePath -> [CommandParam] -> IO Bool
boolSystem command params = boolSystemEnv command params Nothing

boolSystemEnv :: FilePath -> [CommandParam] -> Maybe [(String, String)] -> IO Bool
boolSystemEnv command params environ = dispatch <$> safeSystemEnv command params environ
	where
		dispatch ExitSuccess = True
		dispatch _ = False

{- Runs a system command, returning the exit status. -}
safeSystem :: FilePath -> [CommandParam] -> IO ExitCode
safeSystem command params = safeSystemEnv command params Nothing

{- Unlike many implementations of system, SIGINT(ctrl-c) is allowed
 - to propigate and will terminate the program. -}
safeSystemEnv :: FilePath -> [CommandParam] -> Maybe [(String, String)] -> IO ExitCode
safeSystemEnv command params environ = do
	(_, _, _, pid) <- createProcess (proc command $ toCommand params)
		{ env = environ }
	waitForProcess pid

{- Escapes a filename or other parameter to be safely able to be exposed to
 - the shell. -}
shellEscape :: String -> String
shellEscape f = "'" ++ escaped ++ "'"
	where
		-- replace ' with '"'"'
		escaped = join "'\"'\"'" $ split "'" f

{- Unescapes a set of shellEscaped words or filenames. -}
shellUnEscape :: String -> [String]
shellUnEscape [] = []
shellUnEscape s = word : shellUnEscape rest
	where
		(word, rest) = findword "" s
		findword w [] = (w, "")
		findword w (c:cs)
			| c == ' ' = (w, cs)
			| c == '\'' = inquote c w cs
			| c == '"' = inquote c w cs
			| otherwise = findword (w++[c]) cs
		inquote _ w [] = (w, "")
		inquote q w (c:cs)
			| c == q = findword w cs
			| otherwise = inquote q (w++[c]) cs

{- For quickcheck. -}
prop_idempotent_shellEscape :: String -> Bool
prop_idempotent_shellEscape s = [s] == (shellUnEscape . shellEscape) s
prop_idempotent_shellEscape_multiword :: [String] -> Bool
prop_idempotent_shellEscape_multiword s = s == (shellUnEscape . unwords . map shellEscape) s
