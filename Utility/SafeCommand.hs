{- safely running shell commands
 -
 - Copyright 2010-2013 Joey Hess <joey@kitenet.net>
 -
 - License: BSD-2-clause
 -}

module Utility.SafeCommand where

import System.Exit
import Utility.Process
import Data.String.Utils
import Control.Applicative
import System.FilePath
import Data.Char

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
toCommand = concatMap unwrap
  where
	unwrap (Param s) = [s]
	unwrap (Params s) = filter (not . null) (split " " s)
	-- Files that start with a non-alphanumeric that is not a path
	-- separator are modified to avoid the command interpreting them as
	-- options or other special constructs.
	unwrap (File s@(h:_))
		| isAlphaNum h || h `elem` pathseps = [s]
		| otherwise = ["./" ++ s]
	unwrap (File s) = [s]
	-- '/' is explicitly included because it's an alternative
	-- path separator on Windows.
	pathseps = pathSeparator:"./"

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

safeSystemEnv :: FilePath -> [CommandParam] -> Maybe [(String, String)] -> IO ExitCode
safeSystemEnv command params environ = do
	(_, _, _, pid) <- createProcess (proc command $ toCommand params)
		{ env = environ }
	waitForProcess pid

{- Wraps a shell command line inside sh -c, allowing it to be run in a
 - login shell that may not support POSIX shell, eg csh. -}
shellWrap :: String -> String
shellWrap cmdline = "sh -c " ++ shellEscape cmdline

{- Escapes a filename or other parameter to be safely able to be exposed to
 - the shell.
 -
 - This method works for POSIX shells, as well as other shells like csh.
 -}
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

{- Segements a list of filenames into groups that are all below the manximum
 - command-line length limit. Does not preserve order. -}
segmentXargs :: [FilePath] -> [[FilePath]]
segmentXargs l = go l [] 0 []
  where
	go [] c _ r = c:r
	go (f:fs) c accumlen r
		| len < maxlen && newlen > maxlen = go (f:fs) [] 0 (c:r)
		| otherwise = go fs (f:c) newlen r
	  where
		len = length f
		newlen = accumlen + len

	{- 10k of filenames per command, well under Linux's 20k limit;
	 - allows room for other parameters etc. -}
	maxlen = 10240
