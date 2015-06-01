{- safely running shell commands
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.SafeCommand where

import System.Exit
import Utility.Process
import Data.String.Utils
import System.FilePath
import Data.Char
import Control.Applicative
import Prelude

-- | Parameters that can be passed to a shell command.
data CommandParam
	= Param String -- ^ A parameter
	| File FilePath -- ^ The name of a file
	deriving (Eq, Show, Ord)

-- | Used to pass a list of CommandParams to a function that runs
-- a command and expects Strings. -}
toCommand :: [CommandParam] -> [String]
toCommand = map unwrap
  where
	unwrap (Param s) = s
	-- Files that start with a non-alphanumeric that is not a path
	-- separator are modified to avoid the command interpreting them as
	-- options or other special constructs.
	unwrap (File s@(h:_))
		| isAlphaNum h || h `elem` pathseps = s
		| otherwise = "./" ++ s
	unwrap (File s) = s
	-- '/' is explicitly included because it's an alternative
	-- path separator on Windows.
	pathseps = pathSeparator:"./"

-- | Run a system command, and returns True or False if it succeeded or failed.
--
-- This and other command running functions in this module log the commands
-- run at debug level, using System.Log.Logger.
boolSystem :: FilePath -> [CommandParam] -> IO Bool
boolSystem command params = boolSystem' command params id

boolSystem' :: FilePath -> [CommandParam] -> (CreateProcess -> CreateProcess) -> IO Bool
boolSystem' command params mkprocess = dispatch <$> safeSystem' command params mkprocess
  where
	dispatch ExitSuccess = True
	dispatch _ = False

boolSystemEnv :: FilePath -> [CommandParam] -> Maybe [(String, String)] -> IO Bool
boolSystemEnv command params environ = boolSystem' command params $
	\p -> p { env = environ }

-- | Runs a system command, returning the exit status.
safeSystem :: FilePath -> [CommandParam] -> IO ExitCode
safeSystem command params = safeSystem' command params id

safeSystem' :: FilePath -> [CommandParam] -> (CreateProcess -> CreateProcess) -> IO ExitCode
safeSystem' command params mkprocess = do
	(_, _, _, pid) <- createProcess p
	waitForProcess pid
  where
	p = mkprocess $ proc command (toCommand params)

safeSystemEnv :: FilePath -> [CommandParam] -> Maybe [(String, String)] -> IO ExitCode
safeSystemEnv command params environ = safeSystem' command params $ 
	\p -> p { env = environ }

-- | Wraps a shell command line inside sh -c, allowing it to be run in a
-- login shell that may not support POSIX shell, eg csh.
shellWrap :: String -> String
shellWrap cmdline = "sh -c " ++ shellEscape cmdline

-- | Escapes a filename or other parameter to be safely able to be exposed to
-- the shell.
--
-- This method works for POSIX shells, as well as other shells like csh.
shellEscape :: String -> String
shellEscape f = "'" ++ escaped ++ "'"
  where
	-- replace ' with '"'"'
	escaped = join "'\"'\"'" $ split "'" f

-- | Unescapes a set of shellEscaped words or filenames.
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

-- | For quickcheck.
prop_idempotent_shellEscape :: String -> Bool
prop_idempotent_shellEscape s = [s] == (shellUnEscape . shellEscape) s
prop_idempotent_shellEscape_multiword :: [String] -> Bool
prop_idempotent_shellEscape_multiword s = s == (shellUnEscape . unwords . map shellEscape) s

-- | Segments a list of filenames into groups that are all below the maximum
--  command-line length limit.
segmentXargsOrdered :: [FilePath] -> [[FilePath]]
segmentXargsOrdered = reverse . map reverse . segmentXargsUnordered

-- | Not preserving order is a little faster, and streams better when
-- there are a great many filenames.
segmentXargsUnordered :: [FilePath] -> [[FilePath]]
segmentXargsUnordered l = go l [] 0 []
  where
	go [] c _ r = (c:r)
	go (f:fs) c accumlen r
		| newlen > maxlen && len < maxlen = go (f:fs) [] 0 (c:r)
		| otherwise = go fs (f:c) newlen r
	  where
		len = length f
		newlen = accumlen + len

	{- 10k of filenames per command, well under 100k limit
	 - of Linux (and OSX has a similar limit);
	 - allows room for other parameters etc. Also allows for
	 - eg, multibyte characters. -}
	maxlen = 10240
