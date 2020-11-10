{- safely running shell commands
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.SafeCommand (
	CommandParam(..),
	toCommand,
	boolSystem,
	boolSystem',
	boolSystemEnv,
	safeSystem,
	safeSystem',
	safeSystemEnv,
	segmentXargsOrdered,
	segmentXargsUnordered,
) where

import Utility.Process

import System.Exit
import System.FilePath
import Data.Char
import Data.List
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
toCommand = map toCommand'

toCommand' :: CommandParam -> String
toCommand' (Param s) = s
-- Files that start with a non-alphanumeric that is not a path
-- separator are modified to avoid the command interpreting them as
-- options or other special constructs.
toCommand' (File s@(h:_))
	| isAlphaNum h || h `elem` pathseps = s
	| otherwise = "./" ++ s
  where
	-- '/' is explicitly included because it's an alternative
	-- path separator on Windows.
	pathseps = pathSeparator:"./"
toCommand' (File s) = s

-- | Run a system command, and returns True or False if it succeeded or failed.
--
-- (Throws an exception if the command is not found.)
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
safeSystem' command params mkprocess = 
	withCreateProcess p $ \_ _ _ pid ->
		waitForProcess pid
  where
	p = mkprocess $ proc command (toCommand params)

safeSystemEnv :: FilePath -> [CommandParam] -> Maybe [(String, String)] -> IO ExitCode
safeSystemEnv command params environ = safeSystem' command params $ 
	\p -> p { env = environ }

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
