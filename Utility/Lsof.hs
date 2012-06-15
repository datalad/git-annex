{- lsof interface
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Utility.Lsof where

import Common

import System.Cmd.Utils
import System.Posix.Types

data OpenMode = ReadWrite | ReadOnly | WriteOnly | Unknown

type CmdLine = String

data ProcessInfo = ProcessInfo ProcessID CmdLine

query :: FilePath -> IO [(FilePath, OpenMode, ProcessInfo)]
query p = do
	(h, s) <- pipeFrom "lsof" ["-F0can", "--", p]
	!r <- parse s
	forceSuccess h
	return r

{- Parsing null-delimited output like:
 -
 - pPID\0cCMDLINE\0
 - aMODE\0nFILE\0
 - aMODE\0nFILE\0
 - pPID\0[...]
 -
 - Where each new process block is started by a pid, and a process can
 - have multiple files open.
 -}
parse :: String -> [(FilePath, OpenMode, ProcessInfo)]
parse s = go [] $ lines s
	where
		go c [] = c
		go c (l@(t:r):ls)
			| t == 'p' = parseprocess r
			| otherwise = parsefail
		go _ _ = parsefail

		parseprocess l =
			case splitnull l of
				[pid, 'c':cmdline] ->
					case readish pid of
						(Just n) -> ProcessInfo n cmdline
						Nothing -> parsefail
				_ -> parsefail

		parsefile l =
			case splitnull l of
				['a':mode, 'n':file] -> (file, parsemode mode)
				_ -> parsefail

		parsemode ('r':_) = ReadOnly
		parsemode ('w':_) = WriteOnly
		parsemode ('u':_) = ReadWrite
		parsemode _ = Unknown

		ls = lines s

		splitnull = split "\0"

		parsefail = error "failed to parse lsof output: " ++ show s
