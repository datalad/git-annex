{- lsof interface
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Utility.Lsof where

import Common

import System.Posix.Types

data LsofOpenMode = OpenReadWrite | OpenReadOnly | OpenWriteOnly | OpenUnknown
	deriving (Show, Eq)

type CmdLine = String

data ProcessInfo = ProcessInfo ProcessID CmdLine
	deriving (Show)

{- Checks each of the files in a directory to find open files.
 - Note that this will find hard links to files elsewhere that are open. -}
queryDir :: FilePath -> IO [(FilePath, LsofOpenMode, ProcessInfo)]
queryDir path = query ["+d", path]

{- Runs lsof with some parameters.
 -
 - Ignores nonzero exit code; lsof returns that when no files are open.
 -
 - Note: If lsof is not available, this always returns [] !
 -}
query :: [String] -> IO [(FilePath, LsofOpenMode, ProcessInfo)]
query opts =
	withHandle StdoutHandle (createProcessChecked checkSuccessProcess) p $ \h -> do
		parse <$> hGetContentsStrict h
	where
		p = proc "lsof" ("-F0can" : opts)

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
parse :: String -> [(FilePath, LsofOpenMode, ProcessInfo)]
parse s = bundle $ go [] $ lines s
	where
		bundle = concatMap (\(fs, p) -> map (\(f, m) -> (f, m, p)) fs)

		go c [] = c
		go c ((t:r):ls)
			| t == 'p' =
				let (fs, ls') = parsefiles [] ls
				in go ((fs, parseprocess r):c) ls'
			| otherwise = parsefail
		go _ _ = parsefail

		parseprocess l =
			case splitnull l of
				[pid, 'c':cmdline, ""] ->
					case readish pid of
						(Just n) -> ProcessInfo n cmdline
						Nothing -> parsefail
				_ -> parsefail

		parsefiles c [] = (c, [])
		parsefiles c (l:ls) = 
			case splitnull l of
				['a':mode, 'n':file, ""] ->
					parsefiles ((file, parsemode mode):c) ls
				(('p':_):_) -> (c, l:ls)
				_ -> parsefail

		parsemode ('r':_) = OpenReadOnly
		parsemode ('w':_) = OpenWriteOnly
		parsemode ('u':_) = OpenReadWrite
		parsemode _ = OpenUnknown

		splitnull = split "\0"

		parsefail = error $ "failed to parse lsof output: " ++ show s
