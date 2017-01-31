{- lsof interface
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.Lsof where

import Common
import Build.SysConfig as SysConfig
import Utility.Env

import System.Posix.Types

data LsofOpenMode = OpenReadWrite | OpenReadOnly | OpenWriteOnly | OpenUnknown
	deriving (Show, Eq)

type CmdLine = String

data ProcessInfo = ProcessInfo ProcessID CmdLine
	deriving (Show)

{- lsof is not in PATH on all systems, so SysConfig may have the absolute
 - path where the program was found. Make sure at runtime that lsof is
 - available, and if it's not in PATH, adjust PATH to contain it. -}
setup :: IO ()
setup = do
	let cmd = fromMaybe "lsof" SysConfig.lsof
	when (isAbsolute cmd) $ do
		path <- getSearchPath
		let path' = takeDirectory cmd : path
		setEnv "PATH" (intercalate [searchPathSeparator] path') True

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
	withHandle StdoutHandle (createProcessChecked checkSuccessProcess) p $
		parse <$$> hGetContentsStrict
  where
	p = proc "lsof" ("-F0can" : opts)

type LsofParser = String -> [(FilePath, LsofOpenMode, ProcessInfo)]

parse :: LsofParser
#ifdef __ANDROID__
parse = parseDefault
#else
parse = parseFormatted
#endif

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
parseFormatted :: LsofParser
parseFormatted s = bundle $ go [] $ lines s
  where
	bundle = concatMap (\(fs, p) -> map (\(f, m) -> (f, m, p)) fs)

	go c [] = c
	go c ((t:r):ls)
		| t == 'p' =
			let (fs, ls') = parsefiles [] ls
			in go ((fs, parseprocess r):c) ls'
		| otherwise = parsefail
	go _ _ = parsefail

	parseprocess l = case splitnull l of
		[pid, 'c':cmdline, ""] ->
			case readish pid of
				(Just n) -> ProcessInfo n cmdline
				Nothing -> parsefail
		_ -> parsefail

	parsefiles c [] = (c, [])
	parsefiles c (l:ls) = parsefiles' c (splitnull l) l ls

	parsefiles' c ['a':mode, 'n':file, ""] _ ls =
		parsefiles ((file, parsemode mode):c) ls
	parsefiles' c (('p':_):_) l ls = (c, l:ls)
	-- Some buggy versions of lsof emit a f field
	-- that was not requested, so ignore it.
	parsefiles' c (('f':_):rest) l ls = parsefiles' c rest l ls
	parsefiles' _ _ _ _ = parsefail

	parsemode ('r':_) = OpenReadOnly
	parsemode ('w':_) = OpenWriteOnly
	parsemode ('u':_) = OpenReadWrite
	parsemode _ = OpenUnknown

	splitnull = splitc '\0'

	parsefail = error $ "failed to parse lsof output: " ++ show s

{- Parses lsof's default output format. -}
parseDefault :: LsofParser
parseDefault = mapMaybe parseline . drop 1 . lines
  where
	parseline l = case words l of
		(command : spid : _user : _fd : _type : _device : _size : _node : rest) -> 
			case readish spid of
				Nothing -> Nothing
				Just pid -> Just (unwords rest, OpenUnknown, ProcessInfo pid command)
		_ -> Nothing
