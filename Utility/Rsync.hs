{- various rsync stuff
 -
 - Copyright 2010-2013 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.Rsync where

import Common
import Utility.Metered

import Data.Char
import System.Console.GetOpt
import Data.Tuple.Utils

{- Generates parameters to make rsync use a specified command as its remote
 - shell. -}
rsyncShell :: [CommandParam] -> [CommandParam]
rsyncShell command = [Param "-e", Param $ unwords $ map escape (toCommand command)]
  where
	{- rsync requires some weird, non-shell like quoting in
	- here. A doubled single quote inside the single quoted
	- string is a single quote. -}
	escape s = "'" ++  intercalate "''" (splitc '\'' s) ++ "'"

{- Runs rsync in server mode to send a file. -}
rsyncServerSend :: [CommandParam] -> FilePath -> IO Bool
rsyncServerSend options file = rsync $
	rsyncServerParams ++ Param "--sender" : options ++ [File file]

{- Runs rsync in server mode to receive a file. -}
rsyncServerReceive :: [CommandParam] -> FilePath -> IO Bool
rsyncServerReceive options file = rsync $
	rsyncServerParams ++ options ++ [File file]

rsyncServerParams :: [CommandParam]
rsyncServerParams =
	[ Param "--server"
	-- preserve timestamps
	, Param "-t"
	-- allow resuming of transfers of big files
	, Param "--inplace"
	-- other options rsync normally uses in server mode
	, Param "-e.Lsf"
	, Param "."
	]

rsyncUseDestinationPermissions :: CommandParam
rsyncUseDestinationPermissions = Param "--chmod=ugo=rwX"

rsync :: [CommandParam] -> IO Bool
rsync = boolSystem "rsync" . rsyncParamsFixup

{- On Windows, rsync is from msys2, and expects to get msys2 formatted
 - paths to files. (It thinks that C:foo refers to a host named "C").
 - Fix up the Params appropriately. -}
rsyncParamsFixup :: [CommandParam] -> [CommandParam]
#ifdef mingw32_HOST_OS
rsyncParamsFixup = map fixup
  where
	fixup (File f) = File (toMSYS2Path f)
	fixup (Param s)
		| rsyncUrlIsPath s = Param (toMSYS2Path s)
	fixup p = p
#else
rsyncParamsFixup = id
#endif

{- Checks if an rsync url involves the remote shell (ssh or rsh).
 - Use of such urls with rsync requires additional shell
 - escaping. -}
rsyncUrlIsShell :: String -> Bool
rsyncUrlIsShell s
	| "rsync://" `isPrefixOf` s = False
	| otherwise = go s
  where
	-- host::dir is rsync protocol, while host:dir is ssh/rsh
	go [] = False
	go (c:cs)
		| c == '/' = False -- got to directory with no colon
		| c == ':' = not $ ":" `isPrefixOf` cs
		| otherwise = go cs

{- Checks if a rsync url is really just a local path. -}
rsyncUrlIsPath :: String -> Bool
rsyncUrlIsPath s
#ifdef mingw32_HOST_OS
	| not (null (takeDrive s)) = True
#endif
	| rsyncUrlIsShell s = False
	| otherwise = ':' `notElem` s

{- Runs rsync, but intercepts its progress output and updates a progress
 - meter.
 -
 - The params must enable rsync's --progress mode for this to work.
 -}
rsyncProgress :: OutputHandler -> MeterUpdate -> [CommandParam] -> IO Bool
rsyncProgress oh meter = commandMeter parseRsyncProgress oh meter "rsync" . rsyncParamsFixup

{- Strategy: Look for chunks prefixed with \r (rsync writes a \r before
 - the first progress output, and each thereafter). The first number
 - after the \r is the number of bytes processed. After the number,
 - there must appear some whitespace, or we didn't get the whole number,
 - and return the \r and part we did get, for later processing.
 -
 - In some locales, the number will have one or more commas in the middle
 - of it.
 -}
parseRsyncProgress :: ProgressParser
parseRsyncProgress = go [] . reverse . progresschunks
  where
	go remainder [] = (Nothing, remainder)
	go remainder (x:xs) = case parsebytes (findbytesstart x) of
		Nothing -> go (delim:x++remainder) xs
		Just b -> (Just (toBytesProcessed b), remainder)

	delim = '\r'

	{- Find chunks that each start with delim.
	 - The first chunk doesn't start with it
	 - (it's empty when delim is at the start of the string). -}
	progresschunks = drop 1 . splitc delim
	findbytesstart s = dropWhile isSpace s

	parsebytes :: String -> Maybe Integer
	parsebytes s = case break isSpace s of
		([], _) -> Nothing
		(_, []) -> Nothing
		(b, _) -> readish $ filter (/= ',') b

{- Filters options to those that are safe to pass to rsync in server mode,
 - without causing it to eg, expose files. -}
filterRsyncSafeOptions :: [String] -> [String]
filterRsyncSafeOptions = fst3 . getOpt Permute
	[ Option [] ["bwlimit"] (reqArgLong "bwlimit") "" ]
  where
	reqArgLong x = ReqArg (\v -> "--" ++ x ++ "=" ++ v) ""
