{- Process transcript
 -
 - Copyright 2012-2018 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Process.Transcript where

import Utility.Process
import Utility.Misc

import System.IO
import System.Exit
import Control.Concurrent.Async
import Control.Monad
#ifndef mingw32_HOST_OS
import qualified System.Posix.IO
#else
import Control.Applicative
#endif
import Data.Maybe
import Prelude

-- | Runs a process and returns a transcript combining its stdout and
-- stderr, and whether it succeeded or failed.
processTranscript :: String -> [String] -> (Maybe String) -> IO (String, Bool)
processTranscript cmd opts = processTranscript' (proc cmd opts)

-- | Also feeds the process some input.
processTranscript' :: CreateProcess -> Maybe String -> IO (String, Bool)
processTranscript' cp input = do
	(t, c) <- processTranscript'' cp input
	return (t, c == ExitSuccess)

processTranscript'' :: CreateProcess -> Maybe String -> IO (String, ExitCode)
processTranscript'' cp input = do
#ifndef mingw32_HOST_OS
{- This implementation interleves stdout and stderr in exactly the order
 - the process writes them. -}
	(readf, writef) <- System.Posix.IO.createPipe
	System.Posix.IO.setFdOption readf System.Posix.IO.CloseOnExec True
	System.Posix.IO.setFdOption writef System.Posix.IO.CloseOnExec True
	readh <- System.Posix.IO.fdToHandle readf
	writeh <- System.Posix.IO.fdToHandle writef
	p@(_, _, _, pid) <- createProcess $ cp
		{ std_in = if isJust input then CreatePipe else Inherit
		, std_out = UseHandle writeh
		, std_err = UseHandle writeh
		}
	hClose writeh

	get <- asyncreader readh
	writeinput input p
	transcript <- wait get
#else
{- This implementation for Windows puts stderr after stdout. -}
	p@(_, _, _, pid) <- createProcess $ cp
		{ std_in = if isJust input then CreatePipe else Inherit
		, std_out = CreatePipe
		, std_err = CreatePipe
		}

	getout <- asyncreader (stdoutHandle p)
	geterr <- asyncreader (stderrHandle p)
	writeinput input p
	transcript <- (++) <$> wait getout <*> wait geterr
#endif
	code <- waitForProcess pid
	return (transcript, code)
  where
	asyncreader = async . hGetContentsStrict

	writeinput (Just s) p = do
		let inh = stdinHandle p
		unless (null s) $ do
			hPutStr inh s
			hFlush inh
		hClose inh
	writeinput Nothing _ = return ()
