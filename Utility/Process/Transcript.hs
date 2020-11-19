{- Process transcript
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Process.Transcript (
	processTranscript,
	processTranscript',
	processTranscript'',
) where

import Utility.Process

import System.IO
import System.Exit
import Control.Concurrent.Async
import Control.Monad
import Control.Exception
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
 	let setup = do
		(readf, writef) <- System.Posix.IO.createPipe
		System.Posix.IO.setFdOption readf System.Posix.IO.CloseOnExec True
		System.Posix.IO.setFdOption writef System.Posix.IO.CloseOnExec True
		readh <- System.Posix.IO.fdToHandle readf
		writeh <- System.Posix.IO.fdToHandle writef
		return (readh, writeh)
	let cleanup (readh, writeh) = do
		hClose readh
		hClose writeh
	bracket setup cleanup $ \(readh, writeh) -> do
		let cp' = cp
			{ std_in = if isJust input then CreatePipe else Inherit
			, std_out = UseHandle writeh
			, std_err = UseHandle writeh
			}
		withCreateProcess cp' $ \hin hout herr pid -> do
			get <- asyncreader pid readh
			writeinput input (hin, hout, herr, pid)
			code <- waitForProcess pid
			transcript <- wait get
			return (transcript, code)
#else
{- This implementation for Windows puts stderr after stdout. -}
	let cp' = cp 
		{ std_in = if isJust input then CreatePipe else Inherit
		, std_out = CreatePipe
		, std_err = CreatePipe
		}
	withCreateProcess cp' $ \hin hout herr pid -> do
		let p = (hin, hout, herr, pid)
		getout <- asyncreader pid (stdoutHandle p)
		geterr <- asyncreader pid (stderrHandle p)
		writeinput input p
		code <- waitForProcess pid
		transcript <- (++) <$> wait getout <*> wait geterr
		return (transcript, code)
#endif
  where
	asyncreader pid h = async $ reader pid h []
	reader pid h c = hGetLineUntilExitOrEOF pid h >>= \case
		Nothing -> return (concat (reverse c))
		Just l -> reader pid h (l:c)
	writeinput (Just s) p = do
		let inh = stdinHandle p
		unless (null s) $ do
			hPutStr inh s
			hFlush inh
		hClose inh
	writeinput Nothing _ = return ()
