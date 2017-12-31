{- Process transcript
 -
 - Copyright 2012-2015 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Process.Transcript where

import Utility.Process

import System.IO
import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
#ifndef mingw32_HOST_OS
import qualified System.Posix.IO
#else
import Control.Applicative
#endif
import Data.Maybe
import Prelude

-- | Runs a process, optionally feeding it some input, and
-- returns a transcript combining its stdout and stderr, and
-- whether it succeeded or failed.
processTranscript :: String -> [String] -> (Maybe String) -> IO (String, Bool)
processTranscript cmd opts = processTranscript' (proc cmd opts)

processTranscript' :: CreateProcess -> Maybe String -> IO (String, Bool)
processTranscript' cp input = do
#ifndef mingw32_HOST_OS
{- This implementation interleves stdout and stderr in exactly the order
 - the process writes them. -}
	(readf, writef) <- System.Posix.IO.createPipe
	readh <- System.Posix.IO.fdToHandle readf
	writeh <- System.Posix.IO.fdToHandle writef
	p@(_, _, _, pid) <- createProcess $ cp
		{ std_in = if isJust input then CreatePipe else Inherit
		, std_out = UseHandle writeh
		, std_err = UseHandle writeh
		}
	hClose writeh

	get <- mkreader readh
	writeinput input p
	transcript <- get

	ok <- checkSuccessProcess pid
	return (transcript, ok)
#else
{- This implementation for Windows puts stderr after stdout. -}
	p@(_, _, _, pid) <- createProcess $ cp
		{ std_in = if isJust input then CreatePipe else Inherit
		, std_out = CreatePipe
		, std_err = CreatePipe
		}

	getout <- mkreader (stdoutHandle p)
	geterr <- mkreader (stderrHandle p)
	writeinput input p
	transcript <- (++) <$> getout <*> geterr

	ok <- checkSuccessProcess pid
	return (transcript, ok)
#endif
  where
	mkreader h = do
		s <- hGetContents h
		v <- newEmptyMVar
		void $ forkIO $ do
			void $ E.evaluate (length s)
			putMVar v ()
		return $ do
			takeMVar v
			return s

	writeinput (Just s) p = do
		let inh = stdinHandle p
		unless (null s) $ do
			hPutStr inh s
			hFlush inh
		hClose inh
	writeinput Nothing _ = return ()
