{- Interface for running a shell command as a coprocess,
 - sending it queries and getting back results.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.CoProcess (
	CoProcessHandle,
	start,
	stop,
	query
) where

import System.Cmd.Utils

import Common

type CoProcessHandle = (PipeHandle, Handle, Handle)

start :: FilePath -> [String] -> IO CoProcessHandle
start command params = hPipeBoth command params

stop :: CoProcessHandle -> IO ()
stop (pid, from, to) = do
	hClose to
	hClose from
	forceSuccess pid

query :: CoProcessHandle -> (Handle -> IO a) -> (Handle -> IO b) -> IO b
query (_, from, to) send receive = do
	_ <- send to
	hFlush to
	receive from
