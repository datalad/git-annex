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

import System.Process

import Common

type CoProcessHandle = (ProcessHandle, Handle, Handle, FilePath, [String])

start :: FilePath -> [String] -> IO CoProcessHandle
start command params = do
	(from, to, _err, pid) <- runInteractiveProcess command params Nothing Nothing
	return (pid, to, from, command, params)

stop :: CoProcessHandle -> IO ()
stop (pid, from, to, command, params) = do
	hClose to
	hClose from
	forceSuccessProcess pid command params

query :: CoProcessHandle -> (Handle -> IO a) -> (Handle -> IO b) -> IO b
query (_, from, to, _, _) send receive = do
	_ <- send to
	hFlush to
	receive from
