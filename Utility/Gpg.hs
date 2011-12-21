{- gpg interface
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Gpg where

import qualified Data.ByteString.Lazy.Char8 as L
import System.Posix.Types
import Control.Applicative
import Control.Concurrent
import Control.Exception (finally)
import System.Exit
import System.Environment

import Common

newtype KeyIds = KeyIds [String]
        deriving (Ord, Eq)

stdParams :: [CommandParam] -> IO [String]
stdParams params = do
	-- Enable batch mode if GPG_AGENT_INFO is set, to avoid extraneous
	-- gpg output about password prompts.
	e <- catchDefaultIO (getEnv "GPG_AGENT_INFO") ""
	let batch = if null e then [] else ["--batch"]
	return $ batch ++ defaults ++ toCommand params
	where
		-- be quiet, even about checking the trustdb
		defaults = ["--quiet", "--trust-model", "always"]

{- Runs gpg with some params and returns its stdout, strictly. -}
readStrict :: [CommandParam] -> IO String
readStrict params = do
	params' <- stdParams params
	pOpen ReadFromPipe "gpg" params' hGetContentsStrict

{- Runs gpg, piping an input value to it, and returninging its stdout,
 - strictly. -}
pipeStrict :: [CommandParam] -> String -> IO String
pipeStrict params input = do
	params' <- stdParams params
	(pid, fromh, toh) <- hPipeBoth "gpg" params'
	_ <- forkIO $ finally (hPutStr toh input) (hClose toh)
	output <- hGetContentsStrict fromh
	forceSuccess pid
	return output

{- Runs gpg with some parameters, first feeding it a passphrase via
 - --passphrase-fd, then feeding it an input, and passing a handle
 - to its output to an action.
 -
 - Note that to avoid deadlock with the cleanup stage,
 - the action must fully consume gpg's input before returning. -}
passphraseHandle :: [CommandParam] -> String -> IO L.ByteString -> (Handle -> IO a) -> IO a
passphraseHandle params passphrase a b = do
	-- pipe the passphrase into gpg on a fd
	(frompipe, topipe) <- createPipe
	_ <- forkIO $ do
		toh <- fdToHandle topipe
		hPutStrLn toh passphrase
		hClose toh
	let Fd pfd = frompipe
	let passphrasefd = [Param "--passphrase-fd", Param $ show pfd]

	params' <- stdParams $ passphrasefd ++ params
	(pid, fromh, toh) <- hPipeBoth "gpg" params'
	pid2 <- forkProcess $ do
		L.hPut toh =<< a
		hClose toh
		exitSuccess
	hClose toh
	ret <- b fromh

	-- cleanup
	forceSuccess pid
	_ <- getProcessStatus True False pid2
	closeFd frompipe
	return ret

{- Finds gpg public keys matching some string. (Could be an email address,
 - a key id, or a name. -}
findPubKeys :: String -> IO KeyIds
findPubKeys for = KeyIds . parse <$> readStrict params
	where
		params = [Params "--with-colons --list-public-keys", Param for]
		parse = map keyIdField . filter pubKey . lines
		pubKey = isPrefixOf "pub:"
		keyIdField s = split ":" s !! 4
