{- module using Data.ByteString.Lazy.Char8 for git IO
 -
 - This can be imported instead of Git when more efficient ByteString IO
 - is needed.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.ByteString (
	module Git,
	pipeRead,
	pipeWrite,
	pipeWriteRead,
	pipeNullSplit
) where

import Control.Applicative
import System.Cmd.Utils
import System.IO
import qualified Data.ByteString.Lazy.Char8 as L

import Git hiding (pipeRead, pipeWrite, pipeWriteRead, pipeNullSplit)
import Utility.SafeCommand

{- Runs a git subcommand and returns its output, lazily. 
 -
 - Note that this leaves the git process running, and so zombies will
 - result unless reap is called.
 -}
pipeRead :: Repo -> [CommandParam] -> IO L.ByteString
pipeRead repo params = assertLocal repo $ do
	(_, h) <- hPipeFrom "git" $ toCommand $ gitCommandLine repo params
	hSetBinaryMode h True
	L.hGetContents h

{- Runs a git subcommand, feeding it input.
 - You should call either getProcessStatus or forceSuccess on the PipeHandle. -}
pipeWrite :: Repo -> [CommandParam] -> L.ByteString -> IO PipeHandle
pipeWrite repo params s = assertLocal repo $ do
	(p, h) <- hPipeTo "git" (toCommand $ gitCommandLine repo params)
	L.hPut h s
	hClose h
	return p

{- Runs a git subcommand, feeding it input, and returning its output.
 - You should call either getProcessStatus or forceSuccess on the PipeHandle. -}
pipeWriteRead :: Repo -> [CommandParam] -> L.ByteString -> IO (PipeHandle, L.ByteString)
pipeWriteRead repo params s = assertLocal repo $ do
	(p, from, to) <- hPipeBoth "git" (toCommand $ gitCommandLine repo params)
	hSetBinaryMode from True
	L.hPut to s
	hClose to
	c <- L.hGetContents from
	return (p, c)

{- Reads null terminated output of a git command (as enabled by the -z 
 - parameter), and splits it. -}
pipeNullSplit :: Repo -> [CommandParam] -> IO [L.ByteString]
pipeNullSplit repo params = filter (not . L.null) . L.split '\0' <$>
	pipeRead repo params
