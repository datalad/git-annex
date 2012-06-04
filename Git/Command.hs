{- running git commands
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Command where

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Control.Concurrent
import Control.Exception (finally)

import Common
import Git
import Git.Types

{- Constructs a git command line operating on the specified repo. -}
gitCommandLine :: [CommandParam] -> Repo -> [CommandParam]
gitCommandLine params Repo { location = l@(Local _ _ ) } = setdir : settree ++ params
	where
		setdir = Param $ "--git-dir=" ++ gitdir l
		settree = case worktree l of
			Nothing -> []
			Just t -> [Param $ "--work-tree=" ++ t]
gitCommandLine _ repo = assertLocal repo $ error "internal"

{- Runs git in the specified repo. -}
runBool :: String -> [CommandParam] -> Repo -> IO Bool
runBool subcommand params repo = assertLocal repo $
	boolSystem "git" $ gitCommandLine (Param subcommand : params) repo

{- Runs git in the specified repo, throwing an error if it fails. -}
run :: String -> [CommandParam] -> Repo -> IO ()
run subcommand params repo = assertLocal repo $
	unlessM (runBool subcommand params repo) $
		error $ "git " ++ subcommand ++ " " ++ show params ++ " failed"

{- Runs a git subcommand and returns its output, lazily. 
 -
 - Note that this leaves the git process running, and so zombies will
 - result unless reap is called.
 -}
pipeRead :: [CommandParam] -> Repo -> IO String
pipeRead params repo = assertLocal repo $ do
	(_, h) <- hPipeFrom "git" $ toCommand $ gitCommandLine params repo
	fileEncoding h
	hGetContents h

{- Runs a git subcommand, feeding it input.
 - You should call either getProcessStatus or forceSuccess on the PipeHandle. -}
pipeWrite :: [CommandParam] -> L.Text -> Repo -> IO PipeHandle
pipeWrite params s repo = assertLocal repo $ do
	(p, h) <- hPipeTo "git" (toCommand $ gitCommandLine params repo)
	L.hPutStr h s
	hClose h
	return p

{- Runs a git subcommand, feeding it input, and returning its output.
 - You should call either getProcessStatus or forceSuccess on the PipeHandle. -}
pipeWriteRead :: [CommandParam] -> String -> Repo -> IO (PipeHandle, String)
pipeWriteRead params s repo = assertLocal repo $ do
	(p, from, to) <- hPipeBoth "git" (toCommand $ gitCommandLine params repo)
	fileEncoding to
	fileEncoding from
	_ <- forkIO $ finally (hPutStr to s) (hClose to)
	c <- hGetContents from
	return (p, c)

{- Reads null terminated output of a git command (as enabled by the -z 
 - parameter), and splits it. -}
pipeNullSplit :: [CommandParam] -> Repo -> IO [String]
pipeNullSplit params repo =
	filter (not . null) . split sep <$> pipeRead params repo
	where
		sep = "\0"

{- Reaps any zombie git processes. -}
reap :: IO ()
reap = do
	-- throws an exception when there are no child processes
	catchDefaultIO (getAnyProcessStatus False True) Nothing
		>>= maybe noop (const reap)
