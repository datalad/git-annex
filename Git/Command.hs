{- running git commands
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Command where

import System.Posix.Process (getAnyProcessStatus)
import System.Process

import Common
import Git
import Git.Types
import qualified Utility.CoProcess as CoProcess

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
	boolSystemEnv "git"
		(gitCommandLine (Param subcommand : params) repo)
		(gitEnv repo)

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
pipeRead params repo = assertLocal repo $
	withHandle StdoutHandle createBackgroundProcess p $ \h -> do
		fileEncoding h
		hGetContents h
	where
		p  = (proc "git" $ toCommand $ gitCommandLine params repo)
			{ env = gitEnv repo }

{- Runs a git subcommand, feeding it input, and returning its output,
 - which is expected to be fairly small, since it's all read into memory
 - strictly. -}
pipeWriteRead :: [CommandParam] -> String -> Repo -> IO String
pipeWriteRead params s repo = assertLocal repo $
	writeReadProcessEnv "git" (toCommand $ gitCommandLine params repo) 
		(gitEnv repo) s

{- Runs a git subcommand, feeding it input on a handle with an action. -}
pipeWrite :: [CommandParam] -> Repo -> (Handle -> IO ()) -> IO ()
pipeWrite params repo = withHandle StdinHandle createProcessSuccess p
	where
		p = (proc "git" $ toCommand $ gitCommandLine params repo)
			{ env = gitEnv repo }

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

{- Runs a git command as a coprocess. -}
gitCoProcessStart :: [CommandParam] -> Repo -> IO CoProcess.CoProcessHandle
gitCoProcessStart params repo = CoProcess.start "git" (toCommand $ gitCommandLine params repo) (gitEnv repo)
