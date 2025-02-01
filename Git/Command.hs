{- running git commands
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Git.Command where

import Common
import Git
import Git.Types
import qualified Utility.CoProcess as CoProcess

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S

{- Constructs a git command line operating on the specified repo. -}
gitCommandLine :: [CommandParam] -> Repo -> [CommandParam]
gitCommandLine params r@(Repo { location = l@(Local { } ) }) =
	setdir ++ settree ++ gitGlobalOpts r ++ params
  where
	setdir
		| gitEnvOverridesGitDir r = []
		| otherwise = [Param $ "--git-dir=" ++ fromOsPath (gitdir l)]
	settree = case worktree l of
		Nothing -> []
		Just t -> [Param $ "--work-tree=" ++ fromOsPath t]
gitCommandLine _ repo = assertLocal repo $ error "internal"

{- Runs git in the specified repo. -}
runBool :: [CommandParam] -> Repo -> IO Bool
runBool params repo = assertLocal repo $
	boolSystemEnv "git" (gitCommandLine params repo) (gitEnv repo)

{- Runs git in the specified repo, throwing an error if it fails. -}
run :: [CommandParam] -> Repo -> IO ()
run params repo = assertLocal repo $
	unlessM (runBool params repo) $
		giveup $ "git " ++ show params ++ " failed"

{- Runs git and forces it to be quiet, throwing an error if it fails. -}
runQuiet :: [CommandParam] -> Repo -> IO ()
runQuiet params repo = withNullHandle $ \nullh ->
	let p = (proc "git" $ toCommand $ gitCommandLine (params) repo)
		{ env = gitEnv repo
		, std_out = UseHandle nullh
		, std_err = UseHandle nullh
		}
	in withCreateProcess p $ \_ _ _ -> forceSuccessProcess p

{- Runs a git command and returns its output, lazily.
 -
 - Also returns an action that should be used when the output is all
 - read, that will wait on the command, and
 - return True if it succeeded.
 -}
pipeReadLazy :: [CommandParam] -> Repo -> IO (L.ByteString, IO Bool)
pipeReadLazy params repo = assertLocal repo $ do
	(_, Just h, _, pid) <- createProcess p { std_out = CreatePipe }
	c <- L.hGetContents h
	return (c, checkSuccessProcess pid)
  where
	p  = gitCreateProcess params repo

{- Runs a git command, and returns its output, strictly.
 -
 - Nonzero exit status is ignored.
 -}
pipeReadStrict :: [CommandParam] -> Repo -> IO S.ByteString
pipeReadStrict = pipeReadStrict' id

pipeReadStrict' :: (CreateProcess -> CreateProcess) -> [CommandParam] -> Repo -> IO S.ByteString
pipeReadStrict' fp params repo = assertLocal repo $ withCreateProcess p go
  where
	p = fp (gitCreateProcess params repo) { std_out = CreatePipe }

	go _ (Just outh) _ pid = do
		output <- S.hGetContents outh
		hClose outh
		void $ waitForProcess pid
		return output
	go _ _ _ _ = error "internal"

{- Runs a git command, feeding it an input, and returning its output,
 - which is expected to be fairly small, since it's all read into memory
 - strictly. -}
pipeWriteRead :: [CommandParam] -> Maybe (Handle -> IO ()) -> Repo -> IO S.ByteString
pipeWriteRead params writer repo = assertLocal repo $
	writeReadProcessEnv "git" (toCommand $ gitCommandLine params repo) 
		(gitEnv repo) writer'
  where
	writer' = case writer of
		Nothing -> Nothing
		Just a -> Just $ \h -> do
			adjusthandle h
			a h
	adjusthandle h = hSetNewlineMode h noNewlineTranslation

{- Runs a git command, feeding it input on a handle with an action. -}
pipeWrite :: [CommandParam] -> Repo -> (Handle -> IO ()) -> IO ()
pipeWrite params repo feeder = assertLocal repo $
	let p = (gitCreateProcess params repo)
		{ std_in = CreatePipe }
	in withCreateProcess p (go p)
  where
	go p (Just hin) _ _ pid = do
		feeder hin
		hClose hin
		forceSuccessProcess p pid
	go _ _ _ _ _ = error "internal"

{- Reads null terminated output of a git command (as enabled by the -z 
 - parameter), and splits it. -}
pipeNullSplit :: [CommandParam] -> Repo -> IO ([L.ByteString], IO Bool)
pipeNullSplit params repo = do
	(s, cleanup) <- pipeReadLazy params repo
	return (filter (not . L.null) $ L.split 0 s, cleanup)

{- Reads lazily, but copies each part to a strict ByteString for
 - convenience.
 -}
pipeNullSplit' :: [CommandParam] -> Repo -> IO ([S.ByteString], IO Bool)
pipeNullSplit' = pipeNullSplit'' id

pipeNullSplit'' :: (S.ByteString -> t) -> [CommandParam] -> Repo -> IO ([t], IO Bool)
pipeNullSplit'' f params repo = do
	(s, cleanup) <- pipeNullSplit params repo
	return (map (f . L.toStrict) s, cleanup)

pipeNullSplitStrict :: [CommandParam] -> Repo -> IO [S.ByteString]
pipeNullSplitStrict params repo = do
	s <- pipeReadStrict params repo
	return $ filter (not . S.null) $ S.split 0 s

{- Runs a git command as a coprocess. -}
gitCoProcessStart :: Bool -> [CommandParam] -> Repo -> IO CoProcess.CoProcessHandle
gitCoProcessStart restartable params repo = CoProcess.start numrestarts "git"
	(toCommand $ gitCommandLine params repo)
	(gitEnv repo)
  where
	{- If a long-running git command like cat-file --batch
	 - crashes, it will likely start up again ok. If it keeps crashing
	 - 10 times, something is badly wrong. -}
	numrestarts = if restartable then 10 else 0

gitCreateProcess :: [CommandParam] -> Repo -> CreateProcess
gitCreateProcess params repo =
	(proc "git" $ toCommand $ gitCommandLine params repo)
			{ env = gitEnv repo }
