{- The current git repository.
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.CurrentRepo where

import Common
import Git.Types
import Git.Construct
import qualified Git.Config
import Utility.Env

{- Gets the current git repository.
 -
 - Honors GIT_DIR and GIT_WORK_TREE.
 - Both environment variables are unset, to avoid confusing other git
 - commands that also look at them. Instead, the Git module passes
 - --work-tree and --git-dir to git commands it runs.
 -
 - When GIT_WORK_TREE or core.worktree are set, changes the working
 - directory if necessary to ensure it is within the repository's work
 - tree. While not needed for git commands, this is useful for anything
 - else that looks for files in the worktree.
 -}
get :: IO Repo
get = do
	gd <- pathenv "GIT_DIR"
	r <- configure gd =<< fromCwd
	wt <- maybe (worktree $ location r) Just <$> pathenv "GIT_WORK_TREE"
	case wt of
		Nothing -> return r
		Just d -> do
			curr <- getCurrentDirectory
			unless (d `dirContains` curr) $
				setCurrentDirectory d
			return $ addworktree wt r
  where
	pathenv s = do
		v <- getEnv s
		case v of
			Just d -> do
				unsetEnv s
				Just <$> absPath d
			Nothing -> return Nothing

	configure Nothing (Just r) = Git.Config.read r
	configure (Just d) _ = do
		absd <- absPath d
		curr <- getCurrentDirectory
		Git.Config.read $ newFrom $
			Local { gitdir = absd, worktree = Just curr }
	configure Nothing Nothing = giveup "Not in a git repository."

	addworktree w r = changelocation r $
		Local { gitdir = gitdir (location r), worktree = w }
	changelocation r l = r { location = l }
