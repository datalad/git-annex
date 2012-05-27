{- The current git repository.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.CurrentRepo where

import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.Env (getEnv, unsetEnv)

import Common
import Git.Types
import Git.Construct
import qualified Git.Config

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
	r <- configure gd =<< maybe fromCwd fromPath gd
	wt <- maybe (Git.Config.workTree r) Just <$> pathenv "GIT_WORK_TREE"
	case wt of
		Nothing -> return r
		Just d -> do
			cwd <- getCurrentDirectory
			unless (d `dirContains` cwd) $
				changeWorkingDirectory d
			return $ addworktree wt r
	where
		pathenv s = do
			v <- getEnv s
			when (isJust v) $
				unsetEnv s
			case v of
				Nothing -> return Nothing
				Just d -> Just <$> absPath d
		configure Nothing r = Git.Config.read r
		configure (Just d) r = do
			r' <- Git.Config.read r
			-- Let GIT_DIR override the default gitdir.
			return $ changelocation r' $
				Local { gitdir = d, worktree = worktree (location r') }
		addworktree w r = changelocation r $
			Local { gitdir = gitdir (location r), worktree = w }
		changelocation r l = r { location = l }
