{- The current git repository.
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Git.CurrentRepo where

import Common
import Git.Types
import Git.Construct
import qualified Git.Config
import Utility.Env
import Utility.Env.Set
import Utility.Path.AbsRel
import qualified Utility.RawFilePath as R

import qualified Data.ByteString as B
import qualified System.FilePath.ByteString as P

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
 -
 - Also works around a git bug when running some hooks. It
 - runs the hooks in the top of the repository, but if GIT_WORK_TREE
 - was relative (but not "."), it then points to the wrong directory.
 - In this situation GIT_PREFIX contains the directory that 
 - GIT_WORK_TREE is relative to.
 -}
get :: IO Repo
get = do
	gd <- getpathenv "GIT_DIR"
	r <- configure gd =<< fromCwd
	prefix <- getpathenv "GIT_PREFIX"
	wt <- maybe (worktree (location r)) Just
		<$> getpathenvprefix "GIT_WORK_TREE" prefix
	case wt of
		Nothing -> return r
		Just d -> do
			curr <- R.getCurrentDirectory
			unless (d `dirContains` curr) $
				setCurrentDirectory (fromRawFilePath d)
			return $ addworktree wt r
  where
	getpathenv s = do
		v <- getEnv s
		case v of
			Just d -> do
				unsetEnv s
				return (Just (toRawFilePath d))
			Nothing -> return Nothing
	
	getpathenvprefix s (Just prefix) | not (B.null prefix) =
		getpathenv s >>= \case
			Nothing -> return Nothing
			Just d
				| d == "." -> return (Just d)
				| otherwise -> Just 
					<$> absPath (prefix P.</> d)
	getpathenvprefix s _ = getpathenv s

	configure Nothing (Just r) = Git.Config.read r
	configure (Just d) _ = do
		absd <- absPath d
		curr <- R.getCurrentDirectory
		loc <- adjustGitDirFile $ Local
			{ gitdir = absd
			, worktree = Just curr
			}
		r <- Git.Config.read $ newFrom loc
		return $ if Git.Config.isBare r
			then r { location = (location r) { worktree = Nothing } }
			else r
	configure Nothing Nothing = giveup "Not in a git repository."

	addworktree w r = changelocation r $ Local
		{ gitdir = gitdir (location r)
		, worktree = w
		}
	
	changelocation r l = r { location = l }
