{- git-annex repository fixups
 -
 - Copyright 2013-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.Fixup where

import Git.Types
import Git.Config
import Types.GitConfig
import Config.Files
import qualified Git
import Utility.Path
import Utility.SafeCommand
import Utility.Directory
import Utility.Exception
import Utility.Monad
import Utility.FileSystemEncoding
import Utility.PartialPrelude

import System.IO
import System.FilePath
import System.PosixCompat.Files
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.IfElse
import qualified Data.Map as M
import Control.Applicative
import Prelude

fixupRepo :: Repo -> GitConfig -> IO Repo
fixupRepo r c = do
	let r' = disableWildcardExpansion r
	r'' <- fixupUnusualRepos r' c
	if annexDirect c
		then return (fixupDirect r'')
		else return r''

{- Disable git's built-in wildcard expansion, which is not wanted
 - when using it as plumbing by git-annex. -}
disableWildcardExpansion :: Repo -> Repo
disableWildcardExpansion r = r
	{ gitGlobalOpts = gitGlobalOpts r ++ [Param "--literal-pathspecs"] }

{- Direct mode repos have core.bare=true, but are not really bare.
 - Fix up the Repo to be a non-bare repo, and arrange for git commands
 - run by git-annex to be passed parameters that override this setting. -}
fixupDirect :: Repo -> Repo
fixupDirect r@(Repo { location = l@(Local { gitdir = d, worktree = Nothing }) }) = do
	r
		{ location = l { worktree = Just (parentDir d) }
		, gitGlobalOpts = gitGlobalOpts r ++
			[ Param "-c"
			, Param $ decodeBS' coreBare ++ "=" ++ boolConfig False
			]
		}
fixupDirect r = r

{- Submodules have their gitdir containing ".git/modules/", and
 - have core.worktree set, and also have a .git file in the top
 - of the repo. We need to unset core.worktree, and change the .git
 - file into a symlink to the git directory. This way, annex symlinks will be
 - of the usual .git/annex/object form, and will consistently work
 - whether a repo is used as a submodule or not, and wheverever the
 - submodule is mounted.
 -
 - git-worktree directories have a .git file.
 - That needs to be converted to a symlink, and .git/annex made a symlink
 - to the main repository's git-annex directory.
 - The worktree shares git config with the main repository, so the same
 - annex uuid and other configuration will be used in the worktree as in
 - the main repository.
 -
 - git clone or init with --separate-git-dir similarly makes a .git file,
 - which in that case points to a different git directory. It's
 - also converted to a symlink so links to .git/annex will work. 
 - 
 - When the filesystem doesn't support symlinks, we cannot make .git
 - into a symlink. But we don't need too, since the repo will use direct
 - mode.
 -
 - Before making any changes, check if there's a .noannex file
 - in the repo. If that file will prevent git-annex from being used,
 - there's no need to fix up the repository.
 -}
fixupUnusualRepos :: Repo -> GitConfig -> IO Repo
fixupUnusualRepos r@(Repo { location = l@(Local { worktree = Just w, gitdir = d }) }) c
	| needsSubmoduleFixup r = ifM notnoannex
		( do
			when (coreSymlinks c) $
				(replacedotgit >> unsetcoreworktree)
					`catchNonAsync` \_e -> hPutStrLn stderr
						"warning: unable to convert submodule to form that will work with git-annex"
			return $ r'
				{ config = M.delete "core.worktree" (config r)
				}
		, return r
		)
	| otherwise = ifM (needsGitLinkFixup r <&&> notnoannex)
		( do
			when (coreSymlinks c) $
				(replacedotgit >> worktreefixup)
					`catchNonAsync` \_e -> hPutStrLn stderr
						"warning: unable to convert .git file to symlink that will work with git-annex"
			return r'
		, return r
		)
  where
	dotgit = w </> ".git"

	replacedotgit = whenM (doesFileExist dotgit) $ do
		linktarget <- relPathDirToFile w d
		nukeFile dotgit
		createSymbolicLink linktarget dotgit
	
	unsetcoreworktree =
		maybe (error "unset core.worktree failed") (\_ -> return ())
			=<< Git.Config.unset "core.worktree" r
	
	worktreefixup =
		-- git-worktree sets up a "commondir" file that contains
		-- the path to the main git directory.
		-- Using --separate-git-dir does not.
		catchDefaultIO Nothing (headMaybe . lines <$> readFile (d </> "commondir")) >>= \case
			Just gd -> do
				-- Make the worktree's git directory
				-- contain an annex symlink to the main
				-- repository's annex directory.
				let linktarget = gd </> "annex"
				createSymbolicLink linktarget (dotgit </> "annex")
			Nothing -> return ()

	-- Repo adjusted, so that symlinks to objects that get checked
	-- in will have the usual path, rather than pointing off to the
	-- real .git directory.
	r'
		| coreSymlinks c = r { location = l { gitdir = dotgit } }
		| otherwise = r

	notnoannex = isNothing <$> noAnnexFileContent (Git.repoWorkTree r)
fixupUnusualRepos r _ = return r

needsSubmoduleFixup :: Repo -> Bool
needsSubmoduleFixup (Repo { location = (Local { worktree = Just _, gitdir = d }) }) =
	(".git" </> "modules") `isInfixOf` d
needsSubmoduleFixup _ = False

needsGitLinkFixup :: Repo -> IO Bool
needsGitLinkFixup (Repo { location = (Local { worktree = Just wt, gitdir = d }) })
	-- Optimization: Avoid statting .git in the common case; only
	-- when the gitdir is not in the usual place inside the worktree
	-- might .git be a file.
	| wt </> ".git" == d = return False
	| otherwise = doesFileExist (wt </> ".git")
needsGitLinkFixup _ = return False
