{- git-annex repository initialization
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Init (
	ensureInitialized,
	initialize,
	uninitialize
) where

import Control.Monad.State (liftIO)
import Control.Monad (unless)
import System.Directory

import qualified Annex
import qualified Git
import qualified Branch
import Version
import Messages
import Types
import Utility
import UUID

initialize :: Annex ()
initialize = do
	prepUUID
	Branch.create
	setVersion
	g <- Annex.gitRepo
	unless (Git.repoIsLocalBare g) $
		gitPreCommitHookWrite g

uninitialize :: Annex ()
uninitialize = do
	g <- Annex.gitRepo
	gitPreCommitHookUnWrite g

{- Will automatically initialize if there is already a git-annex
   branch from somewhere. Otherwise, require a manual init
   to avoid git-annex accidentially being run in git
   repos that did not intend to use it. -}
ensureInitialized :: Annex ()
ensureInitialized = getVersion >>= maybe needsinit checkVersion
	where
		needsinit = do
			annexed <- Branch.hasSomeBranch
			if annexed
				then initialize
				else error "First run: git-annex init"

{- set up a git pre-commit hook, if one is not already present -}
gitPreCommitHookWrite :: Git.Repo -> Annex ()
gitPreCommitHookWrite repo = do
	exists <- liftIO $ doesFileExist hook
	if exists
		then warning $ "pre-commit hook (" ++ hook ++ ") already exists, not configuring"
		else liftIO $ do
			viaTmp writeFile hook preCommitScript
			p <- getPermissions hook
			setPermissions hook $ p {executable = True}
	where
		hook = preCommitHook repo

gitPreCommitHookUnWrite :: Git.Repo -> Annex ()
gitPreCommitHookUnWrite repo = do
	let hook = preCommitHook repo
	whenM (liftIO $ doesFileExist hook) $ do
		c <- liftIO $ readFile hook
		if c == preCommitScript
			then liftIO $ removeFile hook
			else warning $ "pre-commit hook (" ++ hook ++ 
				") contents modified; not deleting." ++
				" Edit it to remove call to git annex."

preCommitHook :: Git.Repo -> FilePath
preCommitHook repo = 
	Git.workTree repo ++ "/" ++ Git.gitDir repo ++ "/hooks/pre-commit"

preCommitScript :: String
preCommitScript = 
		"#!/bin/sh\n" ++
		"# automatically configured by git-annex\n" ++ 
		"git annex pre-commit .\n"
