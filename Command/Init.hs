{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Init where

import Control.Monad.State (liftIO)
import Control.Monad (when, unless)
import System.Directory

import Command
import qualified Annex
import qualified Git
import qualified Branch
import UUID
import Version
import Messages
import Types
import Utility
	
command :: [Command]
command = [standaloneCommand "init" paramDesc seek
		"initialize git-annex with repository description"]

seek :: [CommandSeek]
seek = [withWords start]

start :: CommandStartWords
start ws = do
	when (null description) $
		error "please specify a description of this repository\n"
	showStart "init" description
	next $ perform description
	where
		description = unwords ws

perform :: String -> CommandPerform
perform description = do
	Branch.create
	g <- Annex.gitRepo
	u <- getUUID g
	setVersion
	describeUUID u description
	unless (Git.repoIsLocalBare g) $
		gitPreCommitHookWrite g
	next $ return True

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

preCommitHook :: Git.Repo -> FilePath
preCommitHook repo = 
	Git.workTree repo ++ "/" ++ Git.gitDir repo ++ "/hooks/pre-commit"

preCommitScript :: String
preCommitScript = 
		"#!/bin/sh\n" ++
		"# automatically configured by git-annex\n" ++ 
		"git annex pre-commit .\n"
