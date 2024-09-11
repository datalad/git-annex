{- git-annex command
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.Sim where

import Command
import Annex.Sim
import Annex.Sim.File
import Utility.Tmp.Dir

import System.Random

cmd :: Command
cmd = command "sim" SectionTesting
	"simulate a network of repositories"
	paramCommand (withParams seek)

seek :: CmdParams -> CommandSeek
seek _ = do
	rng <- initStdGen
	repobyname <- mkGetExistingRepoByName
	withTmpDir "sim" $ \tmpdir -> do
		let getpath = GetSimRepoPath $ \u -> tmpdir </> fromUUID u
		let st = emptySimState rng repobyname getpath
		st' <- runSimCommand (CommandInit (RepoName "foo")) st
			>>= runSimCommand (CommandUse (RepoName "bar") "here")
			>>= runSimCommand (CommandConnect (RepoName "foo") (RemoteName "bar"))
			>>= runSimCommand (CommandConnect (RepoName "bar") (RemoteName "foo"))
			>>= runSimCommand (CommandAdd "bigfile" 1000000 [RepoName "foo"])
			>>= runSimCommand (CommandAction (RepoName "bar") (ActionGitPull (RemoteName "foo")))
			>>= runSimCommand (CommandAction (RepoName "bar") (ActionGetWanted (RemoteName "foo")))
		st'' <- liftIO $ updateSimRepos st'
		liftIO $ print tmpdir
		_ <- liftIO $ getLine
		return ()

