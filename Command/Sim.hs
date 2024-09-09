{- git-annex command
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Sim where

import Command
import Annex.Sim
import qualified Annex
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
	r <- Annex.gitRepo
	withTmpDir "sim" $ \tmpdir -> do
		let st = emptySimState rng repobyname
		st' <- runSimCommand (CommandInit (RepoName "foo")) st
			>>= runSimCommand (CommandTrustLevel (RepoName "foo") "trusted")
			>>= runSimCommand (CommandUse (RepoName "bar") "here")
		let simdir = \u -> tmpdir </> fromUUID u
		st'' <- liftIO $ updateSimRepos r simdir st'
		liftIO $ print tmpdir
		_ <- liftIO $ getLine
		return ()

