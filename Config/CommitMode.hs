{- git-annex configuration
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Config.CommitMode where

import Annex.Common
import qualified Annex
import Git.Branch (CommitMode(..))

implicitCommitMode :: Annex CommitMode
implicitCommitMode = go . annexAllowSign <$> Annex.getGitConfig
  where
	go True = ManualCommit
	go False = AutomaticCommit
