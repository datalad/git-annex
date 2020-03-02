{- git-annex configuration
 -
 - Copyright 2017-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Config.GitConfig where

import Annex.Common
import qualified Annex
import Types.GitConfig
import Git.Types
import Logs.Config

{- Gets a specific setting from GitConfig. If necessary, loads the
 - repository-global defaults when the GitConfig does not yet 
 - have a value.
 -
 - Note: Be sure to add the config to mergeGitConfig and to
 - globalConfigs.
 -}
getGitConfigVal :: (GitConfig -> Configurable a) -> Annex a
getGitConfigVal f = getGitConfigVal' f >>= \case
	HasGlobalConfig c -> return c
	DefaultConfig d -> return d
	HasGitConfig c -> return c

getGitConfigVal' :: (GitConfig -> Configurable a) -> Annex (Configurable a)
getGitConfigVal' f = (f <$> Annex.getGitConfig) >>= \case
	DefaultConfig _ -> do
		r <- Annex.gitRepo
		m <- loadGlobalConfig
		let globalgc = extractGitConfig FromGlobalConfig (r { config = m })
		-- This merge of the repo-global config and the git
		-- config makes all repository-global default
		-- values populate the GitConfig with HasGlobalConfig
		-- values, so it will only need to be done once.
		Annex.overrideGitConfig (\gc -> mergeGitConfig gc globalgc)
		f <$> Annex.getGitConfig
	c -> return c
