{- git-annex configuration
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Config.GitConfig where

import Annex.Common
import qualified Annex
import Types.GitConfig
import Git.Types
import Logs.Config

{- Gets a specific setting from GitConfig. If necessary, loads the
 - repository-global defaults when the GitConfig does not yet 
 - have a value. -}
getGitConfigVal :: (GitConfig -> Configurable a) -> Annex a
getGitConfigVal f = do
	v <- f <$> Annex.getGitConfig
	case v of
		HasConfig c -> return c
		DefaultConfig _ -> do
			r <- Annex.gitRepo
			m <- loadGlobalConfig
			let globalgc = extractGitConfig (r { config = m })
			-- This merge of the repo-global config and the git
			-- config makes all repository-global default
			-- values populate the GitConfig with HasConfig
			-- values, so it will only need to be done once.
			Annex.changeGitConfig (\gc -> mergeGitConfig gc globalgc)
			v' <- f <$> Annex.getGitConfig
			case v' of
				HasConfig c -> return c
				DefaultConfig d -> return d
