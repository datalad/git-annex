{- Temporarily changing the files git uses.
 -
 - Copyright 2014-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.GitOverlay where

import qualified Control.Exception as E

import Annex.Common
import Git
import Git.Types
import Git.Index
import Git.Env
import qualified Annex

{- Runs an action using a different git index file. -}
withIndexFile :: FilePath -> Annex a -> Annex a
withIndexFile f a = do
	f' <- inRepo $ indexEnvVal f
	withAltRepo
		(\g -> addGitEnv g indexEnv f')
		(\g g' -> g' { gitEnv = gitEnv g })
		a

{- Runs an action using a different git work tree.
 -
 - Smudge and clean filters are disabled in this work tree. -}
withWorkTree :: FilePath -> Annex a -> Annex a
withWorkTree d = withAltRepo
	(\g -> return $ g { location = modlocation (location g), gitGlobalOpts = gitGlobalOpts g ++ disableSmudgeConfig })
	(\g g' -> g' { location = location g, gitGlobalOpts = gitGlobalOpts g })
  where
	modlocation l@(Local {}) = l { worktree = Just d }
	modlocation _ = error "withWorkTree of non-local git repo"
	disableSmudgeConfig = map Param
		[ "-c", "filter.annex.smudge="
		, "-c", "filter.annex.clean="
		]

{- Runs an action with the git index file and HEAD, and a few other
 - files that are related to the work tree coming from an overlay
 - directory other than the usual. This is done by pointing
 - GIT_COMMON_DIR at the regular git directory, and GIT_DIR at the
 - overlay directory.
 -
 - Needs git 2.2.0 or newer.
 -}
withWorkTreeRelated :: FilePath -> Annex a -> Annex a
withWorkTreeRelated d = withAltRepo modrepo unmodrepo
  where
	modrepo g = do
		g' <- addGitEnv g "GIT_COMMON_DIR" =<< absPath (localGitDir g)
		g'' <- addGitEnv g' "GIT_DIR" d
		return (g'' { gitEnvOverridesGitDir = True })
	unmodrepo g g' = g'
		{ gitEnv = gitEnv g
		, gitEnvOverridesGitDir = gitEnvOverridesGitDir g
		}

withAltRepo 
	:: (Repo -> IO Repo)
	-- ^ modify Repo
	-> (Repo -> Repo -> Repo)
	-- ^ undo modifications; first Repo is the original and second
	-- is the one after running the action.
	-> Annex a
	-> Annex a
withAltRepo modrepo unmodrepo a = do
	g <- gitRepo
	g' <- liftIO $ modrepo g
	r <- tryNonAsync $ do
		Annex.changeState $ \s -> s { Annex.repo = g' }
		a
	Annex.changeState $ \s -> s { Annex.repo = unmodrepo g (Annex.repo s) }
	either E.throw return r
