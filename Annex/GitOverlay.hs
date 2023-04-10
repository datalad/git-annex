{- Temporarily changing how git-annex runs git commands.
 -
 - Copyright 2014-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.GitOverlay (
	module Annex.GitOverlay,
	AltIndexFile(..),
) where

import qualified Control.Exception as E

import Annex.Common
import Types.IndexFiles
import Git
import Git.Types
import Git.Index
import Git.Env
import qualified Annex
import qualified Annex.Queue
import Config.Smudge

{- Runs an action using a different git index file. -}
withIndexFile :: AltIndexFile -> (FilePath -> Annex a) -> Annex a
withIndexFile i = withAltRepo usecachedgitenv restoregitenv
  where
	-- This is an optimisation. Since withIndexFile is run repeatedly,
	-- typically with the same file, and addGitEnv uses the slow
	-- getEnvironment when gitEnv is Nothing, and has to do a
	-- nontrivial amount of work, we cache the modified environment
	-- the first time, and reuse it in subsequent calls for the same
	-- index file.
	--
	-- (This could be done at another level; eg when creating the
	-- Git object in the first place, but it's more efficient to let
	-- the environment be inherited in all calls to git where it
	-- does not need to be modified.)
	--
	-- Also, the use of AltIndexFile avoids needing to construct
	-- the FilePath each time, which saves enough time to be worth the
	-- added complication.
	usecachedgitenv g = case gitEnv g of
		Nothing -> Annex.withState $ \s -> case Annex.cachedgitenv s of
			Just (cachedi, cachedf, cachede) | i == cachedi ->
				return (s, (g { gitEnv = Just cachede }, cachedf))
			_ -> do
				r@(g', f) <- addindex g
				let cache = (,,)
					<$> Just i
					<*> Just f
					<*> gitEnv g'
				return (s { Annex.cachedgitenv = cache }, r)
		Just _ -> liftIO $ addindex g
	
	addindex g = do
		f <- indexEnvVal $ case i of
			AnnexIndexFile -> gitAnnexIndex g
			ViewIndexFile -> gitAnnexViewIndex g
		g' <- addGitEnv g indexEnv f
		return (g', f)
	
	restoregitenv g g' = g' { gitEnv = gitEnv g }

{- Runs an action using a different git work tree.
 -
 - Smudge and clean filters are disabled in this work tree. -}
withWorkTree :: FilePath -> Annex a -> Annex a
withWorkTree d a = withAltRepo
	(\g -> return $ (g { location = modlocation (location g), gitGlobalOpts = gitGlobalOpts g ++ bypassSmudgeConfig }, ()))
	(\g g' -> g' { location = location g, gitGlobalOpts = gitGlobalOpts g })
	(const a)
  where
	modlocation l@(Local {}) = l { worktree = Just (toRawFilePath d) }
	modlocation _ = giveup "withWorkTree of non-local git repo"

{- Runs an action with the git index file and HEAD, and a few other
 - files that are related to the work tree coming from an overlay
 - directory other than the usual. This is done by pointing
 - GIT_COMMON_DIR at the regular git directory, and GIT_DIR at the
 - overlay directory.
 -
 - Needs git 2.2.0 or newer.
 -}
withWorkTreeRelated :: FilePath -> Annex a -> Annex a
withWorkTreeRelated d a = withAltRepo modrepo unmodrepo (const a)
  where
	modrepo g = liftIO $ do
		g' <- addGitEnv g "GIT_COMMON_DIR" . fromRawFilePath
			=<< absPath (localGitDir g)
		g'' <- addGitEnv g' "GIT_DIR" d
		return (g'' { gitEnvOverridesGitDir = True }, ())
	unmodrepo g g' = g'
		{ gitEnv = gitEnv g
		, gitEnvOverridesGitDir = gitEnvOverridesGitDir g
		}

withAltRepo 
	:: (Repo -> Annex (Repo, t))
	-- ^ modify Repo
	-> (Repo -> Repo -> Repo)
	-- ^ undo modifications; first Repo is the original and second
	-- is the one after running the action.
	-> (t -> Annex a)
	-> Annex a
withAltRepo modrepo unmodrepo a = do
	g <- gitRepo
	(g', t) <- modrepo g
	q <- Annex.Queue.get
	v <- tryNonAsync $ do
		Annex.changeState $ \s -> s
			{ Annex.repo = g'
			-- Start a separate queue for any changes made
			-- with the modified repo.
			, Annex.repoqueue = Nothing
			}
		a t
	void $ tryNonAsync Annex.Queue.flush
	Annex.changeState $ \s -> s
		{ Annex.repo = unmodrepo g (Annex.repo s)
		, Annex.repoqueue = Just q
		}
	either E.throw return v
