{- Using other git index files
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Index (withIndexFile) where

import qualified Control.Exception as E

import Annex.Common
import Git.Types
import Git.Env
import qualified Annex

{- Runs an action using a different git index file. -}
withIndexFile :: FilePath -> Annex a -> Annex a
withIndexFile f a = do
	g <- gitRepo
	g' <- liftIO $ addGitEnv g "GIT_INDEX_FILE" f

	r <- tryNonAsync $ do
		Annex.changeState $ \s -> s { Annex.repo = g' }
		a
	Annex.changeState $ \s -> s { Annex.repo = (Annex.repo s) { gitEnv = gitEnv g} }
	either E.throw return r
