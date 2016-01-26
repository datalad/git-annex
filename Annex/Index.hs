{- Using other git index files
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Index (
	withIndexFile,
	addGitEnv,
) where

import qualified Control.Exception as E

import Annex.Common
import Git.Types
import qualified Annex
import Utility.Env

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

addGitEnv :: Repo -> String -> String -> IO Repo
addGitEnv g var val = do
	e <- maybe copyenv return (gitEnv g)
	let e' = addEntry var val e
	return $ g { gitEnv = Just e' }
  where
	copyenv = do
#ifdef __ANDROID__
		{- This should not be necessary on Android, but there is some
		 - weird getEnvironment breakage. See
		 - https://github.com/neurocyte/ghc-android/issues/7
		 - Use getEnv to get some key environment variables that
		 - git expects to have. -}
		let keyenv = words "USER PATH GIT_EXEC_PATH HOSTNAME HOME"
		let getEnvPair k = maybe Nothing (\v -> Just (k, v)) <$> getEnv k
		liftIO $ catMaybes <$> forM keyenv getEnvPair
#else
		liftIO getEnvironment
#endif
