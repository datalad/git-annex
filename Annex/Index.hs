{- Using other git index files
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Index (
	withIndexFile,
) where

import qualified Control.Exception as E

import Common.Annex
import Git.Types
import qualified Annex
import Utility.Env
import Annex.Exception

{- Runs an action using a different git index file. -}
withIndexFile :: FilePath -> Annex a -> Annex a
withIndexFile f a = do
	g <- gitRepo
#ifdef __ANDROID__
	{- This should not be necessary on Android, but there is some
	 - weird getEnvironment breakage. See
	 - https://github.com/neurocyte/ghc-android/issues/7
	 - Use getEnv to get some key environment variables that
	 - git expects to have. -}
	let keyenv = words "USER PATH GIT_EXEC_PATH HOSTNAME HOME"
	let getEnvPair k = maybe Nothing (\v -> Just (k, v)) <$> getEnv k
	e <- liftIO $ catMaybes <$> forM keyenv getEnvPair
	let e' = ("GIT_INDEX_FILE", f):e
#else
	e <- liftIO getEnvironment
	let e' = addEntry "GIT_INDEX_FILE" f e
#endif
	let g' = g { gitEnv = Just e' }

	r <- tryAnnex $ do
		Annex.changeState $ \s -> s { Annex.repo = g' }
		a
	Annex.changeState $ \s -> s { Annex.repo = (Annex.repo s) { gitEnv = gitEnv g} }
	either E.throw return r
