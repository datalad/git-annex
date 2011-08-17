{- common functions for special remotes
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Special where

import qualified Data.Map as M
import Data.Maybe
import Data.String.Utils
import Control.Monad.State (liftIO)

import Types
import Types.Remote
import qualified Git
import qualified Annex
import UUID
import Utility

{- Special remotes don't have a configured url, so Git.Repo does not
 - automatically generate remotes for them. This looks for a different
 - configuration key instead.
 -}
findSpecialRemotes :: String -> Annex [Git.Repo]
findSpecialRemotes s = do
	g <- Annex.gitRepo
	return $ map construct $ remotepairs g
	where
		remotepairs r = M.toList $ M.filterWithKey match $ Git.configMap r
		construct (k,_) = Git.repoRemoteNameSet Git.repoFromUnknown k
		match k _ = startswith "remote." k && endswith (".annex-"++s) k

{- Sets up configuration for a special remote in .git/config. -}
gitConfigSpecialRemote :: UUID -> RemoteConfig -> String -> String -> Annex ()
gitConfigSpecialRemote u c k v = do
	g <- Annex.gitRepo
	liftIO $ do
		Git.run g "config" [Param (configsetting $ "annex-"++k), Param v]
		Git.run g "config" [Param (configsetting "annex-uuid"), Param u]
	where
		remotename = fromJust (M.lookup "name" c)
		configsetting s = "remote." ++ remotename ++ "." ++ s
