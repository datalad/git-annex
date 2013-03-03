{- common functions for special remotes
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Special where

import qualified Data.Map as M

import Common.Annex
import Types.Remote
import qualified Git
import qualified Git.Command
import qualified Git.Construct

{- Special remotes don't have a configured url, so Git.Repo does not
 - automatically generate remotes for them. This looks for a different
 - configuration key instead.
 -}
findSpecialRemotes :: String -> Annex [Git.Repo]
findSpecialRemotes s = do
	m <- fromRepo Git.config
	liftIO $ mapM construct $ remotepairs m
  where
	remotepairs = M.toList . M.filterWithKey match
	construct (k,_) = Git.Construct.remoteNamedFromKey k Git.Construct.fromUnknown
	match k _ = startswith "remote." k && endswith (".annex-"++s) k

{- Sets up configuration for a special remote in .git/config. -}
gitConfigSpecialRemote :: UUID -> RemoteConfig -> String -> String -> Annex ()
gitConfigSpecialRemote u c k v = do
	set ("annex-"++k) v
	set ("annex-uuid") (fromUUID u)
  where
	set a b = inRepo $ Git.Command.run
		[Param "config", Param (configsetting a), Param b]
	remotename = fromJust (M.lookup "name" c)
	configsetting s = "remote." ++ remotename ++ "." ++ s
