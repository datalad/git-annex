{- git-annex remote messages
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Messages where

import Common.Annex
import qualified Git
import qualified Types.Remote as Remote

showChecking :: Git.Repo -> Annex ()
showChecking r = showAction $ "checking " ++ Git.repoDescribe r

class Checkable a where
	descCheckable :: a -> String

instance Checkable Git.Repo where
	descCheckable = Git.repoDescribe

instance Checkable (Remote.RemoteA a) where
	descCheckable = Remote.name

cantCheck :: Checkable a => a -> e
cantCheck v = error $ "unable to check " ++ descCheckable v
