{- git-annex remote messages
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Messages where

import Common.Annex
import qualified Git

showChecking :: Git.Repo -> Annex ()
showChecking r = showAction $ "checking " ++ Git.repoDescribe r

cantCheck :: Git.Repo -> a
cantCheck r = error $ "unable to check " ++ Git.repoDescribe r
