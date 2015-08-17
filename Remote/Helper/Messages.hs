{- git-annex remote messages
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Remote.Helper.Messages where

import Common.Annex
import qualified Git
import qualified Types.Remote as Remote

class Checkable a where
	descCheckable :: a -> String

instance Checkable Git.Repo where
	descCheckable = Git.repoDescribe

instance Checkable (Remote.RemoteA a) where
	descCheckable = Remote.name

instance Checkable String where
	descCheckable = id

showChecking :: Checkable a => a -> Annex ()
showChecking v = showAction $ "checking " ++ descCheckable v

cantCheck :: Checkable a => a -> e
cantCheck v = error $ "unable to check " ++ descCheckable v
