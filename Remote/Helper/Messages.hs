{- git-annex remote messages
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Remote.Helper.Messages where

import Annex.Common
import qualified Git
import qualified Types.Remote as Remote

class Describable a where
	describe :: a -> String

instance Describable Git.Repo where
	describe = Git.repoDescribe

instance Describable (Remote.RemoteA a) where
	describe = Remote.name

instance Describable String where
	describe = id

showChecking :: Describable a => a -> Annex ()
showChecking v = showAction $ "checking " ++ describe v

cantCheck :: Describable a => a -> e
cantCheck v = error $ "unable to check " ++ describe v

showLocking :: Describable a => a -> Annex ()
showLocking v = showAction $ "locking " ++ describe v
