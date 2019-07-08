{- git-annex DesktopNotify type
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.DesktopNotify where

import Data.Monoid
import qualified Data.Semigroup as Sem
import Prelude

data DesktopNotify = DesktopNotify
	{ notifyStart :: Bool
	, notifyFinish :: Bool
	}
	deriving (Show)

instance Sem.Semigroup DesktopNotify where
	(DesktopNotify s1 f1) <> (DesktopNotify s2 f2) =
		DesktopNotify (s1 || s2) (f1 || f2)

instance Monoid DesktopNotify where
	mempty = DesktopNotify False False

mkNotifyStart :: DesktopNotify
mkNotifyStart = DesktopNotify True False

mkNotifyFinish :: DesktopNotify
mkNotifyFinish = DesktopNotify False True
