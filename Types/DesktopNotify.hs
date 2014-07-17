{- git-annex DesktopNotify type
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.DesktopNotify where

import Data.Monoid

data DesktopNotify = DesktopNotify
	{ notifyStart :: Bool
	, notifyFinish :: Bool
	}
	deriving (Show)

instance Monoid DesktopNotify where
	mempty = DesktopNotify False False
	mappend (DesktopNotify s1 f1) (DesktopNotify s2 f2) =
		DesktopNotify (s1 || s2) (f1 || f2)

mkNotifyStart :: DesktopNotify
mkNotifyStart = DesktopNotify True False

mkNotifyFinish :: DesktopNotify
mkNotifyFinish = DesktopNotify False True
