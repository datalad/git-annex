{- git-annex DesktopNotify type
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Types.DesktopNotify where

import Data.Monoid
#if MIN_VERSION_base(4,9,0)
import qualified Data.Semigroup as Sem
#endif
import Prelude

data DesktopNotify = DesktopNotify
	{ notifyStart :: Bool
	, notifyFinish :: Bool
	}
	deriving (Show)

appendDesktopNotify :: DesktopNotify -> DesktopNotify -> DesktopNotify
appendDesktopNotify (DesktopNotify s1 f1) (DesktopNotify s2 f2) =
	DesktopNotify (s1 || s2) (f1 || f2)

#if MIN_VERSION_base(4,9,0)
instance Sem.Semigroup DesktopNotify where
	(<>) = appendDesktopNotify
#endif

instance Monoid DesktopNotify where
	mempty = DesktopNotify False False
#if MIN_VERSION_base(4,11,0)
#elif MIN_VERSION_base(4,9,0)
	mappend = (Sem.<>)
#else
	mappend = appendDesktopNotify
#endif

mkNotifyStart :: DesktopNotify
mkNotifyStart = DesktopNotify True False

mkNotifyFinish :: DesktopNotify
mkNotifyFinish = DesktopNotify False True
