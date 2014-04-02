{- network functions
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Network where

import Utility.Process
import Utility.Exception

import Control.Applicative

{- Haskell lacks uname(2) bindings, except in the
 - Bindings.Uname addon. Rather than depend on that,
 - use uname -n when available. -}
getHostname :: IO (Maybe String)
getHostname = catchMaybeIO uname_node
  where
	uname_node = takeWhile (/= '\n') <$> readProcess "uname" ["-n"]
