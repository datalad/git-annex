{- Helpers for tor remotes.
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Tor where

import Annex.Common

import Network.Socket

torHandle :: Socket -> IO Handle
torHandle s = do
	h <- socketToHandle s ReadWriteMode
	hSetBuffering h LineBuffering
	hSetBinaryMode h False
	fileEncoding h
	return h
