{- generic directory watching types
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.DirWatcher.Types where

import Common

type Hook a = Maybe (a -> Maybe FileStatus -> IO ())

data WatchHooks = WatchHooks
	{ addHook :: Hook FilePath
	, addSymlinkHook :: Hook FilePath
	, delHook :: Hook FilePath
	, delDirHook :: Hook FilePath
	, errHook :: Hook String -- error message
	, modifyHook :: Hook FilePath
	}

mkWatchHooks :: WatchHooks
mkWatchHooks = WatchHooks Nothing Nothing Nothing Nothing Nothing Nothing
