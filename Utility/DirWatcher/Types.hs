{- generic directory watching types
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.DirWatcher.Types (
	Hook,
	WatchHooks(..),
	mkWatchHooks,
) where

import Common

type Hook a = Maybe (a -> Maybe FileStatus -> IO ())

data WatchHooks = WatchHooks
	{ addHook :: Hook OsPath
	, addSymlinkHook :: Hook OsPath
	, delHook :: Hook OsPath
	, delDirHook :: Hook OsPath
	, errHook :: Hook String -- error message
	, modifyHook :: Hook OsPath
	}

mkWatchHooks :: WatchHooks
mkWatchHooks = WatchHooks Nothing Nothing Nothing Nothing Nothing Nothing
