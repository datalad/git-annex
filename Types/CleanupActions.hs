{- Enumeration of cleanup actions
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.CleanupActions where

import Types.UUID

data CleanupAction
	= RemoteCleanup UUID
	| StopHook UUID
	| FsckCleanup
	| SshCachingCleanup
	deriving (Eq, Ord)
