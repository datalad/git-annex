{- Enumeration of cleanup actions
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.CleanupActions where

import Types.UUID

import Utility.Url

data CleanupAction
	= RemoteCleanup UUID
	| StopHook UUID
	| FsckCleanup
	| SshCachingCleanup
	| AdjustedBranchUpdate
	| TorrentCleanup URLString
	| OtherTmpCleanup
	deriving (Eq, Ord)
