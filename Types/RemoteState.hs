{- git-annex remote state handle type
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.RemoteState where

import Types.UUID

{- When there is per-remote state, remotes are identified by UUID.
 -
 - However, sameas remotes mean that two different Remote implementations
 - can be used for the same underlying data store. To avoid them using
 - state in conflicting ways, a different UUID needs to be used for each
 - additional sameas remote.
 -}
newtype RemoteStateHandle = RemoteStateHandle UUID
