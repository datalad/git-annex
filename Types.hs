{- git-annex abstract data types
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types (
	Annex,
	Backend,
	Key,
	AssociatedFile(..),
	UUID(..),
	GitConfig(..),
	RemoteGitConfig(..),
	Remote,
	RemoteType,
	Transfer,
	TransferInfo,
) where

import Annex
import Types.Backend
import Types.GitConfig
import Types.Key
import Types.UUID
import Types.Remote
import Types.Transfer

type Backend = BackendA Annex
type Remote = RemoteA Annex
type RemoteType = RemoteTypeA Annex
type TransferInfo = TransferInfoA Annex
