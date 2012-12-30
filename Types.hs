{- git-annex abstract data types
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types (
	Annex,
	Backend,
	Key,
	UUID(..),
	Config(..),
	Remote,
	RemoteType,
	Option,
	MeterUpdate
) where

import Annex
import Types.Backend
import Types.Config
import Types.Key
import Types.UUID
import Types.Remote
import Types.Option
import Types.Meters

type Backend = BackendA Annex
type Remote = RemoteA Annex
type RemoteType = RemoteTypeA Annex
