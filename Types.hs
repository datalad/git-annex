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
	UUID
) where

import Annex
import Types.Backend
import Types.Key
import Types.UUID
