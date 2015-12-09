{- Sqlite database used for tracking a key's associated files, data types.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Database.AssociatedFiles.Types (
	DbHandle(..)
) where

import qualified Database.Handle as H

newtype DbHandle = DbHandle H.DbHandle
