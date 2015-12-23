{- Sqlite database of information about Keys, data types.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Database.Keys.Types (
	DbHandle(..)
) where

import qualified Database.Queue as H

newtype DbHandle = DbHandle H.DbQueue
