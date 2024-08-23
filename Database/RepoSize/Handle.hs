{- Sqlite database used to track the sizes of repositories.
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Database.RepoSize.Handle where

import qualified Database.Handle as H

-- Contains Nothing if the database was not able to be opened due to
-- permissions.
newtype RepoSizeHandle = RepoSizeHandle (Maybe H.DbHandle)
