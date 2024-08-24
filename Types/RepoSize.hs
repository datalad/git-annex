{- git-annex repo sizes types
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Types.RepoSize where

import Types.UUID
import Types.Key

import Control.Concurrent
import Database.Persist.Sql hiding (Key)
import qualified Data.Text as T

-- The current size of a repo.
newtype RepoSize = RepoSize { fromRepoSize :: Integer }
	deriving (Show, Eq, Ord, Num)

-- The maximum size of a repo.
newtype MaxSize = MaxSize { fromMaxSize :: Integer }
	deriving (Show, Eq, Ord)

-- Used when an action is in progress that will change the current size of
-- a repository.
--
-- This allows other concurrent changes to the same repository take
-- the changes to its size into account. If NoLiveUpdate is used, it
-- prevents that.
data LiveUpdate
	= LiveUpdate (MVar ()) (MVar (UUID, Key, SizeChange)) (MVar ())
	| NoLiveUpdate

data SizeChange = AddingKey | RemovingKey
	deriving (Show, Eq)

instance PersistField SizeChange where
        toPersistValue AddingKey = toPersistValue (1 :: Int)
	toPersistValue RemovingKey = toPersistValue (-1 :: Int)
	fromPersistValue b = fromPersistValue b >>= \case
		(1 :: Int) -> Right AddingKey
		-1 -> Right RemovingKey
		v -> Left $ T.pack $ "bad serialized SizeChange "++ show v

instance PersistFieldSql SizeChange where
        sqlType _ = SqlInt32
