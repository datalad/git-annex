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
import Data.Unique
import Text.Read
import System.Process (Pid)
import qualified Data.Text as T
import qualified Data.Set as S

-- The current size of a repo.
newtype RepoSize = RepoSize { fromRepoSize :: Integer }
	deriving (Show, Eq, Ord, Num)

-- The maximum size of a repo.
newtype MaxSize = MaxSize { fromMaxSize :: Integer }
	deriving (Show, Eq, Ord)

-- An offset to the size of a repo.
newtype SizeOffset = SizeOffset { fromSizeChange :: Integer }
	deriving (Show, Eq, Ord, Num)

-- Used when an action is in progress that will change the current size of
-- a repository.
--
-- This allows other concurrent changes to the same repository take
-- the changes to its size into account. If NoLiveUpdate is used, it
-- prevents that.
data LiveUpdate
	= LiveUpdate
		{ liveUpdateNeeded :: MVar ()
		, liveUpdateStart :: MVar ()
		, liveUpdateReady :: MVar ()
		, liveUpdateDone :: MVar (Maybe (UUID, Key, SizeChange, MVar ()))
		}
	| NoLiveUpdate

data SizeChange = AddingKey | RemovingKey
	deriving (Show, Eq, Ord)

instance PersistField SizeChange where
        toPersistValue AddingKey = toPersistValue (1 :: Int)
	toPersistValue RemovingKey = toPersistValue (-1 :: Int)
	fromPersistValue b = fromPersistValue b >>= \case
		(1 :: Int) -> Right AddingKey
		-1 -> Right RemovingKey
		v -> Left $ T.pack $ "bad serialized SizeChange "++ show v

instance PersistFieldSql SizeChange where
        sqlType _ = SqlInt32

data SizeChangeId = SizeChangeId
	{ sizeChangeUniqueId :: SizeChangeUniqueId
	, sizeChangeProcessId :: SizeChangeProcessId
	}
	deriving (Show, Eq, Ord)

-- A unique value for the current process.
newtype SizeChangeUniqueId = SizeChangeUniqueId Int
	deriving (Show, Eq, Ord)

-- A pid, using Integer for portability
newtype SizeChangeProcessId = SizeChangeProcessId Integer
	deriving (Show, Eq, Ord)

mkSizeChangeId :: Pid -> IO SizeChangeId
mkSizeChangeId pid = do
	u <- newUnique
	return $ SizeChangeId
		{ sizeChangeUniqueId = 
			SizeChangeUniqueId $ hashUnique u
		, sizeChangeProcessId = 
			SizeChangeProcessId $ fromIntegral pid
		}

instance PersistField SizeChangeUniqueId where
        toPersistValue (SizeChangeUniqueId i) = toPersistValue (show i)
	fromPersistValue b = fromPersistValue b >>= parse
	  where
		parse s = maybe
			(Left $ T.pack $ "bad serialized SizeChangeUniqueId " ++ show s)
			Right
			(SizeChangeUniqueId <$> readMaybe s)

instance PersistFieldSql SizeChangeUniqueId where
        sqlType _ = SqlString

instance PersistField SizeChangeProcessId where
        toPersistValue (SizeChangeProcessId i) = toPersistValue (show i)
	fromPersistValue b = fromPersistValue b >>= parse
	  where
		parse s = maybe
			(Left $ T.pack $ "bad serialized SizeChangeProcessId " ++ show s)
			Right
			(SizeChangeProcessId <$> readMaybe s)

instance PersistFieldSql SizeChangeProcessId where
        sqlType _ = SqlString

newtype StaleSizeChanger = StaleSizeChanger
	{ staleSizeChangerProcessId :: SizeChangeProcessId }
	deriving (Show, Eq, Ord)

isStaleSizeChangeId :: S.Set StaleSizeChanger -> SizeChangeId -> Bool
isStaleSizeChangeId s cid =
	StaleSizeChanger (sizeChangeProcessId cid) `S.member` s
