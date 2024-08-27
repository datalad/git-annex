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
import Data.Unique
import Text.Read
import System.Process (Pid)
import Utility.Split

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
	= LiveUpdate
		{ liveUpdateNeeded :: MVar ()
		, liveUpdateStart :: MVar ()
		, liveUpdateReady :: MVar ()
		, liveUpdateDone :: MVar (Maybe (UUID, Key, SizeChange, MVar ()))
		}
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

data SizeChangeId = SizeChangeId
	{ sizeChangeUniqueId :: Int
	-- ^ unique per process
	, sizeChangeProcessId :: Integer
	-- ^ a pid, using Integer for portability
	}
	deriving (Show, Eq)

mkSizeChangeId :: Pid -> IO SizeChangeId
mkSizeChangeId pid = do
	u <- newUnique
	return $ SizeChangeId
		{ sizeChangeProcessId = fromIntegral pid
		, sizeChangeUniqueId = hashUnique u
		}

instance PersistField SizeChangeId where
        toPersistValue cid = toPersistValue $ 
		show (sizeChangeProcessId cid) ++ ":" ++ 
		show (sizeChangeUniqueId cid)
	fromPersistValue b = fromPersistValue b >>= parse
	  where
		parse s = maybe
			(Left $ T.pack $ "bad serialized SizeChangeId " ++ show s)
			Right
			(parse' s)
		parse' s = case splitc ':' s of
			(pid:uid:[]) -> SizeChangeId
				<$> readMaybe pid
				<*> readMaybe uid
			_ -> Nothing

instance PersistFieldSql SizeChangeId where
        sqlType _ = SqlString
