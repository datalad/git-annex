{- Handle for the Keys database.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Database.Keys.Handle (
	DbHandle,
	newDbHandle,
	DbState(..),
	withDbState,
	flushDbQueue,
) where

import qualified Database.Queue as H
import Utility.Exception

import Control.Concurrent
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Applicative
import Prelude

-- The MVar is always left full except when actions are run
-- that access the database.
newtype DbHandle = DbHandle (MVar DbState)

-- The database can be closed or open, but it also may have been
-- tried to open (for read) and didn't exist yet.
data DbState = DbClosed | DbOpen H.DbQueue | DbEmpty

newDbHandle :: IO DbHandle
newDbHandle = DbHandle <$> newMVar DbClosed

-- Runs an action on the state of the handle, which can change its state.
-- The MVar is empty while the action runs, which blocks other users
-- of the handle from running.
withDbState
	:: (MonadIO m, MonadCatch m)
	=> DbHandle
	-> (DbState
	-> m (v, DbState))
	-> m v
withDbState (DbHandle mvar) a = do
	st <- liftIO $ takeMVar mvar
	go st `onException` (liftIO $ putMVar mvar st)
  where
	go st = do
		(v, st') <- a st
		liftIO $ putMVar mvar st'
		return v

flushDbQueue :: DbHandle -> IO ()
flushDbQueue (DbHandle mvar) = go =<< readMVar mvar
  where
	go (DbOpen qh) = H.flushDbQueue qh
	go _ = return ()
