{- Handle for the Keys database.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Database.Keys.Handle (
	DbHandle,
	newDbHandle,
	DbState(..),
	DbWasOpen(..),
	withDbState,
	flushDbQueue,
	closeDbHandle,
) where

import qualified Database.Queue as H
import Database.Keys.Tables
import Utility.Exception
import Utility.DebugLocks

import Control.Concurrent
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Applicative
import Prelude

-- The MVar is always left full except when actions are run
-- that access the database.
newtype DbHandle = DbHandle (MVar DbState)

-- The database can be closed or open, but it also may have been
-- tried to open (for read) and didn't exist yet or is not readable.
data DbState 
	= DbClosed DbWasOpen
	| DbOpen (H.DbQueue, DbTablesChanged)
	| DbUnavailable

-- Was the database previously opened by this process?
data DbWasOpen = DbWasOpen Bool

newDbHandle :: IO DbHandle
newDbHandle = DbHandle <$> newMVar (DbClosed (DbWasOpen False))

-- Runs an action on the state of the handle, which can change its state.
-- The MVar is empty while the action runs, which blocks other users
-- of the handle from running.
withDbState
	:: (MonadIO m, MonadCatch m)
	=> DbHandle
	-> (DbState -> m (v, DbState))
	-> m v
withDbState (DbHandle mvar) a = do
	st <- liftIO $ debugLocks $ takeMVar mvar
	go st `onException` (liftIO $ debugLocks $ putMVar mvar st)
  where
	go st = do
		(v, st') <- a st
		liftIO $ debugLocks $ putMVar mvar st'
		return v

flushDbQueue :: DbHandle -> IO ()
flushDbQueue h = withDbState h go
  where
	go (DbOpen (qh, _)) = do
		H.flushDbQueue qh
		return ((), DbOpen (qh, mempty))
	go st = return ((), st)

closeDbHandle :: DbHandle -> IO ()
closeDbHandle h = withDbState h go
  where
	go (DbOpen (qh, _)) = do
		H.closeDbQueue qh
		return ((), DbClosed (DbWasOpen True))
	go st = return ((), st)
