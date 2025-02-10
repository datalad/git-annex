{- Persistent sqlite RawFilePath support
 -
 - The functions below are copied from persistent-sqlite, but modified to
 - take a RawFilePath and ignore the sqlConnectionStr from the
 - SqliteConnectionInfo. This avoids encoding problems using Text
 - in some situations.
 -
 - This module is expected to eventually be supersceded by
 - persistent-sqlite getting support for OsString.
 -
 - Copyright (c) 2012 Michael Snoyman, http://www.yesodweb.com/
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -
 - Permission is hereby granted, free of charge, to any person obtaining
 - a copy of this software and associated documentation files (the
 - "Software"), to deal in the Software without restriction, including
 - without limitation the rights to use, copy, modify, merge, publish,
 - distribute, sublicense, and/or sell copies of the Software, and to
 - permit persons to whom the Software is furnished to do so, subject to
 - the following conditions:
 - 
 - The above copyright notice and this permission notice shall be
 - included in all copies or substantial portions of the Software.
 - 
 - THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 - EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 - MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 - NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 - LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 - OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 - WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

{-# LANGUAGE OverloadedStrings, CPP #-}

module Database.RawFilePath where

#if MIN_VERSION_persistent_sqlite(2,13,3)
import Database.Persist.Sqlite
import qualified Database.Sqlite as Sqlite
import Utility.RawFilePath (RawFilePath)
import qualified Control.Exception as E
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import UnliftIO.Resource (ResourceT, runResourceT)

openWith'
	:: RawFilePath 
	-> (SqlBackend -> Sqlite.Connection -> r)
	-> SqliteConnectionInfo
	-> LogFunc
	-> IO r
openWith' db f connInfo logFunc = do
	conn <- Sqlite.open' db
	backend <- wrapConnectionInfo connInfo conn logFunc `E.onException` Sqlite.close conn
	return $ f backend conn

runSqlite' :: (MonadUnliftIO m)
	=> RawFilePath
	-> ReaderT SqlBackend (NoLoggingT (ResourceT m)) a
	-> m a
runSqlite' connstr = runResourceT
	. runNoLoggingT
	. withSqliteConn' connstr
	. runSqlConn

withSqliteConn'
	:: (MonadUnliftIO m, MonadLoggerIO m)
	=> RawFilePath
	-> (SqlBackend -> m a)
	-> m a
withSqliteConn' connstr = withSqliteConnInfo' connstr $
	mkSqliteConnectionInfo mempty

runSqliteInfo'
	:: (MonadUnliftIO m)
	=> RawFilePath
	-> SqliteConnectionInfo
	-> ReaderT SqlBackend (NoLoggingT (ResourceT m)) a
	-> m a
runSqliteInfo' db conInfo = runResourceT
	. runNoLoggingT
	. withSqliteConnInfo' db conInfo
	. runSqlConn

withSqliteConnInfo'
	:: (MonadUnliftIO m, MonadLoggerIO m)
        => RawFilePath
	-> SqliteConnectionInfo
	-> (SqlBackend -> m a)
	-> m a
withSqliteConnInfo' db = withSqlConn . openWith' db const
#endif
