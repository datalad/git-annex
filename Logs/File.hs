{- git-annex log files
 -
 - Copyright 2018-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, BangPatterns #-}

module Logs.File (
	writeLogFile,
	withLogHandle,
	appendLogFile,
	modifyLogFile,
	streamLogFile,
	streamLogFileUnsafe,
	checkLogFile,
	calcLogFile,
	calcLogFileUnsafe,
	fileLines,
	fileLines',
) where

import Annex.Common
import Annex.Perms
import Annex.LockFile
import Annex.ReplaceFile
import Utility.Tmp
import qualified Utility.FileIO as F

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

-- | Writes content to a file, replacing the file atomically, and
-- making the new file have whatever permissions the git repository is
-- configured to use. Creates the parent directory when necessary.
writeLogFile :: OsPath -> String -> Annex ()
writeLogFile f c = createDirWhenNeeded f $ viaTmp writelog f c
  where
	writelog tmp c' = do
		liftIO $ writeFileString tmp c'
		setAnnexFilePerm tmp

-- | Runs the action with a handle connected to a temp file.
-- The temp file replaces the log file once the action succeeds.
withLogHandle :: OsPath -> (Handle -> Annex a) -> Annex a
withLogHandle f a = do
	createAnnexDirectory (parentDir f)
	replaceGitAnnexDirFile f $ \tmp ->
		bracket (setup tmp) cleanup a
  where
	setup tmp = do
		setAnnexFilePerm tmp
		liftIO $ F.openFile tmp WriteMode
	cleanup h = liftIO $ hClose h

-- | Appends a line to a log file, first locking it to prevent
-- concurrent writers.
appendLogFile :: OsPath -> OsPath -> L.ByteString -> Annex ()
appendLogFile f lck c = 
	createDirWhenNeeded f $
		withExclusiveLock lck $ do
			liftIO $ F.withFile f AppendMode $
				\h -> L8.hPutStrLn h c
			setAnnexFilePerm f

-- | Modifies a log file.
--
-- If the function does not make any changes, avoids rewriting the file
-- for speed, but that does mean the whole file content has to be buffered
-- in memory.
--
-- The file is locked to prevent concurrent writers, and it is written
-- atomically.
modifyLogFile :: OsPath -> OsPath -> ([L.ByteString] -> [L.ByteString]) -> Annex ()
modifyLogFile f lck modf = withExclusiveLock lck $ do
	ls <- liftIO $ fromMaybe []
		<$> tryWhenExists (fileLines <$> F.readFile f)
	let ls' = modf ls
	when (ls' /= ls) $
		createDirWhenNeeded f $
			viaTmp writelog f (L8.unlines ls')
  where
	writelog lf b = do
		liftIO $ F.writeFile lf b
		setAnnexFilePerm lf

-- | Checks the content of a log file to see if any line matches.
checkLogFile :: OsPath -> OsPath -> (L.ByteString -> Bool) -> Annex Bool
checkLogFile f lck matchf = withSharedLock lck $ bracket setup cleanup go
  where
	setup = liftIO $ tryWhenExists $ F.openFile f ReadMode
	cleanup Nothing = noop
	cleanup (Just h) = liftIO $ hClose h
	go Nothing = return False
	go (Just h) = do
		!r <- liftIO (any matchf . fileLines <$> L.hGetContents h)
		return r

-- | Folds a function over lines of a log file to calculate a value.
calcLogFile :: OsPath -> OsPath -> t -> (L.ByteString -> t -> t) -> Annex t
calcLogFile f lck start update =
	withSharedLock lck $ calcLogFileUnsafe f start update

-- | Unsafe version that does not do locking.
calcLogFileUnsafe :: OsPath -> t -> (L.ByteString -> t -> t) -> Annex t
calcLogFileUnsafe f start update = bracket setup cleanup go
  where
	setup = liftIO $ tryWhenExists $ F.openFile f ReadMode
	cleanup Nothing = noop
	cleanup (Just h) = liftIO $ hClose h
	go Nothing = return start
	go (Just h) = go' start =<< liftIO (fileLines <$> L.hGetContents h)
	go' v [] = return v
	go' v (l:ls) = do
		let !v' = update l v
		go' v' ls

-- | Streams lines from a log file, passing each line to the processor,
-- and then empties the file at the end.
--
-- If the processor is interrupted or throws an exception, the log file is
-- left unchanged.
--
-- There is also a finalizer, that is run once all lines have been
-- streamed. It is run even if the log file does not exist. If the
-- finalizer throws an exception, the log file is left unchanged.
-- 
-- Locking is used to prevent writes to to the log file while this
-- is running.
streamLogFile :: OsPath -> OsPath -> Annex () -> (String -> Annex ()) -> Annex ()
streamLogFile f lck finalizer processor = 
	withExclusiveLock lck $ do
		streamLogFileUnsafe f finalizer processor
		liftIO $ F.writeFile' f mempty
		setAnnexFilePerm f

-- Unsafe version that does not do locking, and does not empty the file
-- at the end.
streamLogFileUnsafe :: OsPath -> Annex () -> (String -> Annex ()) -> Annex ()
streamLogFileUnsafe f finalizer processor = bracketOnError setup cleanup go
  where
	setup = liftIO $ tryWhenExists $ F.openFile f ReadMode 
	cleanup Nothing = noop
	cleanup (Just h) = liftIO $ hClose h
	go Nothing = finalizer
	go (Just h) = do
		liftIO $ fileEncoding h
		mapM_ processor =<< liftIO (lines <$> hGetContents h)
		liftIO $ hClose h
		finalizer

createDirWhenNeeded :: OsPath -> Annex () -> Annex ()
createDirWhenNeeded f a = a `catchNonAsync` \_e -> do
	-- Most of the time, the directory will exist, so this is only
	-- done if writing the file fails.
	createAnnexDirectory (parentDir f)
	a
