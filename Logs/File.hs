{- git-annex log files
 -
 - Copyright 2018-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Logs.File (
	writeLogFile,
	withLogHandle,
	appendLogFile,
	modifyLogFile,
	streamLogFile,
	checkLogFile,
) where

import Annex.Common
import Annex.Perms
import Annex.LockFile
import Annex.ReplaceFile
import Utility.Tmp

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

-- | Writes content to a file, replacing the file atomically, and
-- making the new file have whatever permissions the git repository is
-- configured to use. Creates the parent directory when necessary.
writeLogFile :: RawFilePath -> String -> Annex ()
writeLogFile f c = createDirWhenNeeded f $ viaTmp writelog (fromRawFilePath f) c
  where
	writelog tmp c' = do
		liftIO $ writeFile tmp c'
		setAnnexFilePerm (toRawFilePath tmp)

-- | Runs the action with a handle connected to a temp file.
-- The temp file replaces the log file once the action succeeds.
withLogHandle :: RawFilePath -> (Handle -> Annex a) -> Annex a
withLogHandle f a = do
	createAnnexDirectory (parentDir f)
	replaceGitAnnexDirFile (fromRawFilePath f) $ \tmp ->
		bracket (setup tmp) cleanup a
  where
	setup tmp = do
		setAnnexFilePerm (toRawFilePath tmp)
		liftIO $ openFile tmp WriteMode
	cleanup h = liftIO $ hClose h

-- | Appends a line to a log file, first locking it to prevent
-- concurrent writers.
appendLogFile :: RawFilePath -> RawFilePath -> L.ByteString -> Annex ()
appendLogFile f lck c = 
	createDirWhenNeeded f $
		withExclusiveLock lck $ do
			liftIO $ withFile f' AppendMode $
				\h -> L8.hPutStrLn h c
			setAnnexFilePerm (toRawFilePath f')
  where
	f' = fromRawFilePath f

-- | Modifies a log file.
--
-- If the function does not make any changes, avoids rewriting the file
-- for speed, but that does mean the whole file content has to be buffered
-- in memory.
--
-- The file is locked to prevent concurrent writers, and it is written
-- atomically.
modifyLogFile :: RawFilePath -> RawFilePath -> ([L.ByteString] -> [L.ByteString]) -> Annex ()
modifyLogFile f lck modf = withExclusiveLock lck $ do
	ls <- liftIO $ fromMaybe []
		<$> tryWhenExists (L8.lines <$> L.readFile f')
	let ls' = modf ls
	when (ls' /= ls) $
		createDirWhenNeeded f $
			viaTmp writelog f' (L8.unlines ls')
  where
	f' = fromRawFilePath f
	writelog lf b = do
		liftIO $ L.writeFile lf b
		setAnnexFilePerm (toRawFilePath lf)

-- | Checks the content of a log file to see if any line matches.
--
-- This can safely be used while appendLogFile or any atomic
-- action is concurrently modifying the file. It does not lock the file,
-- for speed, but instead relies on the fact that a log file usually
-- ends in a newline.
checkLogFile :: FilePath -> RawFilePath -> (L.ByteString -> Bool) -> Annex Bool
checkLogFile f lck matchf = withExclusiveLock lck $ bracket setup cleanup go
  where
	setup = liftIO $ tryWhenExists $ openFile f ReadMode
	cleanup Nothing = noop
	cleanup (Just h) = liftIO $ hClose h
	go Nothing = return False
	go (Just h) = do
		!r <- liftIO (any matchf . fullLines <$> L.hGetContents h)
		return r

-- | Gets only the lines that end in a newline. If the last part of a file
-- does not, it's assumed to be a new line being logged that is incomplete,
-- and is omitted.
--
-- Unlike lines, this does not collapse repeated newlines etc.
fullLines :: L.ByteString -> [L.ByteString]
fullLines = go []
  where
	go c b = case L8.elemIndex '\n' b of
		Nothing -> reverse c
		Just n ->
			let (l, b') = L.splitAt n b
			in go (l:c) (L.drop 1 b')

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
streamLogFile :: FilePath -> RawFilePath -> Annex () -> (String -> Annex ()) -> Annex ()
streamLogFile f lck finalizer processor = 
	withExclusiveLock lck $ bracketOnError setup cleanup go
  where
	setup = liftIO $ tryWhenExists $ openFile f ReadMode 
	cleanup Nothing = noop
	cleanup (Just h) = liftIO $ hClose h
	go Nothing = finalizer
	go (Just h) = do
		mapM_ processor =<< liftIO (lines <$> hGetContents h)
		liftIO $ hClose h
		finalizer
		liftIO $ writeFile f ""
		setAnnexFilePerm (toRawFilePath f)

createDirWhenNeeded :: RawFilePath -> Annex () -> Annex ()
createDirWhenNeeded f a = a `catchNonAsync` \_e -> do
	-- Most of the time, the directory will exist, so this is only
	-- done if writing the file fails.
	createAnnexDirectory (parentDir f)
	a
