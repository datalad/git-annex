{- git-annex log files
 -
 - Copyright 2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.File (writeLogFile, withLogHandle, appendLogFile, streamLogFile) where

import Annex.Common
import Annex.Perms
import Annex.LockFile
import Annex.ReplaceFile
import qualified Git
import Utility.Tmp

-- | Writes content to a file, replacing the file atomically, and
-- making the new file have whatever permissions the git repository is
-- configured to use. Creates the parent directory when necessary.
writeLogFile :: FilePath -> String -> Annex ()
writeLogFile f c = createDirWhenNeeded f $ viaTmp writelog f c
  where
	writelog f' c' = do
		liftIO $ writeFile f' c'
		setAnnexFilePerm f'

-- | Runs the action with a handle connected to a temp file.
-- The temp file replaces the log file once the action succeeds.
withLogHandle :: FilePath -> (Handle -> Annex a) -> Annex a
withLogHandle f a = do
	createAnnexDirectory (parentDir f)
	replaceGitAnnexDirFile f $ \tmp ->
		bracket (setup tmp) cleanup a
  where
	setup tmp = do
		setAnnexFilePerm tmp
		liftIO $ openFile tmp WriteMode
	cleanup h = liftIO $ hClose h

-- | Appends a line to a log file, first locking it to prevent
-- concurrent writers.
appendLogFile :: FilePath -> (Git.Repo -> FilePath) -> String -> Annex ()
appendLogFile f lck c = createDirWhenNeeded f $ withExclusiveLock lck $ do
	liftIO $ withFile f AppendMode $ \h -> hPutStrLn h c
	setAnnexFilePerm f

-- | Streams lines from a log file, and then empties the file at the end.
--
-- If the action is interrupted or throws an exception, the log file is
-- left unchanged.
--
-- Does nothing if the log file does not exist.
-- 
-- Locking is used to prevent writes to to the log file while this
-- is running.
streamLogFile :: FilePath -> (Git.Repo -> FilePath) -> (String -> Annex ()) -> Annex ()
streamLogFile f lck a = withExclusiveLock lck $ bracketOnError setup cleanup go
  where
	setup = liftIO $ tryWhenExists $ openFile f ReadMode 
	cleanup Nothing = noop
	cleanup (Just h) = liftIO $ hClose h
	go Nothing = noop
	go (Just h) = do
		mapM_ a =<< liftIO (lines <$> hGetContents h)
		liftIO $ hClose h
		liftIO $ writeFile f ""
		setAnnexFilePerm f

createDirWhenNeeded :: FilePath -> Annex () -> Annex ()
createDirWhenNeeded f a = a `catchNonAsync` \_e -> do
	-- Most of the time, the directory will exist, so this is only
	-- done if writing the file fails.
	createAnnexDirectory (parentDir f)
	a
