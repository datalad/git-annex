{- general purpose utility functions
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility (
	hGetContentsStrict,
	readFileStrict,
	unsetFileMode,
	readMaybe,
	viaTmp,
	withTempFile,
	dirContains,
	dirContents,
	myHomeDir,
	catchBool,
	inPath
) where

import IO (bracket)
import System.IO
import System.Posix.Process hiding (executeFile)
import System.Posix.Files
import System.Posix.Types
import System.Posix.User
import System.FilePath
import System.Directory
import Foreign (complement)
import Utility.Path

{- A version of hgetContents that is not lazy. Ensures file is 
 - all read before it gets closed. -}
hGetContentsStrict :: Handle -> IO String
hGetContentsStrict h  = hGetContents h >>= \s -> length s `seq` return s

{- A version of readFile that is not lazy. -}
readFileStrict :: FilePath -> IO String
readFileStrict f = readFile f >>= \s -> length s `seq` return s

{- Removes a FileMode from a file.
 - For example, call with otherWriteMode to chmod o-w -}
unsetFileMode :: FilePath -> FileMode -> IO ()
unsetFileMode f m = do
	s <- getFileStatus f
	setFileMode f $ fileMode s `intersectFileModes` complement m

{- Attempts to read a value from a String. -}
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
	((x,_):_) -> Just x
	_ -> Nothing

{- Runs an action like writeFile, writing to a tmp file first and
 - then moving it into place. -}
viaTmp :: (FilePath -> String -> IO ()) -> FilePath -> String -> IO ()
viaTmp a file content = do
	pid <- getProcessID
        let tmpfile = file ++ ".tmp" ++ show pid
	createDirectoryIfMissing True (parentDir file)
	a tmpfile content
	renameFile tmpfile file

{- Runs an action with a temp file, then removes the file. -}
withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile template a = bracket create remove use
	where
		create = do
			tmpdir <- catch getTemporaryDirectory (const $ return ".")
			openTempFile tmpdir template
		remove (name, handle) = do
			hClose handle
			catchBool (removeFile name >> return True)
		use (name, handle) = a name handle

{- Lists the contents of a directory.
 - Unlike getDirectoryContents, paths are not relative to the directory. -}
dirContents :: FilePath -> IO [FilePath]
dirContents d = do
	c <- getDirectoryContents d
	return $ map (d </>) $ filter notcruft c
	where
		notcruft "." = False
		notcruft ".." = False
		notcruft _ = True

{- Current user's home directory. -}
myHomeDir :: IO FilePath
myHomeDir = do
	uid <- getEffectiveUserID
	u <- getUserEntryForID uid
	return $ homeDirectory u

{- Catches IO errors and returns a Bool -}
catchBool :: IO Bool -> IO Bool
catchBool = flip catch (const $ return False)

{- Checks if a command is available in PATH. -}
inPath :: String -> IO Bool
inPath command = search =<< getSearchPath
	where
		search [] = return False
		search (d:ds) = do
			e <- doesFileExist $ d </> command
			if e then return True else search ds
