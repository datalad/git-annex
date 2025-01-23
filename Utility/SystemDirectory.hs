{- System.Directory wrapped to use OsPath.
 -
 - getFileSize is omitted, use Utility.FileSize instead
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.SystemDirectory (
	createDirectory,
	createDirectoryIfMissing,
	removeDirectory,
	removeDirectoryRecursive,
	removePathForcibly,
	renameDirectory,
	listDirectory,
	getDirectoryContents,
	getCurrentDirectory,
	setCurrentDirectory,
	withCurrentDirectory,
	getTemporaryDirectory,
	removeFile,
	renameFile,
	renamePath,
	copyFile,
	canonicalizePath,
	doesPathExist,
	doesFileExist,
	doesDirectoryExist,
	getModificationTime,
) where

#ifdef WITH_OSPATH
import System.Directory.OsPath
#else
import qualified System.Directory as X
import Data.Time.Clock (UTCTime)
import Utility.OsPath
import Utility.FileSystemEncoding

createDirectory :: OsPath -> IO ()
createDirectory = X.createDirectory . fromRawFilePath

createDirectoryIfMissing :: Bool -> OsPath -> IO ()
createDirectoryIfMissing b = X.createDirectoryIfMissing b . fromRawFilePath

removeDirectory :: OsPath -> IO ()
removeDirectory = X.removeDirectory . fromRawFilePath

removeDirectoryRecursive :: OsPath -> IO ()
removeDirectoryRecursive = X.removeDirectoryRecursive . fromRawFilePath

removePathForcibly :: OsPath -> IO ()
removePathForcibly = X.removePathForcibly . fromRawFilePath

renameDirectory :: OsPath -> OsPath -> IO ()
renameDirectory a b = X.renameDirectory (fromRawFilePath a) (fromRawFilePath b)

listDirectory :: OsPath -> IO [OsPath]
listDirectory p = map toRawFilePath <$> X.listDirectory (fromRawFilePath p)

getDirectoryContents :: OsPath -> IO [OsPath]
getDirectoryContents p = map toRawFilePath <$> X.getDirectoryContents (fromRawFilePath p)

getCurrentDirectory :: IO OsPath
getCurrentDirectory = toRawFilePath <$> X.getCurrentDirectory

setCurrentDirectory :: OsPath -> IO ()
setCurrentDirectory = X.setCurrentDirectory . fromRawFilePath

withCurrentDirectory :: OsPath -> IO a -> IO a
withCurrentDirectory = X.withCurrentDirectory . fromRawFilePath

getTemporaryDirectory :: IO OsPath
getTemporaryDirectory = toRawFilePath <$> X.getTemporaryDirectory

removeFile :: OsPath -> IO ()
removeFile = X.removeFile . fromRawFilePath

renameFile :: OsPath -> OsPath -> IO ()
renameFile a b = X.renameFile (fromRawFilePath a) (fromRawFilePath b)

renamePath :: OsPath -> OsPath -> IO ()
renamePath a b = X.renamePath (fromRawFilePath a) (fromRawFilePath b)

copyFile :: OsPath -> OsPath -> IO ()
copyFile a b = X.copyFile (fromRawFilePath a) (fromRawFilePath b)

canonicalizePath :: OsPath -> IO OsPath
canonicalizePath p = toRawFilePath <$> X.canonicalizePath (fromRawFilePath p)

doesPathExist :: OsPath -> IO Bool
doesPathExist = X.doesPathExist . fromRawFilePath

doesFileExist :: OsPath -> IO Bool
doesFileExist = X.doesFileExist . fromRawFilePath

doesDirectoryExist :: OsPath -> IO Bool
doesDirectoryExist = X.doesDirectoryExist . fromRawFilePath

getModificationTime :: OsPath -> IO UTCTime
getModificationTime = X.getModificationTime . fromRawFilePath
#endif
