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

createDirectory :: OsPath -> IO ()
createDirectory = X.createDirectory . fromOsPath

createDirectoryIfMissing :: Bool -> OsPath -> IO ()
createDirectoryIfMissing b = X.createDirectoryIfMissing b . fromOsPath

removeDirectory :: OsPath -> IO ()
removeDirectory = X.removeDirectory . fromOsPath

removeDirectoryRecursive :: OsPath -> IO ()
removeDirectoryRecursive = X.removeDirectoryRecursive . fromOsPath

removePathForcibly :: OsPath -> IO ()
removePathForcibly = X.removePathForcibly . fromOsPath

renameDirectory :: OsPath -> OsPath -> IO ()
renameDirectory a b = X.renameDirectory (fromOsPath a) (fromOsPath b)

listDirectory :: OsPath -> IO [OsPath]
listDirectory p = map toOsPath <$> X.listDirectory (fromOsPath p)

getDirectoryContents :: OsPath -> IO [OsPath]
getDirectoryContents p = map toOsPath <$> X.getDirectoryContents (fromOsPath p)

getCurrentDirectory :: IO OsPath
getCurrentDirectory = toOsPath <$> X.getCurrentDirectory

setCurrentDirectory :: OsPath -> IO ()
setCurrentDirectory = X.setCurrentDirectory . fromOsPath

withCurrentDirectory :: OsPath -> IO a -> IO a
withCurrentDirectory = X.withCurrentDirectory . fromOsPath

getTemporaryDirectory :: IO OsPath
getTemporaryDirectory = toOsPath <$> X.getTemporaryDirectory

removeFile :: OsPath -> IO ()
removeFile = X.removeFile . fromOsPath

renameFile :: OsPath -> OsPath -> IO ()
renameFile a b = X.renameFile (fromOsPath a) (fromOsPath b)

renamePath :: OsPath -> OsPath -> IO ()
renamePath a b = X.renamePath (fromOsPath a) (fromOsPath b)

copyFile :: OsPath -> OsPath -> IO ()
copyFile a b = X.copyFile (fromOsPath a) (fromOsPath b)

canonicalizePath :: OsPath -> IO OsPath
canonicalizePath p = toOsPath <$> X.canonicalizePath (fromOsPath p)

doesPathExist :: OsPath -> IO Bool
doesPathExist = X.doesPathExist . fromOsPath

doesFileExist :: OsPath -> IO Bool
doesFileExist = X.doesFileExist . fromOsPath

doesDirectoryExist :: OsPath -> IO Bool
doesDirectoryExist = X.doesDirectoryExist . fromOsPath

getModificationTime :: OsPath -> IO UTCTime
getModificationTime = X.getModificationTime . fromOsPath
#endif
