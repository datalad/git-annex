{- streaming directory reading
 -
 - Copyright 2011-2025 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Directory.Stream (
	DirectoryHandle,
	openDirectory,
	closeDirectory,
	readDirectory,
	isDirectoryPopulated,
) where

import Control.Monad
import Control.Concurrent
import qualified Data.ByteString as B
import Data.Maybe
import Prelude

#ifdef mingw32_HOST_OS
import qualified System.Win32 as Win32
import System.FilePath
#else
import qualified System.Posix.Directory.ByteString as Posix
#endif

import Utility.Directory
import Utility.Exception
import Utility.FileSystemEncoding

#ifndef mingw32_HOST_OS
data DirectoryHandle = DirectoryHandle IsOpen Posix.DirStream
#else
data DirectoryHandle = DirectoryHandle IsOpen Win32.HANDLE Win32.FindData (MVar ())
#endif

type IsOpen = MVar () -- full when the handle is open

openDirectory :: RawFilePath -> IO DirectoryHandle
openDirectory path = do
#ifndef mingw32_HOST_OS
	dirp <- Posix.openDirStream path
	isopen <- newMVar ()
	return (DirectoryHandle isopen dirp)
#else
	(h, fdat) <- Win32.findFirstFile (fromRawFilePath path </> "*")
	-- Indicate that the fdat contains a filename that readDirectory
	-- has not yet returned, by making the MVar be full.
	-- (There's always at least a "." entry.)
	alreadyhave <- newMVar ()
	isopen <- newMVar ()
	return (DirectoryHandle isopen h fdat alreadyhave)
#endif

closeDirectory :: DirectoryHandle -> IO ()
#ifndef mingw32_HOST_OS
closeDirectory (DirectoryHandle isopen dirp) =
	whenOpen isopen $
		Posix.closeDirStream dirp
#else
closeDirectory (DirectoryHandle isopen h _ alreadyhave) =
	whenOpen isopen $ do
		_ <- tryTakeMVar alreadyhave
		Win32.findClose h
#endif
  where
	whenOpen :: IsOpen -> IO () -> IO ()
	whenOpen mv f = do
		v <- tryTakeMVar mv
		when (isJust v) f

-- | Reads the next entry from the handle. Once the end of the directory
-- is reached, returns Nothing and automatically closes the handle.
readDirectory :: DirectoryHandle -> IO (Maybe RawFilePath)
#ifndef mingw32_HOST_OS
readDirectory hdl@(DirectoryHandle _ dirp) = do
	e <- Posix.readDirStream dirp
	if B.null e
		then do
			closeDirectory hdl
			return Nothing
		else return (Just e)
#else
readDirectory hdl@(DirectoryHandle _ h fdat mv) = do
	-- If the MVar is full, then the filename in fdat has
	-- not yet been returned. Otherwise, need to find the next
	-- file.
	r <- tryTakeMVar mv
	case r of
		Just () -> getfn
		Nothing -> do
			more <- Win32.findNextFile h fdat
			if more
				then getfn
				else do
					closeDirectory hdl
					return Nothing
  where
	getfn = do
		filename <- Win32.getFindDataFileName fdat
		return (Just (toRawFilePath filename))
#endif

-- | True only when directory exists and is not empty.
isDirectoryPopulated :: RawFilePath -> IO Bool
isDirectoryPopulated d = bracket (openDirectory d) closeDirectory check
	`catchIO` const (return False)
  where
	check h = do
		v <- readDirectory h
		case v of
			Nothing -> return False
			Just f
				| not (dirCruft f) -> return True
				| otherwise -> check h
