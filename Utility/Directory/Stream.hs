{- streaming directory traversal
 -
 - Copyright 2011-2018 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Directory.Stream where

import Control.Monad
import System.FilePath
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Concurrent
import Data.Maybe
import Prelude

#ifdef mingw32_HOST_OS
import qualified System.Win32 as Win32
#else
import qualified System.Posix as Posix
#endif

import Utility.Directory
import Utility.Exception

#ifndef mingw32_HOST_OS
data DirectoryHandle = DirectoryHandle IsOpen Posix.DirStream
#else
data DirectoryHandle = DirectoryHandle IsOpen Win32.HANDLE Win32.FindData (MVar ())
#endif

type IsOpen = MVar () -- full when the handle is open

openDirectory :: FilePath -> IO DirectoryHandle
openDirectory path = do
#ifndef mingw32_HOST_OS
	dirp <- Posix.openDirStream path
	isopen <- newMVar ()
	return (DirectoryHandle isopen dirp)
#else
	(h, fdat) <- Win32.findFirstFile (path </> "*")
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
readDirectory :: DirectoryHandle -> IO (Maybe FilePath)
#ifndef mingw32_HOST_OS
readDirectory hdl@(DirectoryHandle _ dirp) = do
	e <- Posix.readDirStream dirp
	if null e
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
		return (Just filename)
#endif

-- | Like getDirectoryContents, but rather than buffering the whole
-- directory content in memory, lazily streams.
--
-- This is like lazy readFile in that the handle to the directory remains
-- open until the whole list is consumed, or until the list is garbage
-- collected. So use with caution particularly when traversing directory
-- trees.
streamDirectoryContents :: FilePath -> IO [FilePath]
streamDirectoryContents d = openDirectory d >>= collect
  where
	collect hdl = readDirectory hdl >>= \case
		Nothing -> return []
		Just f -> do
			rest <- unsafeInterleaveIO (collect hdl)
			return (f:rest)

-- | True only when directory exists and contains nothing.
-- Throws exception if directory does not exist.
isDirectoryEmpty :: FilePath -> IO Bool
isDirectoryEmpty d = bracket (openDirectory d) closeDirectory check
  where
	check h = do
		v <- readDirectory h
		case v of
			Nothing -> return True
			Just f
				| not (dirCruft f) -> return False
				| otherwise -> check h
