{- Portability shim around System.Posix.Files.ByteString
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.RawFilePath (
	RawFilePath,
	readSymbolicLink,
) where

#ifndef mingw32_HOST_OS
import System.Posix.Files.ByteString
import System.Posix.ByteString.FilePath
#else
import qualified Data.ByteString as B
import System.IO.Error

type RawFilePath = B.ByteString

readSymbolicLink :: RawFilePath -> IO RawFilePath
readSymbolicLink _ = ioError $ mkIOError illegalOperationErrorType x Nothing Nothing
  where
	x = "Utility.RawFilePath.readSymbolicLink: not supported"
#endif
