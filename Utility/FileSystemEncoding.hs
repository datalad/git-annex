{- GHC File system encoding handling.
 -
 - Copyright 2012-2021 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.FileSystemEncoding (
	useFileSystemEncoding,
	fileEncoding,
	RawFilePath,
	fromRawFilePath,
	toRawFilePath,
	decodeBL,
	encodeBL,
	decodeBS,
	encodeBS,
	truncateFilePath,
) where

import qualified GHC.Foreign as GHC
import qualified GHC.IO.Encoding as Encoding
import System.IO
import System.IO.Unsafe
import System.FilePath.ByteString (RawFilePath, encodeFilePath, decodeFilePath)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Unsafe (unsafePackMallocCStringLen)
#ifdef mingw32_HOST_OS
import qualified Data.ByteString.UTF8 as S8
import qualified Data.ByteString.Lazy.UTF8 as L8
#endif

{- Makes all subsequent Handles that are opened, as well as stdio Handles,
 - use the filesystem encoding, instead of the encoding of the current
 - locale.
 -
 - The filesystem encoding allows "arbitrary undecodable bytes to be
 - round-tripped through it". This avoids encoded failures when data is not
 - encoded matching the current locale.
 -
 - Note that code can still use hSetEncoding to change the encoding of a
 - Handle. This only affects the default encoding.
 -}
useFileSystemEncoding :: IO ()
useFileSystemEncoding = do
#ifndef mingw32_HOST_OS
	e <- Encoding.getFileSystemEncoding
#else
	{- The file system encoding does not work well on Windows,
	 - and Windows only has utf FilePaths anyway. -}
	let e = Encoding.utf8
#endif
	hSetEncoding stdin e
	hSetEncoding stdout e
	hSetEncoding stderr e
	Encoding.setLocaleEncoding e	

fileEncoding :: Handle -> IO ()
#ifndef mingw32_HOST_OS
fileEncoding h = hSetEncoding h =<< Encoding.getFileSystemEncoding
#else
fileEncoding h = hSetEncoding h Encoding.utf8
#endif

{- Decodes a ByteString into a FilePath, applying the filesystem encoding. -}
decodeBL :: L.ByteString -> FilePath
#ifndef mingw32_HOST_OS
decodeBL = decodeBS . L.toStrict
#else
{- On Windows, we assume that the ByteString is utf-8, since Windows
 - only uses unicode for filenames. -}
decodeBL = L8.toString
#endif

{- Encodes a FilePath into a ByteString, applying the filesystem encoding. -}
encodeBL :: FilePath -> L.ByteString
#ifndef mingw32_HOST_OS
encodeBL = L.fromStrict . encodeBS
#else
encodeBL = L8.fromString
#endif

decodeBS :: S.ByteString -> FilePath
#ifndef mingw32_HOST_OS
-- This does the same thing as System.FilePath.ByteString.decodeFilePath,
-- with an identical implementation. However, older versions of that library
-- truncated at NUL, which this must not do, because it may end up used on
-- something other than a unix filepath.
{-# NOINLINE decodeBS #-}
decodeBS b = unsafePerformIO $ do
	enc <- Encoding.getFileSystemEncoding
	S.useAsCStringLen b (GHC.peekCStringLen enc)
#else
decodeBS = S8.toString
#endif

encodeBS :: FilePath -> S.ByteString
#ifndef mingw32_HOST_OS
-- This does the same thing as System.FilePath.ByteString.encodeFilePath,
-- with an identical implementation. However, older versions of that library
-- truncated at NUL, which this must not do, because it may end up used on
-- something other than a unix filepath.
{-# NOINLINE encodeBS #-}
encodeBS f = unsafePerformIO $ do
	enc <- Encoding.getFileSystemEncoding
	GHC.newCStringLen enc f >>= unsafePackMallocCStringLen
#else
encodeBS = S8.fromString
#endif

fromRawFilePath :: RawFilePath -> FilePath
fromRawFilePath = decodeFilePath

toRawFilePath :: FilePath -> RawFilePath
toRawFilePath = encodeFilePath

{- Truncates a FilePath to the given number of bytes (or less),
 - as represented on disk.
 -
 - Avoids returning an invalid part of a unicode byte sequence, at the
 - cost of efficiency when running on a large FilePath.
 -}
truncateFilePath :: Int -> FilePath -> FilePath
#ifndef mingw32_HOST_OS
truncateFilePath n = go . reverse
  where
	go f =
		let b = encodeBS f
		in if S.length b <= n
			then reverse f
			else go (drop 1 f)
#else
{- On Windows, count the number of bytes used by each utf8 character. -}
truncateFilePath n = reverse . go [] n . L8.fromString
  where
	go coll cnt bs
		| cnt <= 0 = coll
		| otherwise = case L8.decode bs of
			Just (c, x) | c /= L8.replacement_char ->
				let x' = fromIntegral x
				in if cnt - x' < 0
					then coll
					else go (c:coll) (cnt - x') (L8.drop 1 bs)
			_ -> coll
#endif
