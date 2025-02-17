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

import qualified GHC.IO.Encoding as Encoding
import System.IO
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
#ifdef mingw32_HOST_OS
import qualified Data.ByteString.UTF8 as S8
import qualified Data.ByteString.Lazy.UTF8 as L8
#else
import qualified GHC.Foreign as GHC
import System.IO.Unsafe
import Data.ByteString.Unsafe (unsafePackMallocCStringLen)
import Data.Char
import Data.List
#endif

-- | A literal file path
type RawFilePath = S.ByteString

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
-- with an identical implementation.
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
-- with an identical implementation.
{-# NOINLINE encodeBS #-}
encodeBS f = unsafePerformIO $ do
	enc <- Encoding.getFileSystemEncoding
	GHC.newCStringLen enc f >>= unsafePackMallocCStringLen
#else
encodeBS = S8.fromString
#endif

fromRawFilePath :: RawFilePath -> FilePath
fromRawFilePath = decodeBS

toRawFilePath :: FilePath -> RawFilePath
toRawFilePath = encodeBS

{- Truncates a FilePath to the given number of bytes (or less),
 - as represented on disk.
 -
 - Avoids returning an invalid part of a unicode byte sequence, at the
 - cost of efficiency when running on a large FilePath.
 -}
truncateFilePath :: Int -> RawFilePath -> RawFilePath
#ifndef mingw32_HOST_OS
{- On unix, do not assume a unicode locale, but does assume ascii
 - characters are a single byte. -}
truncateFilePath n b = 
	let blen = S.length b
	in if blen <= n
		then b
		else go blen (reverse (fromRawFilePath b))
  where
	go blen f = case uncons f of
		Just (c, f')
			| isAscii c ->
				let blen' = blen - 1
				in if blen' <= n
					then toRawFilePath (reverse f')
					else go blen' f'
			| otherwise ->
				let blen' = S.length (toRawFilePath f')
				in if blen' <= n 
					then toRawFilePath (reverse f')
					else go blen' f'
		Nothing -> toRawFilePath (reverse f)
#else
{- On Windows, count the number of bytes used by each utf8 character. -}
truncateFilePath n = toRawFilePath . reverse . go [] n
  where
	go coll cnt bs
		| cnt <= 0 = coll
		| otherwise = case S8.decode bs of
			Just (c, x)
				| c /= S8.replacement_char ->
					let x' = fromIntegral x
					in if cnt - x' < 0
						then coll
						else go (c:coll) (cnt - x') (S8.drop 1 bs)
				| otherwise ->
					go ('_':coll) (cnt - 1) (S8.drop 1 bs)
			_ -> coll
#endif
