{- GHC File system encoding handling.
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.FileSystemEncoding (
	fileEncoding,
	withFilePath,
	md5FilePath,
	decodeBS,
	encodeBS,
	decodeW8,
	encodeW8,
	encodeW8NUL,
	decodeW8NUL,
	truncateFilePath,
) where

import qualified GHC.Foreign as GHC
import qualified GHC.IO.Encoding as Encoding
import Foreign.C
import System.IO
import System.IO.Unsafe
import qualified Data.Hash.MD5 as MD5
import Data.Word
import Data.Bits.Utils
import Data.List
import Data.List.Utils
import qualified Data.ByteString.Lazy as L
#ifdef mingw32_HOST_OS
import qualified Data.ByteString.Lazy.UTF8 as L8
#endif

import Utility.Exception

{- Sets a Handle to use the filesystem encoding. This causes data
 - written or read from it to be encoded/decoded the same
 - as ghc 7.4 does to filenames etc. This special encoding
 - allows "arbitrary undecodable bytes to be round-tripped through it".
 -}
fileEncoding :: Handle -> IO ()
#ifndef mingw32_HOST_OS
fileEncoding h = hSetEncoding h =<< Encoding.getFileSystemEncoding
#else
{- The file system encoding does not work well on Windows,
 - and Windows only has utf FilePaths anyway. -}
fileEncoding h = hSetEncoding h Encoding.utf8
#endif

{- Marshal a Haskell FilePath into a NUL terminated C string using temporary
 - storage. The FilePath is encoded using the filesystem encoding,
 - reversing the decoding that should have been done when the FilePath
 - was obtained. -}
withFilePath :: FilePath -> (CString -> IO a) -> IO a
withFilePath fp f = Encoding.getFileSystemEncoding
	>>= \enc -> GHC.withCString enc fp f

{- Encodes a FilePath into a String, applying the filesystem encoding.
 -
 - There are very few things it makes sense to do with such an encoded
 - string. It's not a legal filename; it should not be displayed.
 - So this function is not exported, but instead used by the few functions
 - that can usefully consume it.
 -
 - This use of unsafePerformIO is belived to be safe; GHC's interface
 - only allows doing this conversion with CStrings, and the CString buffer
 - is allocated, used, and deallocated within the call, with no side
 - effects.
 -
 - If the FilePath contains a value that is not legal in the filesystem
 - encoding, rather than thowing an exception, it will be returned as-is.
 -}
{-# NOINLINE _encodeFilePath #-}
_encodeFilePath :: FilePath -> String
_encodeFilePath fp = unsafePerformIO $ do
	enc <- Encoding.getFileSystemEncoding
	GHC.withCString enc fp (GHC.peekCString Encoding.char8)
		`catchNonAsync` (\_ -> return fp)

{- Encodes a FilePath into a Md5.Str, applying the filesystem encoding. -}
md5FilePath :: FilePath -> MD5.Str
md5FilePath = MD5.Str . _encodeFilePath

{- Decodes a ByteString into a FilePath, applying the filesystem encoding. -}
decodeBS :: L.ByteString -> FilePath
#ifndef mingw32_HOST_OS
decodeBS = encodeW8NUL . L.unpack
#else
{- On Windows, we assume that the ByteString is utf-8, since Windows
 - only uses unicode for filenames. -}
decodeBS = L8.toString
#endif

{- Encodes a FilePath into a ByteString, applying the filesystem encoding. -}
encodeBS :: FilePath -> L.ByteString
#ifndef mingw32_HOST_OS
encodeBS = L.pack . decodeW8NUL
#else
encodeBS = L8.fromString
#endif

{- Converts a [Word8] to a FilePath, encoding using the filesystem encoding.
 -
 - w82c produces a String, which may contain Chars that are invalid
 - unicode. From there, this is really a simple matter of applying the
 - file system encoding, only complicated by GHC's interface to doing so.
 -
 - Note that the encoding stops at any NUL in the input. FilePaths
 - do not normally contain embedded NUL, but Haskell Strings may.
 -}
{-# NOINLINE encodeW8 #-}
encodeW8 :: [Word8] -> FilePath
encodeW8 w8 = unsafePerformIO $ do
	enc <- Encoding.getFileSystemEncoding
	GHC.withCString Encoding.char8 (w82s w8) $ GHC.peekCString enc

{- Useful when you want the actual number of bytes that will be used to
 - represent the FilePath on disk. -}
decodeW8 :: FilePath -> [Word8]
decodeW8 = s2w8 . _encodeFilePath

{- Like encodeW8 and decodeW8, but NULs are passed through unchanged. -}
encodeW8NUL :: [Word8] -> FilePath
encodeW8NUL = intercalate nul . map encodeW8 . split (s2w8 nul)
  where
	nul = ['\NUL']

decodeW8NUL :: FilePath -> [Word8]
decodeW8NUL = intercalate (s2w8 nul) . map decodeW8 . split nul
  where
	nul = ['\NUL']

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
		let bytes = decodeW8 f
		in if length bytes <= n
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
