{- GHC File system encoding handling.
 -
 - Copyright 2012-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.FileSystemEncoding (
	fileEncoding,
	withFilePath,
	md5FilePath,
	decodeW8,
	encodeW8,
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

{- Sets a Handle to use the filesystem encoding. This causes data
 - written or read from it to be encoded/decoded the same
 - as ghc 7.4 does to filenames etc. This special encoding
 - allows "arbitrary undecodable bytes to be round-tripped through it". -}
fileEncoding :: Handle -> IO ()
fileEncoding h = hSetEncoding h =<< Encoding.getFileSystemEncoding

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
 -}
{-# NOINLINE _encodeFilePath #-}
_encodeFilePath :: FilePath -> String
_encodeFilePath fp = unsafePerformIO $ do
	enc <- Encoding.getFileSystemEncoding
	GHC.withCString enc fp $ GHC.peekCString Encoding.char8

{- Encodes a FilePath into a Md5.Str, applying the filesystem encoding. -}
md5FilePath :: FilePath -> MD5.Str
md5FilePath = MD5.Str . _encodeFilePath

{- Converts a [Word8] to a FilePath, encoding using the filesystem encoding.
 -
 - w82c produces a String, which may contain Chars that are invalid
 - unicode. From there, this is really a simple matter of applying the
 - file system encoding, only complicated by GHC's interface to doing so.
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

{- Truncates a FilePath to the given number of bytes (or less),
 - as represented on disk.
 -
 - Avoids returning an invalid part of a unicode byte sequence, at the
 - cost of efficiency when running on a large FilePath.
 -}
truncateFilePath :: Int -> FilePath -> FilePath
truncateFilePath n = go . reverse
  where
  	go f =
		let bytes = decodeW8 f
		in if length bytes <= n
			then reverse f
			else go (drop 1 f)
