{- GHC File system encoding handling.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.FileSystemEncoding where

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

{- Encodes a FilePath into a Md5.Str, applying the filesystem encoding.
 -
 - This use of unsafePerformIO is belived to be safe; GHC's interface
 - only allows doing this conversion with CStrings, and the CString buffer
 - is allocated, used, and deallocated within the call, with no side
 - effects.
 -}
{-# NOINLINE encodeFilePath #-}
encodeFilePath :: FilePath -> MD5.Str
encodeFilePath fp = MD5.Str $ unsafePerformIO $ do
	enc <- Encoding.getFileSystemEncoding
	GHC.withCString enc fp $ GHC.peekCString Encoding.char8

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
