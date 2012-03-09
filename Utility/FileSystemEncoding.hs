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

{- Encodes a FilePath into a String of encoded bytes, applying the
 - filesystem encoding.
 -
 - This use of unsafePerformIO is belived to be safe; GHC's interface
 - only allows doing this conversion with CStrings, and the CString buffer
 - is allocated, used, and deallocated within the call, with no side
 - effects.
 -}
{-# NOINLINE encodeFilePath #-}
encodeFilePath :: FilePath -> String
encodeFilePath fp = unsafePerformIO $ do
	enc <- Encoding.getFileSystemEncoding
	GHC.withCString enc fp $ GHC.peekCString Encoding.char8
