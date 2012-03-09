{- GHC File system encoding handling.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.FileSystemEncoding where

import System.IO
import Foreign.C
import GHC.Foreign as GHC
import GHC.IO.Encoding

{- Sets a Handle to use the filesystem encoding. This causes data
 - written or read from it to be encoded/decoded the same
 - as ghc 7.4 does to filenames etc. This special encoding
 - allows "arbitrary undecodable bytes to be round-tripped through it". -}
fileEncoding :: Handle -> IO ()
fileEncoding h = hSetEncoding h =<< getFileSystemEncoding

{- Marshal a Haskell FilePath into a NUL terminated C string using temporary
 - storage. The FilePath is encoded using the filesystem encoding,
 - reversing the decoding that should have been done when the FilePath
 - was obtained. -}
withFilePath :: FilePath -> (CString -> IO a) -> IO a
withFilePath fp f = getFileSystemEncoding >>= \enc -> GHC.withCString enc fp f

{- Encodes a FilePath into a String of encoded bytes, applying the
 - filesystem encoding.
 -
 - This does not do any IO, beyond allocating a C buffer. GHC does not
 - seem to provide a pure way to do this conversion. -}
encodeFilePath :: FilePath -> IO String
encodeFilePath fp = do
	enc <- getFileSystemEncoding
	GHC.withCString enc fp $ GHC.peekCString enc
