{- GHC File system encoding handling.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.FileSystemEncoding where

import Data.ByteString (useAsCString)
import Data.ByteString.Char8 (pack)
import Foreign.C
import System.IO

{- Sets a Handle to use the filesystem encoding. This causes data
 - written or read from it to be encoded/decoded the same
 - as ghc 7.4 does to filenames etc. This special encoding
 - allows "arbitrary undecodable bytes to be round-tripped through it".
 -
 - No-op for old ghc.
 -}
fileEncoding :: Handle -> IO ()
fileEncoding h = return () -- hSetEncoding h =<< Encoding.getFileSystemEncoding

{- Marshal a Haskell FilePath into a NUL terminated C string using temporary
- storage. The FilePath is encoded using the filesystem encoding,
- reversing the decoding that should have been done when the FilePath
- was obtained. -}
withFilePath :: FilePath -> (CString -> IO a) -> IO a
withFilePath fp f = useAsCString (pack fp) f

{- Encodes a FilePath into a String of encoded bytes, applying the
 - filesystem encoding.
 -
 - No-op for old ghc.
 -}
encodeFilePath :: FilePath -> String
encodeFilePath fp = fp
