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
 - allows "arbitrary undecodable bytes to be round-tripped through it".
 -
 - No-op for old ghc.
 -}
fileEncoding :: Handle -> IO ()
fileEncoding h = return () -- hSetEncoding h =<< Encoding.getFileSystemEncoding

{- Encodes a FilePath into a String of encoded bytes, applying the
 - filesystem encoding.
 -
 - No-op for old ghc.
 -}
encodeFilePath :: FilePath -> String
encodeFilePath fp = fp
