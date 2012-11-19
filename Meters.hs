{- git-annex meters
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Meters where

import Common
import Types.Meters
import Utility.Observed

import qualified Data.ByteString.Lazy as L

{- Sends the content of a file to an action, updating the meter as it's
 - consumed. -}
withMeteredFile :: FilePath -> MeterUpdate -> (L.ByteString -> IO a) -> IO a
withMeteredFile f meterupdate a = withBinaryFile f ReadMode $ \h ->
	hGetContentsObserved h (meterupdate . toInteger) >>= a

{- Sends the content of a file to a Handle, updating the meter as it's
 - written. -}
sendMeteredFile :: FilePath -> MeterUpdate -> Handle -> IO ()
sendMeteredFile f meterupdate h = withMeteredFile f meterupdate $ L.hPut h
