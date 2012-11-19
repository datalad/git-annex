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
import qualified Data.ByteString as S

{- Sends the content of a file to an action, updating the meter as it's
 - consumed. -}
withMeteredFile :: FilePath -> MeterUpdate -> (L.ByteString -> IO a) -> IO a
withMeteredFile f meterupdate a = withBinaryFile f ReadMode $ \h ->
	hGetContentsObserved h (meterupdate . toInteger) >>= a

{- Sends the content of a file to a Handle, updating the meter as it's
 - written. -}
streamMeteredFile :: FilePath -> MeterUpdate -> Handle -> IO ()
streamMeteredFile f meterupdate h = withMeteredFile f meterupdate $ L.hPut h

{- Writes a ByteString to a Handle, updating a meter as it's written. -}
meteredWrite :: MeterUpdate -> Handle -> L.ByteString -> IO ()
meteredWrite meterupdate h = go . L.toChunks 
  where
	go [] = return ()
	go (c:cs) = do
		S.hPut h c
		meterupdate $ toInteger $ S.length c
		go cs

meteredWriteFile :: MeterUpdate -> FilePath -> L.ByteString -> IO ()
meteredWriteFile meterupdate f b = withBinaryFile f WriteMode $ \h ->
	meteredWrite meterupdate h b
