{- Simple Base64 access
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Base64 (toB64, fromB64) where

import Codec.Binary.Base64
import Data.Bits.Utils

toB64 :: String -> String		
toB64 = encode . s2w8

fromB64 :: String -> String
fromB64 s = maybe bad w82s $ decode s
	where bad = error "bad base64 encoded data"
