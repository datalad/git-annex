{- Simple Base64 encoding of Strings
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Base64 (toB64, fromB64Maybe, fromB64, prop_b64_roundtrips) where

import qualified "dataenc" Codec.Binary.Base64 as B64
import Control.Applicative
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.UTF8 (fromString, toString)

toB64 :: String -> String	
toB64 = B64.encode . L.unpack . fromString

fromB64Maybe :: String -> Maybe String
fromB64Maybe s = toString . L.pack <$> B64.decode s

fromB64 :: String -> String
fromB64 = fromMaybe bad . fromB64Maybe
  where
	bad = error "bad base64 encoded data"

prop_b64_roundtrips :: String -> Bool
prop_b64_roundtrips s = s == fromB64 (toB64 s)
