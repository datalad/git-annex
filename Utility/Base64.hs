{- Simple Base64 encoding of Strings
 -
 - Note that this uses the FileSystemEncoding, so it can be used on Strings
 - that repesent filepaths containing arbitrarily encoded characters.
 -
 - Copyright 2011, 2015 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Base64 (toB64, fromB64Maybe, fromB64, prop_b64_roundtrips) where

import Utility.FileSystemEncoding

import qualified "sandi" Codec.Binary.Base64 as B64
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import Data.ByteString.UTF8 (fromString, toString)
import Data.Char

toB64 :: String -> String	
toB64 = toString . B64.encode . L.toStrict . encodeBS

fromB64Maybe :: String -> Maybe String
fromB64Maybe s = either (const Nothing) (Just . decodeBS . L.fromStrict)
	(B64.decode $ fromString s)

fromB64 :: String -> String
fromB64 = fromMaybe bad . fromB64Maybe
  where
	bad = error "bad base64 encoded data"

-- Only ascii strings are tested, because an arbitrary string may contain
-- characters not encoded using the FileSystemEncoding, which would thus
-- not roundtrip, as fromB64 always generates an output encoded that way.
prop_b64_roundtrips :: String -> Bool
prop_b64_roundtrips s
	| all (isAscii) s = s == fromB64 (toB64 s)
	| otherwise = True
