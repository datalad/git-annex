{- Simple Base64 encoding
 -
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE PackageImports #-}

module Utility.Base64 where

import Utility.FileSystemEncoding
import Utility.QuickCheck

import qualified "sandi" Codec.Binary.Base64 as B64
import Data.Maybe
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (fromString, toString)
import Data.Char

-- | This uses the FileSystemEncoding, so it can be used on Strings
-- that represent filepaths containing arbitrarily encoded characters.
toB64 :: String -> String
toB64 = toString . B64.encode . encodeBS

toB64' :: B.ByteString -> B.ByteString
toB64' = B64.encode

fromB64Maybe :: String -> Maybe String
fromB64Maybe s = either (const Nothing) (Just . decodeBS)
	(B64.decode $ fromString s)

fromB64Maybe' :: B.ByteString -> Maybe (B.ByteString)
fromB64Maybe' = either (const Nothing) Just . B64.decode

fromB64 :: String -> String
fromB64 = fromMaybe bad . fromB64Maybe
  where
	bad = error "bad base64 encoded data"

fromB64' :: B.ByteString -> B.ByteString
fromB64' = fromMaybe bad . fromB64Maybe'
  where
	bad = error "bad base64 encoded data"

-- Only ascii strings are tested, because an arbitrary string may contain
-- characters not encoded using the FileSystemEncoding, which would thus
-- not roundtrip, as decodeBS always generates an output encoded that way.
prop_b64_roundtrips :: TestableString -> Bool
prop_b64_roundtrips ts
	| all (isAscii) s = s == decodeBS (fromB64' (toB64' (encodeBS s)))
	| otherwise = True
  where
	s = fromTestableString ts
