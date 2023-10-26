{- Simple Base64 encoding
 -
 - Copyright 2011-2023 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Base64 where

import Utility.Exception

import Codec.Binary.Base64 as B64
import Data.Maybe
import qualified Data.ByteString as B

toB64 :: B.ByteString -> B.ByteString
toB64 = B64.encode

fromB64Maybe :: B.ByteString -> Maybe (B.ByteString)
fromB64Maybe = either (const Nothing) Just . B64.decode

fromB64 :: B.ByteString -> B.ByteString
fromB64 = fromMaybe bad . fromB64Maybe
  where
	bad = giveup "bad base64 encoded data"
