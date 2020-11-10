{- Some git commands output encoded filenames, in a rather annoyingly complex
 - C-style encoding.
 -
 - Copyright 2010, 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Git.Filename where

import Common
import Utility.Format (decode_c, encode_c)
import Utility.QuickCheck

import Data.Char
import Data.Word
import qualified Data.ByteString as S

-- encoded filenames will be inside double quotes
decode :: S.ByteString -> RawFilePath
decode b = case S.uncons b of
	Nothing -> b
	Just (h, t)
		| h /= q -> b
		| otherwise -> case S.unsnoc t of
			Nothing -> b
			Just (i, l)
				| l /= q -> b
				| otherwise ->
					encodeBS $ decode_c $ decodeBS i
  where
  	q :: Word8
	q = fromIntegral (ord '"')

{- Should not need to use this, except for testing decode. -}
encode :: RawFilePath -> S.ByteString
encode s = encodeBS $ "\"" ++ encode_c (decodeBS s) ++ "\""

-- Encoding and then decoding roundtrips only when the string does not
-- contain high unicode, because eg,  both "\12345" and "\227\128\185"
-- are encoded to "\343\200\271".
--
-- That is not a real-world problem, and using TestableFilePath
-- limits what's tested to ascii, so avoids running into it.
prop_encode_decode_roundtrip :: TestableFilePath -> Bool
prop_encode_decode_roundtrip ts = 
	s == fromRawFilePath (decode (encode (toRawFilePath s)))
  where
	s = fromTestableFilePath ts
