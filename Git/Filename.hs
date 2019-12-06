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

prop_encode_decode_roundtrip :: FilePath -> Bool
prop_encode_decode_roundtrip s = s' ==
	fromRawFilePath (decode (encode (toRawFilePath s')))
  where
	s' = nonul (nohigh s)
	-- Encoding and then decoding roundtrips only when
	-- the string does not contain high unicode, because eg, 
	-- both "\12345" and "\227\128\185" are encoded to
	-- "\343\200\271".
	--
	-- This property papers over the problem, by only
	-- testing chars < 256.
	nohigh = filter (\c -> ord c < 256)
	-- A String can contain a NUL, but toRawFilePath
	-- truncates on the NUL, which is generally fine
	-- because unix filenames cannot contain NUL.
	-- So the encoding only roundtrips when there is no nul.
	nonul = filter (/= '\NUL')
