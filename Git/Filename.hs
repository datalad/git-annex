{- Some git commands output encoded filenames, in a rather annoyingly complex
 - C-style encoding.
 -
 - Copyright 2010, 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Filename where

import Common
import Utility.Format (decode_c, encode_c)

import Data.Char

decode :: String -> FilePath
decode [] = []
decode f@(c:s)
	-- encoded strings will be inside double quotes
	| c == '"' && end s == ['"'] = decode_c $ beginning s
	| otherwise = f

{- Should not need to use this, except for testing decode. -}
encode :: FilePath -> String
encode s = "\"" ++ encode_c s ++ "\""

{- For quickcheck. 
 -
 - See comment on Utility.Format.prop_encode_c_decode_c_roundtrip for
 - why this only tests chars < 256 -}
prop_encode_decode_roundtrip :: String -> Bool
prop_encode_decode_roundtrip s = s' == decode (encode s')
  where
	s' = filter (\c -> ord c < 256) s
