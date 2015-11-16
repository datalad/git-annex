{- Some git commands output encoded filenames, in a rather annoyingly complex
 - C-style encoding.
 -
 - Copyright 2010, 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Filename where

import Utility.Format (decode_c, encode_c)

import Common

decode :: String -> FilePath
decode [] = []
decode f@(c:s)
	-- encoded strings will be inside double quotes
	| c == '"' && end s == ['"'] = decode_c $ beginning s
	| otherwise = f

{- Should not need to use this, except for testing decode. -}
encode :: FilePath -> String
encode s = "\"" ++ encode_c s ++ "\""

{- for quickcheck -}
prop_isomorphic_deencode :: String -> Bool
prop_isomorphic_deencode s = s == decode (encode s)
