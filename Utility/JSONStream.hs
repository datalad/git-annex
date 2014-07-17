{- Streaming JSON output.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - License: BSD-2-clause
 -}

module Utility.JSONStream (
	start,
	add,
	end
) where

import Text.JSON

{- Text.JSON does not support building up a larger JSON document piece by
 - piece as a stream. To support streaming, a hack. The JSObject is converted
 - to a string with its final "}" is left off, allowing it to be added to
 - later. -}
start :: JSON a => [(String, a)] -> String
start l
	| last s == endchar = init s
	| otherwise = bad s
  where
	s = encodeStrict $ toJSObject l

add :: JSON a => [(String, a)] -> String
add l
	| head s == startchar = ',' : drop 1 s
	| otherwise = bad s
  where
	s = start l

end :: String
end = [endchar, '\n']

startchar :: Char
startchar = '{'

endchar :: Char
endchar = '}'

bad :: String -> a
bad s = error $ "Text.JSON returned unexpected string: " ++ s
