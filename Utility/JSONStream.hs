{- Streaming JSON output.
 -
 - Copyright 2011, 2016 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE GADTs #-}

module Utility.JSONStream (
	JSONChunk(..),
	start,
	add,
	end
) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as B

data JSONChunk v where
	JSONChunk :: ToJSON v => [(String, v)] -> JSONChunk [(String, v)]
	AesonObject :: Object -> JSONChunk Object

encodeJSONChunk :: JSONChunk v -> B.ByteString
encodeJSONChunk (JSONChunk l) = encode $ object $ map mkPair l
  where
	mkPair (s, v) = (T.pack s, toJSON v)
encodeJSONChunk (AesonObject o) = encode o

{- Aeson does not support building up a larger JSON object piece by piece
 - with streaming output. To support streaming, a hack:
 - The final "}" is left off the JSON, allowing more chunks to be added
 - to later. -}
start :: JSONChunk a -> String
start a
	| last s == endchar = init s
	| otherwise = bad s
  where
	s = B.toString $ encodeJSONChunk a

add :: JSONChunk a -> String
add a
	| head s == startchar = ',' : drop 1 s
	| otherwise = bad s
  where
	s = start a

end :: String
end = [endchar, '\n']

startchar :: Char
startchar = '{'

endchar :: Char
endchar = '}'

bad :: String -> a
bad s = error $ "JSON encoder generated unexpected value: " ++ s

