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

import qualified Text.JSON as JSON
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.UTF8 as B

{- Only JSON objects can be used as chunks in the stream, not
 - other values.
 -
 - Both Aeson and Text.Json objects are supported. -}
data JSONChunk a where
	JSONObject :: JSON.JSON a => [(String, a)] -> JSONChunk [(String, a)]
	AesonObject :: Aeson.Object -> JSONChunk Aeson.Object

encodeJSONChunk :: JSONChunk a -> String
encodeJSONChunk (JSONObject l) = JSON.encodeStrict $ JSON.toJSObject l
encodeJSONChunk (AesonObject o) = B.toString (Aeson.encode o)

{- Text.JSON and Aeson do not support building up a larger JSON document
 - piece by piece as a stream. To support streaming, a hack. The final "}" 
 - is left off the object, allowing it to be added to later. -}
start :: JSONChunk a -> String
start a
	| last s == endchar = init s
	| otherwise = bad s
  where
	s = encodeJSONChunk a

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

