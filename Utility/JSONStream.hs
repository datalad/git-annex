{- Streaming JSON output.
 -
 - Copyright 2011, 2016 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE GADTs, OverloadedStrings #-}

module Utility.JSONStream (
	JSONChunk(..),
	start,
	add,
	addNestedObject,
	end
) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BU8
import Data.Char
import Data.Word

data JSONChunk v where
	AesonObject :: Object -> JSONChunk Object
	JSONChunk :: ToJSON v => [(String, v)] -> JSONChunk [(String, v)]

encodeJSONChunk :: JSONChunk v -> B.ByteString
encodeJSONChunk (AesonObject o) = encode o
encodeJSONChunk (JSONChunk l) = encode $ object $ map mkPair l
  where
	mkPair (s, v) = (T.pack s, toJSON v)

{- Aeson does not support building up a larger JSON object piece by piece
 - with streaming output. To support streaming, a hack:
 - The final "}" is left off the JSON, allowing more chunks to be added
 - to later. -}
start :: JSONChunk a -> B.ByteString
start a
	| not (B.null b) && B.last b == endchar = B.init b
	| otherwise = bad b
  where
	b = encodeJSONChunk a

add :: JSONChunk a -> B.ByteString
add a
	| not (B.null b) && B.head b == startchar = 
		B.cons addchar (B.drop 1 b)
	| otherwise = bad b
  where
	b = start a

addNestedObject :: String -> B.ByteString -> B.ByteString
addNestedObject s b = B.concat
	[ ",\""
	, BU8.fromString s
	, "\":"
	, b
	, "}"
	]

end :: B.ByteString
end = endchar `B.cons` sepchar `B.cons` B.empty

startchar :: Word8
startchar = fromIntegral (ord '{')

endchar :: Word8
endchar = fromIntegral (ord '}')

addchar :: Word8
addchar = fromIntegral (ord ',')

sepchar :: Word8
sepchar = fromIntegral (ord '\n')

bad :: B.ByteString -> a
bad b = error $ "JSON encoder generated unexpected value: " ++ show b

