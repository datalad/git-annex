{- Some git commands output quoted filenames, in a rather annoyingly complex
 - C-style encoding.
 -
 - Copyright 2010-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}

module Git.Filename (
	unquote,
	quote,
	QuotePath(..),
	StringContainingQuotedPath(..),
	prop_quote_unquote_roundtrip,
) where

import Common
import Utility.Format (decode_c, encode_c, encode_c', isUtf8Byte)
import Utility.QuickCheck

import Data.Char
import Data.Word
import Data.String
import qualified Data.ByteString as S
import qualified Data.Semigroup as Sem
import Prelude

unquote :: S.ByteString -> RawFilePath
unquote b = case S.uncons b of
	Nothing -> b
	Just (h, t)
		| h /= q -> b
		| otherwise -> case S.unsnoc t of
			Nothing -> b
			Just (i, l)
				| l /= q -> b
				| otherwise -> decode_c i
  where
  	q :: Word8
	q = fromIntegral (ord '"')

-- always encodes and double quotes, even in cases that git does not
quoteAlways :: RawFilePath -> S.ByteString
quoteAlways s = "\"" <> encode_c needencode s <> "\""
  where
	needencode c = isUtf8Byte c || c == fromIntegral (ord '"')

-- git config core.quotePath controls whether to quote unicode characters
newtype QuotePath = QuotePath Bool

class Quoteable t where
	-- double quotes and encodes when git would
	quote :: QuotePath -> t -> S.ByteString

instance Quoteable RawFilePath where
	quote (QuotePath qp) s = case encode_c' needencode s of
		Nothing -> s
		Just s' -> "\"" <> s' <> "\""
	  where
		needencode c
			| c == fromIntegral (ord '"') = True
			| qp = isUtf8Byte c
			| otherwise = False

-- Allows building up a string that contains paths, which will get quoted.
-- With OverloadedStrings, strings are passed through without quoting.
-- Eg: QuotedPath f <> ": not found"
data StringContainingQuotedPath
	= UnquotedString String 
	| QuotedPath RawFilePath
	| StringContainingQuotedPathMulti [StringContainingQuotedPath]
	deriving (Show, Eq)

instance Quoteable StringContainingQuotedPath where
	quote _ (UnquotedString s) = encodeBS s
	quote qp (QuotedPath p) = quote qp p
	quote qp (StringContainingQuotedPathMulti l) = S.concat (map (quote qp) l)

instance IsString StringContainingQuotedPath where
	fromString = UnquotedString

instance Sem.Semigroup StringContainingQuotedPath where
	UnquotedString a <> UnquotedString b = UnquotedString (a <> b)
	a <> b = StringContainingQuotedPathMulti [a, b]

instance Monoid StringContainingQuotedPath where
	mempty = UnquotedString mempty

-- Encoding and then decoding roundtrips only when the string does not
-- contain high unicode, because eg, both "\12345" and "\227\128\185"
-- are encoded to "\343\200\271".
--
-- That is not a real-world problem, and using TestableFilePath
-- limits what's tested to ascii, so avoids running into it.
prop_quote_unquote_roundtrip :: TestableFilePath -> Bool
prop_quote_unquote_roundtrip ts = 
	s == fromRawFilePath (unquote (quoteAlways (toRawFilePath s)))
  where
	s = fromTestableFilePath ts
