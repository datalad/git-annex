{- Remote content identifier logs, pure operations.
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Logs.ContentIdentifier.Pure where

import Annex.Common
import Logs.UUIDBased
import Types.Import
import Utility.Base64

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.ByteString.Builder

type ContentIdentifierLog = Log [ContentIdentifier]

buildLog :: ContentIdentifierLog -> Builder
buildLog = buildLogNew buildContentIdentifierList

buildContentIdentifierList :: [ContentIdentifier] -> Builder
buildContentIdentifierList l = case l of
	[] -> mempty
	[c] -> buildcid c
	(c:cs) -> buildcid c <> charUtf8 ' ' <> buildContentIdentifierList cs
  where
	buildcid (ContentIdentifier c)
		| S8.any (`elem` [' ', '\r', '\n']) c || "!" `S8.isPrefixOf` c =
			charUtf8 '!' <> byteString (toB64' c)
		| otherwise = byteString c

parseLog :: L.ByteString -> ContentIdentifierLog
parseLog = parseLogNew parseContentIdentifierList

parseContentIdentifierList :: A.Parser [ContentIdentifier]
parseContentIdentifierList = reverse . catMaybes <$> valueparser []
  where
	valueparser l = do
		b <- A8.takeWhile1 (/= ' ')
		let cid = if "!" `S8.isPrefixOf` b
			then ContentIdentifier <$> fromB64Maybe' (S.drop 1 b)
			else Just $ ContentIdentifier b
		ifM A8.atEnd
			( return (cid:l)
			, do
				_ <- A8.char ' '
				valueparser (cid:l)
			)

prop_parse_build_contentidentifier_log :: ContentIdentifierLog -> Bool
prop_parse_build_contentidentifier_log l =
	parseLog (toLazyByteString (buildLog l)) == l
