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
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.ByteString.Builder
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty

-- A ContentIdentifier can contain "", so to avoid ambiguity
-- in parsing, the list of them in the log must be non-empty.
type ContentIdentifierLog = Log (NonEmpty ContentIdentifier)

contentIdentifierList :: Maybe (NonEmpty ContentIdentifier) -> [ContentIdentifier]
contentIdentifierList (Just l) = Data.List.NonEmpty.toList l
contentIdentifierList Nothing = []

buildLog :: ContentIdentifierLog -> Builder
buildLog = buildLogNew buildContentIdentifierList

buildContentIdentifierList :: (NonEmpty ContentIdentifier) -> Builder
buildContentIdentifierList l = case l of
	c :| [] -> buildcid c
	(c :| cs) -> go (c:cs)
  where
	buildcid (ContentIdentifier c)
		| S8.any (`elem` [':', '\r', '\n']) c || "!" `S8.isPrefixOf` c =
			charUtf8 '!' <> byteString (toB64' c)
		| otherwise = byteString c
	go [] = mempty
	go (c:[]) = buildcid c
	go (c:cs) = buildcid c <> charUtf8 ':' <> go cs

parseLog :: L.ByteString -> ContentIdentifierLog
parseLog = parseLogNew parseContentIdentifierList

parseContentIdentifierList :: A.Parser (NonEmpty ContentIdentifier)
parseContentIdentifierList = do
	first <- cidparser
	listparser first []
  where
	cidparser = do
		b <- A8.takeWhile (/= ':')
		return $ if "!" `S8.isPrefixOf` b
			then ContentIdentifier $ fromMaybe b (fromB64Maybe' (S.drop 1 b))
			else ContentIdentifier b
	listparser first rest = ifM A8.atEnd
		( return (first :| reverse rest)
		, do
			_ <- A8.char ':'
			cid <- cidparser
			listparser first (cid:rest)
		)

prop_parse_build_contentidentifier_log :: NonEmpty ContentIdentifier -> Bool
prop_parse_build_contentidentifier_log l =
	let v = A.parseOnly parseContentIdentifierList $ L.toStrict $ 
		toLazyByteString $ buildContentIdentifierList l
	in v == Right l
