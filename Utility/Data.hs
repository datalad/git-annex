{- utilities for simple data types
 -
 - Copyright 2013-2021 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Data (
	firstJust,
	eitherToMaybe,
	s2w8,
	w82s,
) where

import Data.Word

{- First item in the list that is not Nothing. -}
firstJust :: Eq a => [Maybe a] -> Maybe a
firstJust ms = case dropWhile (== Nothing) ms of
	[] -> Nothing
	(md:_) -> md

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

c2w8 :: Char -> Word8
c2w8 = fromIntegral . fromEnum

w82c :: Word8 -> Char
w82c = toEnum . fromIntegral

s2w8 :: String -> [Word8]
s2w8 = map c2w8

w82s :: [Word8] -> String
w82s = map w82c
