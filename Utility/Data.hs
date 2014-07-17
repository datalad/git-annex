{- utilities for simple data types
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - License: BSD-2-clause
 -}

module Utility.Data where

{- First item in the list that is not Nothing. -}
firstJust :: Eq a => [Maybe a] -> Maybe a
firstJust ms = case dropWhile (== Nothing) ms of
	[] -> Nothing
	(md:_) -> md

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just
