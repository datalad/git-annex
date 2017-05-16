{- split utility functions
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Split where

import Data.List (intercalate)
import Data.List.Split (splitOn)

-- | same as Data.List.Utils.split
--
-- intercalate x . splitOn x === id
split :: Eq a => [a] -> [a] -> [[a]]
split = splitOn

-- | Split on a single character. This is over twice as fast as using
-- split on a list of length 1, while producing identical results. -}
splitc :: Eq c => c -> [c] -> [[c]]
splitc c s = case break (== c) s of
	(i, _c:rest) -> i : splitc c rest
	(i, []) -> i : []

-- | same as Data.List.Utils.replace
replace :: Eq a => [a] -> [a] -> [a] -> [a] 
replace old new = intercalate new . split old
