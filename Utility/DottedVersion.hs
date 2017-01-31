{- dotted versions, such as 1.0.1
 -
 - Copyright 2011-2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.DottedVersion where

import Common

data DottedVersion = DottedVersion String Integer
	deriving (Eq)

instance Ord DottedVersion where
	compare (DottedVersion _ x) (DottedVersion _ y) = compare x y

instance Show DottedVersion where
	show (DottedVersion s _) = s

{- To compare dotted versions like 1.7.7 and 1.8, they are normalized to
 - a somewhat arbitrary integer representation. -}
normalize :: String -> DottedVersion
normalize v = DottedVersion v $ 
	sum $ mult 1 $ reverse $ extend precision $ take precision $
		map readi $ splitc '.' v
  where
	extend n l = l ++ replicate (n - length l) 0
	mult _ [] = []
	mult n (x:xs) = (n*x) : mult (n*10^width) xs
	readi :: String -> Integer
	readi s = case reads s of
		((x,_):_) -> x
		_ -> 0
	precision = 10 -- number of segments of the version to compare
	width = length "yyyymmddhhmmss" -- maximum width of a segment
