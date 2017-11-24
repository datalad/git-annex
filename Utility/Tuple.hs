{- tuple utility functions
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Tuple where

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c
