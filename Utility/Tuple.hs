{- tuple utility functions
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

thd3 :: (a,b,c) -> c
thd3 (a,b,c) = c
