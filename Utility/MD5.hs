{- modified version of MD5 from http://www.cs.ox.ac.uk/people/ian.lynagh/md5
 -
 - Copyright (C) 2001 Ian Lynagh 
 - License: GPL 2
 -}

module Utility.MD5 where

import Data.Bits
import Data.Word
import Data.Char

display_32bits_as_dir :: Word32 -> [Word8]
display_32bits_as_dir w = trim $ swap_pairs cs
  where
	-- Need 32 characters to use. To avoid inaverdently making
	-- a real word, use letters that appear less frequently.
	chars = map (fromIntegral . ord) (['0'..'9'] ++ "zqjxkmvwgpfZQJXKMVWGPF")
	cs = map (\x -> getc $ (shiftR w (6*x)) .&. 31) [0..7]
	getc n = chars !! fromIntegral n
	swap_pairs (x1:x2:xs) = x2:x1:swap_pairs xs
	swap_pairs _ = []
	-- Last 2 will always be 00, so omit.
	trim = take 6
