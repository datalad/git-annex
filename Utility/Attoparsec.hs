{- attoparsec utility functions
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 - Copyright 2007-2015 Bryan O'Sullivan
 -
 - License: BSD-3-clause
 -}

module Utility.Attoparsec where

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B

-- | Parse and decode an unsigned octal number.
--
-- This parser does not accept a leading @\"0o\"@ string.
octal :: Integral a => A.Parser a
octal = B.foldl' step 0 `fmap` A.takeWhile1 isOctDigit
  where
	isOctDigit w = w >= 48 && w <= 55
	step a w = a * 8 + fromIntegral (w - 48)
