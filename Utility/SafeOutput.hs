{- Safe output to the terminal of possibly attacker-controlled strings,
 - avoiding displaying control characters.
 -
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.SafeOutput (safeOutput) where

import Data.Char
import qualified Data.ByteString as S

class SafeOutputtable t where
	safeOutput :: t -> t

instance SafeOutputtable String where
	safeOutput = filter (not . isControl)

instance SafeOutputtable S.ByteString where
	safeOutput = S.filter (not . isControl . chr . fromIntegral)
