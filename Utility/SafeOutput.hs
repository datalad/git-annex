{- Safe output to the terminal of possibly attacker-controlled strings,
 - avoiding displaying control characters.
 -
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.SafeOutput (
	safeOutput,
	safeOutputChar,
) where

import Data.Char
import qualified Data.ByteString as S

class SafeOutputtable t where
	safeOutput :: t -> t

instance SafeOutputtable String where
	safeOutput = filter safeOutputChar

instance SafeOutputtable S.ByteString where
	safeOutput = S.filter (safeOutputChar . chr . fromIntegral)

safeOutputChar :: Char -> Bool
safeOutputChar c
	| not (isControl c) = True
	| c == '\n' = True
	| c == '\t' = True
	| c == '\DEL' = False
	| ord c > 31 = True
	| otherwise = False
