{- types for limits
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Types.Limit where

import Common.Annex
import Types.FileMatcher

import qualified Data.Set as S

type MkLimit = String -> Either String MatchFiles

type AssumeNotPresent = S.Set UUID
type MatchFiles = AssumeNotPresent -> MatchInfo -> Annex Bool
