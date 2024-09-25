{- git-annex trust levels
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleInstances #-}

module Types.TrustLevel (
	TrustLevel(..),
	TrustMap,
	readTrustLevel,
	showTrustLevel,
	prop_read_show_TrustLevel
) where

import qualified Data.Map as M
import Data.Default
import Data.Ord

import Types.UUID

data TrustLevel = DeadTrusted | UnTrusted | SemiTrusted | Trusted
	deriving (Eq, Enum, Ord, Bounded, Show, Read)

instance Default TrustLevel  where
	def = SemiTrusted

instance Default (Down TrustLevel)  where
	def = Down def

type TrustMap = M.Map UUID TrustLevel

readTrustLevel :: String -> Maybe TrustLevel
readTrustLevel "trusted" = Just Trusted
readTrustLevel "untrusted" = Just UnTrusted
readTrustLevel "semitrusted" = Just SemiTrusted
readTrustLevel "dead" = Just DeadTrusted
readTrustLevel _ = Nothing

showTrustLevel :: TrustLevel -> String
showTrustLevel Trusted = "trusted"
showTrustLevel UnTrusted = "untrusted"
showTrustLevel SemiTrusted = "semitrusted"
showTrustLevel DeadTrusted = "dead"

prop_read_show_TrustLevel :: Bool
prop_read_show_TrustLevel = all check [minBound .. maxBound]
  where
	check l = readTrustLevel (showTrustLevel l) == Just l
