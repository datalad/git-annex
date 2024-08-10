{- Balancing between UUIDs
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Balanced where

import Key
import Types.UUID
import Utility.Hash

import Data.List
import Data.Maybe
import Data.Bits (shiftL)
import qualified Data.Set as S
import qualified Data.ByteArray as BA

type BalancedPicker = S.Set UUID -> Key -> UUID

-- The set of UUIDs provided here are all the UUIDs that are ever
-- expected to be picked amoung. A subset of that can be provided
-- when later using the BalancedPicker.
balancedPicker :: S.Set UUID -> BalancedPicker
balancedPicker s = \s' key -> 
	let n = calcMac tointeger HmacSha256 combineduuids (serializeKey' key)
	in S.elemAt (fromIntegral (n `mod` m)) s'
  where
	combineduuids = mconcat (map fromUUID (S.toAscList s))
	m = fromIntegral (S.size s)

	tointeger :: Digest a -> Integer
	tointeger = foldl' (\i b -> (i `shiftL` 8) + fromIntegral b) 0 
		. BA.unpack

{- The selection for a given key never changes. -}
prop_balanced_stable :: Bool
prop_balanced_stable = balancedPicker us us k == toUUID "332"
  where
	us = S.fromList $ map (toUUID . show) [1..500 :: Int]
	k = fromJust $ deserializeKey "WORM--test"
