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

import Data.Maybe
import Data.List
import Data.Bits (shiftL)
import qualified Data.Set as S
import qualified Data.ByteArray as BA

-- The Int is how many UUIDs to pick.
type BalancedPicker = S.Set UUID -> Key -> Int -> [UUID]

-- The set of UUIDs provided here are all the UUIDs that are ever
-- expected to be picked amoung. A subset of that can be provided
-- when later using the BalancedPicker. Neither set can be empty.
balancedPicker :: S.Set UUID -> BalancedPicker
balancedPicker s = \s' key num ->
	let n = calcMac tointeger HmacSha256 combineduuids (serializeKey' key)
	    m = fromIntegral (S.size s')
	in map (\i -> S.elemAt (fromIntegral ((n + i) `mod` m)) s') 
		[0..fromIntegral (num - 1)]
  where
	combineduuids = mconcat (map fromUUID (S.toAscList s))

	tointeger :: Digest a -> Integer
	tointeger = foldl' (\i b -> (i `shiftL` 8) + fromIntegral b) 0 
		. BA.unpack

{- The selection for a given key never changes. -}
prop_balanced_stable :: Bool
prop_balanced_stable = and
	[ balancedPicker us us k 1 == [toUUID "332"]
	, balancedPicker us us k 3 == [toUUID "332", toUUID "333", toUUID "334"]
	]
  where
	us = S.fromList $ map (toUUID . show) [1..500 :: Int]
	k = fromJust $ deserializeKey "WORM--test"
