{- bloomfilter compatability wrapper
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Bloom (
	Bloom,
	safeSuggestSizing,
	Hashable(..),
	cheapHashes,
	elemB,
	notElemB,

	newMB,
	insertMB,
	unsafeFreezeMB,
) where

import qualified Data.BloomFilter.Mutable as MBloom
import qualified Data.BloomFilter as Bloom
import Data.BloomFilter.Easy (safeSuggestSizing, Bloom)
import Data.BloomFilter.Hash (Hashable(..), cheapHashes)
import Control.Monad.ST (ST)

notElemB :: a -> Bloom a -> Bool
notElemB = Bloom.notElem

elemB :: a -> Bloom a -> Bool
elemB = Bloom.elem

newMB :: (a -> [Bloom.Hash]) -> Int -> ST s (MBloom.MBloom s a)
newMB = MBloom.new

insertMB :: MBloom.MBloom s a -> a -> ST s ()
insertMB = MBloom.insert

unsafeFreezeMB :: MBloom.MBloom s a -> ST s (Bloom a)
unsafeFreezeMB = Bloom.unsafeFreeze
