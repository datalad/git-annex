{- bloomfilter compatability wrapper
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

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

#if MIN_VERSION_bloomfilter(2,0,0)
import qualified Data.BloomFilter.Mutable as MBloom
import qualified Data.BloomFilter as Bloom
#else
import qualified Data.BloomFilter as Bloom
#endif
import Data.BloomFilter.Easy (safeSuggestSizing, Bloom)
import Data.BloomFilter.Hash (Hashable(..), cheapHashes)
import Control.Monad.ST (ST)

#if MIN_VERSION_bloomfilter(2,0,0)

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

#else

notElemB :: a -> Bloom a -> Bool
notElemB = Bloom.notElemB

elemB :: a -> Bloom a -> Bool
elemB = Bloom.elemB

newMB :: (a -> [Bloom.Hash]) -> Int -> ST s (Bloom.MBloom s a)
newMB = Bloom.newMB

insertMB :: Bloom.MBloom s a -> a -> ST s ()
insertMB = Bloom.insertMB

unsafeFreezeMB :: Bloom.MBloom s a -> ST s (Bloom a)
unsafeFreezeMB = Bloom.unsafeFreezeMB

#endif
