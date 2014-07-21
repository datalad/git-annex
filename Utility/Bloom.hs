{- bloomfilter compatability wrapper
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.Bloom (
	Bloom,
	suggestSizing,
	Hashable,
	cheapHashes,
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
import Data.BloomFilter.Easy (suggestSizing, Bloom)
import Data.BloomFilter.Hash (Hashable, cheapHashes)
import Control.Monad.ST.Safe (ST)

#if MIN_VERSION_bloomfilter(2,0,0)

notElemB :: a -> Bloom a -> Bool
notElemB = Bloom.notElem

newMB :: (a -> [Bloom.Hash]) -> Int -> ST s (MBloom.MBloom s a)
newMB = MBloom.new

insertMB :: MBloom.MBloom s a -> a -> ST s ()
insertMB = MBloom.insert

unsafeFreezeMB :: MBloom.MBloom s a -> ST s (Bloom a)
unsafeFreezeMB = Bloom.unsafeFreeze

#else

notElemB :: a -> Bloom a -> Bool
notElemB = Bloom.notElemB

newMB :: (a -> [Bloom.Hash]) -> Int -> ST s (Bloom.MBloom s a)
newMB = Bloom.newMB

insertMB :: Bloom.MBloom s a -> a -> ST s ()
insertMB = Bloom.insertMB

unsafeFreezeMB :: Bloom.MBloom s a -> ST s (Bloom a)
unsafeFreezeMB = Bloom.unsafeFreezeMB

#endif
