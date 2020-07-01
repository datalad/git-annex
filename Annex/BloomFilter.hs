{- git-annex bloom filter
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Annex.BloomFilter where

import Annex.Common
import qualified Annex
import Utility.Bloom

import Control.Monad.ST
import Data.STRef
import Data.Hashable
import qualified Data.HashSet as S
import qualified Data.ByteString as B

{- A bloom filter capable of holding half a million keys with a
 - false positive rate of 1 in 10000000 uses around 16 mb of memory,
 - so will easily fit on even my lowest memory systems.
 -}
bloomCapacity :: Annex Int
bloomCapacity = fromMaybe 500000 . annexBloomCapacity <$> Annex.getGitConfig
bloomAccuracy :: Annex Int
bloomAccuracy = fromMaybe 10000000 . annexBloomAccuracy <$> Annex.getGitConfig
bloomBitsHashes :: Annex (Int, Int)
bloomBitsHashes = do
	capacity <- bloomCapacity
	accuracy <- bloomAccuracy
	case safeSuggestSizing capacity (1 / fromIntegral accuracy) of
		Left e -> do
			warning $ "bloomfilter " ++ e ++ "; falling back to sane value"
			-- precalculated value for 500000 (1/10000000)
			return (16777216,23)
		Right v -> return v

{- Creates a bloom filter, and runs an action to populate it.
 -
 - The action is passed a callback that it can use to feed values into the
 - bloom filter. 
 -
 - Once the action completes, the mutable filter is frozen
 - for later use.
 -}
genBloomFilter
	:: Utility.Bloom.Hashable v
	=> ((v -> Annex ()) -> Annex ())
	-> Annex (Bloom v)
genBloomFilter populate = do
	(numbits, numhashes) <- bloomBitsHashes
	bloom <- lift $ newMB (cheapHashes numhashes) numbits
	populate $ \v -> lift $ insertMB bloom v
	lift $ unsafeFreezeMB bloom
  where
	lift = liftIO . stToIO

bloomFilter :: [v] -> Bloom v -> [v]
bloomFilter l bloom = filter (\v -> v `notElemB` bloom) l

{- This starts off as a HashSet, and is only converted to a bloom filter
 - if it grows too large.
 - 
 - The advantage is, if it stays a HashSet, queries do not have false
 - positives.
 -}
data BloomableFilter v
	= BloomableFilter (S.HashSet (BloomBytes v))
	| BloomedFilter (Bloom (BloomBytes v))
	deriving (Show)

newtype BloomBytes v = BloomBytes B.ByteString
	deriving (Show, Eq, Utility.Bloom.Hashable, Data.Hashable.Hashable)

class BloomableBytes v where
	toBloomBytes :: v -> BloomBytes v

instance BloomableBytes B.ByteString where
	toBloomBytes = BloomBytes

instance BloomableBytes Key where
	toBloomBytes = BloomBytes . serializeKey'

{- The HashSet is grown until it uses around as much memory as the bloom
 - filter is configured to use. So peak memory use is 2x when the HashSet
 - is full and is being converted to the bloom filter.
 -}
genBloomableFilter
	:: BloomableBytes v
	=> ((v -> Annex ()) -> Annex ())
	-> Annex (BloomableFilter v)
genBloomableFilter populate = do
	(numbits, numhashes) <- bloomBitsHashes
	-- A HashSet is a tree, so there's some memory overhead beyond
	-- storing the values. Use 2/3 of the memory for storing
	-- the values, and reserve the rest for that overhead.
	let maxsz = (numbits `div` 8) `div` 3 * 2
	bv <- lift $ newMB (cheapHashes numhashes) numbits
	sv <- lift $ newSTRef S.empty
	szv <- lift $ newSTRef (Just 0)
	populate $ \v -> lift $ readSTRef szv >>= \case
		Just n 
			| n < maxsz -> do
				let bb@(BloomBytes b) = toBloomBytes v
				modifySTRef' sv (S.insert bb)
				modifySTRef' szv (fmap (+ B.length b))
			| otherwise -> do
				s <- readSTRef sv
				forM_ (S.toList s) $
					insertMB bv
				modifySTRef' sv (const S.empty)
				modifySTRef' szv (const Nothing)
				insertMB bv (toBloomBytes v)
		Nothing -> insertMB bv (toBloomBytes v)
	lift $ readSTRef szv >>= \case
		Just _ -> BloomableFilter <$> readSTRef sv
		Nothing -> BloomedFilter <$> unsafeFreezeMB bv
  where
	lift = liftIO . stToIO

data ElemWithFalsePositives
	= ElemNo
	| ElemYes
	| ElemProbablyYes
	deriving (Show)

elemB' :: (BloomableBytes v) => v -> BloomableFilter v -> ElemWithFalsePositives
elemB' v (BloomedFilter b) =
	case elemB (toBloomBytes v) b of
		True -> ElemProbablyYes
		False -> ElemNo
elemB' v (BloomableFilter s) =
	case S.member (toBloomBytes v) s of
		True -> ElemYes
		False -> ElemNo

