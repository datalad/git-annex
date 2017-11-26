{- git-annex bloom filter
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.BloomFilter where

import Annex.Common
import qualified Annex
import Utility.Bloom

import Control.Monad.ST

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
			-- precaulculated value for 500000 (1/10000000)
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
genBloomFilter :: Hashable v => ((v -> Annex ()) -> Annex ()) -> Annex (Bloom v)
genBloomFilter populate = do
	(numbits, numhashes) <- bloomBitsHashes
	bloom <- lift $ newMB (cheapHashes numhashes) numbits
	populate $ \v -> lift $ insertMB bloom v
	lift $ unsafeFreezeMB bloom
  where
	lift = liftIO . stToIO

bloomFilter :: [v] -> Bloom v -> [v]
bloomFilter l bloom = filter (\v -> v `notElemB` bloom) l
