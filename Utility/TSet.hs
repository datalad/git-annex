{- Transactional sets
 -
 - Copyright 2012, 2013 Joey Hess <joey@kitenet.net>
 -}

module Utility.TSet where

import Common

import Control.Concurrent.STM

type TSet = TChan

newTSet :: STM (TSet a)
newTSet = newTChan

{- Gets the contents of the TSet. Blocks until at least one item is
 - present. -}
getTSet :: TSet a -> STM [a]
getTSet tset = do
	c <- readTChan tset
	l <- readTSet tset
	return $ c:l

{- Gets anything currently in the TSet, without blocking. -}
readTSet :: TSet a -> STM [a]
readTSet tset = go []
  where
	go l = do
		v <- tryReadTChan tset
		case v of
			Nothing -> return l
			Just c -> go (c:l)

{- Puts items into a TSet. -}
putTSet :: TSet a -> [a] -> STM ()
putTSet tset vs = mapM_ (writeTChan tset) vs

{- Put a single item into a TSet. -}
putTSet1 :: TSet a -> a -> STM ()
putTSet1 tset v = void $ writeTChan tset v
