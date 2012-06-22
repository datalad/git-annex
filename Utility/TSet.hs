{- Transactional sets
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -}

module Utility.TSet where

import Common

import Control.Concurrent.STM

type TSet = TChan

runTSet :: STM a -> IO a
runTSet = atomically

newTSet :: IO (TSet a)
newTSet = atomically newTChan

{- Gets the contents of the TSet. Blocks until at least one item is
 - present. -}
getTSet :: TSet a -> IO [a]
getTSet tset = runTSet $ do
	c <- readTChan tset
	go [c]
	where
		go l = do
			v <- tryReadTChan tset
			case v of
				Nothing -> return l
				Just c -> go (c:l)

{- Puts items into a TSet. -}
putTSet :: TSet a -> [a] -> IO ()
putTSet tset vs = runTSet $ mapM_ (writeTChan tset) vs

{- Put a single item into a TSet. -}
putTSet1 :: TSet a -> a -> IO ()
putTSet1 tset v = void $ runTSet $ writeTChan tset v
