{- Transactional lists
 -
 - Based on DLists, a transactional list can quickly and efficiently
 - have items inserted at either end, or a whole list appended to it.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -}

{-# LANGUAGE BangPatterns #-}

module Utility.TList where

import Common

import Control.Concurrent.STM
import qualified Data.DList as D

type TList a = TMVar (D.DList a)

newTList :: STM (TList a)
newTList = newEmptyTMVar

{- Gets the contents of the TList. Blocks when empty.
 - TList is left empty. -}
getTList :: TList a -> STM [a]
getTList tlist = D.toList <$> getTDList tlist

getTDList :: TList a -> STM (D.DList a)
getTDList = takeTMVar

{- Replaces the contents of the TList. -}
setTList :: TList a -> [a] -> STM ()
setTList tlist = setTDList tlist . D.fromList

setTDList :: TList a -> D.DList a -> STM ()
setTDList tlist = modifyTList tlist . const

{- Takes anything currently in the TList, without blocking.
 - TList is left empty. -}
takeTList :: TList a -> STM [a]
takeTList tlist = maybe [] D.toList <$> tryTakeTMVar tlist

{- Reads anything in the list, without modifying it, or blocking. -}
readTList :: TList a -> STM [a]
readTList tlist = maybe [] D.toList <$> tryReadTMVar tlist

{- Mutates a TList. -}
modifyTList :: TList a -> (D.DList a -> D.DList a) -> STM ()
modifyTList tlist a = do
	dl <- fromMaybe D.empty <$> tryTakeTMVar tlist
	let !dl' = a dl
	{- The TMVar is left empty when the list is empty.
	 - Thus attempts to read it automatically block. -}
	unless (emptyDList dl') $
		putTMVar tlist dl'
  where
  	emptyDList = D.list True (\_ _ -> False)

consTList :: TList a -> a -> STM ()
consTList tlist v = modifyTList tlist $ \dl -> D.cons v dl

snocTList :: TList a -> a -> STM ()
snocTList tlist v = modifyTList tlist $ \dl -> D.snoc dl v

appendTList :: TList a -> [a] -> STM ()
appendTList tlist l = modifyTList tlist $ \dl -> D.append dl (D.fromList l)
