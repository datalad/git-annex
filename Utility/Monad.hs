{- monadic stuff
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Monad where

import Data.Maybe
import Control.Monad (liftM)

{- Return the first value from a list, if any, satisfying the given
 - predicate -}
firstM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
firstM _ [] = return Nothing
firstM p (x:xs) = do
	q <- p x
	if q
		then return (Just x)
		else firstM p xs

{- Returns true if any value in the list satisfies the predicate,
 - stopping once one is found. -}
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p = liftM isJust . firstM p

{- Runs an action on values from a list until it succeeds. -}
untilTrue :: Monad m => [a] -> (a -> m Bool) -> m Bool
untilTrue = flip anyM

{- Runs an action, passing its value to an observer before returning it. -}
observe :: Monad m => (a -> m b) -> m a -> m a
observe observer a = do
	r <- a
	_ <- observer r
	return r

{- b `after` a runs first a, then b, and returns the value of a -}
after :: Monad m => m b -> m a -> m a
after = observe . const
