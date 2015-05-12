{- monadic stuff
 -
 - Copyright 2010-2012 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Monad where

import Data.Maybe
import Control.Monad

{- Return the first value from a list, if any, satisfying the given
 - predicate -}
firstM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
firstM _ [] = return Nothing
firstM p (x:xs) = ifM (p x) (return $ Just x , firstM p xs)

{- Runs the action on values from the list until it succeeds, returning
 - its result. -}
getM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
getM _ [] = return Nothing
getM p (x:xs) = maybe (getM p xs) (return . Just) =<< p x

{- Returns true if any value in the list satisfies the predicate,
 - stopping once one is found. -}
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p = liftM isJust . firstM p

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ [] = return True
allM p (x:xs) = p x <&&> allM p xs

{- Runs an action on values from a list until it succeeds. -}
untilTrue :: Monad m => [a] -> (a -> m Bool) -> m Bool
untilTrue = flip anyM

{- if with a monadic conditional. -}
ifM :: Monad m => m Bool -> (m a, m a) -> m a
ifM cond (thenclause, elseclause) = do
	c <- cond
	if c then thenclause else elseclause

{- short-circuiting monadic || -}
(<||>) :: Monad m => m Bool -> m Bool -> m Bool
ma <||> mb = ifM ma ( return True , mb )

{- short-circuiting monadic && -}
(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
ma <&&> mb = ifM ma ( mb , return False )

{- Same fixity as && and || -}
infixr 3 <&&>
infixr 2 <||>

{- Runs an action, passing its value to an observer before returning it. -}
observe :: Monad m => (a -> m b) -> m a -> m a
observe observer a = do
	r <- a
	_ <- observer r
	return r

{- b `after` a runs first a, then b, and returns the value of a -}
after :: Monad m => m b -> m a -> m a
after = observe . const

{- do nothing -}
noop :: Monad m => m ()
noop = return ()
