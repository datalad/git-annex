{- applicative stuff
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Applicative where

{- Like <$> , but supports one level of currying.
 - 
 - foo v = bar <$> action v  ==  foo = bar <$$> action
 -}
(<$$>) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
f <$$> v = fmap f . v
infixr 4 <$$>
