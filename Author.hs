{- authorship made explicit in the code
 -
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleInstances, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Author where

data Author = JoeyHess

-- This allows writing eg:
--
-- copyright = author JoeyHess 1999 :: Copyright
type Copyright = forall t. Authored t => t

class Authored t where
	author:: Author -> Int -> t

instance Authored Bool where
	author _ year = year >= 2010
	{-# INLINE author #-}

instance Authored (a -> a) where
	author by year f
		| author by year = f
		| otherwise = author by (pred year) f
	{-# INLINE author #-}

instance Monad m => Authored (a -> m a) where
	author by year = pure . author by year
	{-# INLINE author #-}
