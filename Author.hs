{- git-annex authorship made explicit in the code
 -
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Author where

class Author t where
	authorJoeyHess :: t
	authorJoeyHess' :: Int -> t

instance Author Bool where
	authorJoeyHess = True
	{-# INLINE authorJoeyHess #-}
	authorJoeyHess' year = year >= 2010
	{-# INLINE authorJoeyHess' #-}

instance Author (a -> a) where
	authorJoeyHess = id
	{-# INLINE authorJoeyHess #-}
	authorJoeyHess' year f
		| year >= 2010 = f
		| otherwise = authorJoeyHess' (pred year) f
	{-# INLINE authorJoeyHess' #-}

instance Monad m => Author (a -> m a) where
	authorJoeyHess = pure
	{-# INLINE authorJoeyHess #-}
	authorJoeyHess' year v = pure (authorJoeyHess' year v)
	{-# INLINE authorJoeyHess' #-}
