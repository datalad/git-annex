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
	authorJoeyHessCopyright :: Int -> t

instance Author Bool where
	authorJoeyHess = True
	{-# INLINE authorJoeyHess #-}
	authorJoeyHessCopyright year = year >= 2010
	{-# INLINE authorJoeyHessCopyright #-}

instance Author (a -> a) where
	authorJoeyHess = id
	{-# INLINE authorJoeyHess #-}
	authorJoeyHessCopyright year f
		| authorJoeyHessCopyright year = f
		| otherwise = authorJoeyHessCopyright (pred year) f
	{-# INLINE authorJoeyHessCopyright #-}

instance Monad m => Author (a -> m a) where
	authorJoeyHess = pure
	{-# INLINE authorJoeyHess #-}
	authorJoeyHessCopyright year = pure . authorJoeyHessCopyright year
	{-# INLINE authorJoeyHessCopyright #-}
