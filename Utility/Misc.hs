{- misc utility functions
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Misc where

import System.IO
import Control.Monad

{- A version of hgetContents that is not lazy. Ensures file is 
 - all read before it gets closed. -}
hGetContentsStrict :: Handle -> IO String
hGetContentsStrict = hGetContents >=> \s -> length s `seq` return s

{- A version of readFile that is not lazy. -}
readFileStrict :: FilePath -> IO String
readFileStrict = readFile >=> \s -> length s `seq` return s

{- Like break, but the character matching the condition is not included
 - in the second result list.
 -
 - separate (== ':') "foo:bar" = ("foo", "bar")
 - separate (== ':') "foobar" = ("foobar", "")
 -}
separate :: (a -> Bool) -> [a] -> ([a], [a])
separate c l = unbreak $ break c l
	where
		unbreak r@(a, b)
			| null b = r
			| otherwise = (a, tail b)

{- Breaks out the first line. -}
firstLine :: String -> String
firstLine = takeWhile (/= '\n')

{- Splits a list into segments that are delimited by items matching
 - a predicate. (The delimiters are not included in the segments.) -}
segment :: (a -> Bool) -> [a] -> [[a]]
segment p l = map reverse $ go [] [] l
	where
		go c r [] = reverse $ c:r
		go c r (i:is)
			| p i = go [] (c:r) is
			| otherwise = go (i:c) r is

{- Given two orderings, returns the second if the first is EQ and returns
 - the first otherwise. -}
thenOrd :: Ordering -> Ordering -> Ordering
thenOrd EQ x = x
thenOrd x _ = x
{-# INLINE thenOrd #-}
