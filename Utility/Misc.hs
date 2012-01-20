{- misc utility functions
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Misc where

import System.IO
import System.IO.Error (try)
import Control.Monad
import Control.Applicative

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
firstLine :: String-> String
firstLine = takeWhile (/= '\n')

{- Catches IO errors and returns a Bool -}
catchBoolIO :: IO Bool -> IO Bool
catchBoolIO a = catchDefaultIO a False

{- Catches IO errors and returns a Maybe -}
catchMaybeIO :: IO a -> IO (Maybe a)
catchMaybeIO a = catchDefaultIO (Just <$> a) Nothing

{- Catches IO errors and returns a default value. -}
catchDefaultIO :: IO a -> a -> IO a
catchDefaultIO a def = catch a (const $ return def)

{- Catches IO errors and returns the error message. -}
catchMsgIO :: IO a -> IO (Either String a)
catchMsgIO a = dispatch <$> try a
	where
		dispatch (Left e) = Left $ show e
		dispatch (Right v) = Right v
