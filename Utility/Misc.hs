{- misc utility functions
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Misc where

import System.IO

{- A version of hgetContents that is not lazy. Ensures file is 
 - all read before it gets closed. -}
hGetContentsStrict :: Handle -> IO String
hGetContentsStrict h  = hGetContents h >>= \s -> length s `seq` return s

{- A version of readFile that is not lazy. -}
readFileStrict :: FilePath -> IO String
readFileStrict f = readFile f >>= \s -> length s `seq` return s

{- Attempts to read a value from a String. -}
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
	((x,_):_) -> Just x
	_ -> Nothing

{- Catches IO errors and returns a Bool -}
catchBool :: IO Bool -> IO Bool
catchBool = flip catch (const $ return False)
