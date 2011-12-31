{- git version checking
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Version where

import System.Cmd.Utils

import Common

version :: IO String
version = do
	(_, s) <- pipeFrom "git" ["--version"]
	return $ last $ words $ head $ lines s

older :: String -> IO Bool
older v = do
	current <- version
	return $ normalize current < normalize v

{- To compare dotted versions like 1.7.7 and 1.8, they are normalized to
 - a somewhat arbitrary integer representation. -}
normalize :: String -> Integer
normalize = sum . mult 1 . reverse .
		extend precision . take precision .
		map readi . split "."
	where
		extend n l = l ++ replicate (n - length l) 0
		mult _ [] = []
		mult n (x:xs) = (n*x) : mult (n*10^width) xs
		readi :: String -> Integer
		readi s = case reads s of
			((x,_):_) -> x
			_ -> 0
		precision = 10 -- number of segments of the version to compare
		width = length "yyyymmddhhmmss" -- maximum width of a segment
