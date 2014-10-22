{- git versions
 -
 - Copyright 2011, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Version where

import Common

data GitVersion = GitVersion String Integer
	deriving (Eq)

instance Ord GitVersion where
	compare (GitVersion _ x) (GitVersion _ y) = compare x y

instance Show GitVersion where
	show (GitVersion s _) = s

installed :: IO GitVersion
installed = normalize . extract <$> readProcess "git" ["--version"]
  where
	extract s = case lines s of
		[] -> ""
		(l:_) -> unwords $ drop 2 $ words l

{- To compare dotted versions like 1.7.7 and 1.8, they are normalized to
 - a somewhat arbitrary integer representation. -}
normalize :: String -> GitVersion
normalize v = GitVersion v $ 
	sum $ mult 1 $ reverse $ extend precision $ take precision $
		map readi $ split "." v
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
