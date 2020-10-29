{- git versions
 -
 - Copyright 2011, 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Git.Version (
	installed,
	older,
	normalize,
	GitVersion,
) where

import Utility.Process
import Utility.DottedVersion

type GitVersion = DottedVersion

installed :: IO GitVersion
installed = normalize . extract <$> readProcess "git" ["--version"]
  where
	extract s = case lines s of
		[] -> ""
		(l:_) -> unwords $ drop 2 $ words l

older :: String -> IO Bool
older n = do
	v <- installed
	return $ v < normalize n 
