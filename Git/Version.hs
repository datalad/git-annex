{- git versions
 -
 - Copyright 2011, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Version (
	installed,
	normalize,
	GitVersion,
) where

import Common
import Utility.DottedVersion

type GitVersion = DottedVersion

installed :: IO GitVersion
installed = normalize . extract <$> readProcess "git" ["--version"]
  where
	extract s = case lines s of
		[] -> ""
		(l:_) -> unwords $ drop 2 $ words l
