{- git bundles
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Git.Bundle where

import Common
import Git
import Git.Command

import Data.Char (ord)
import qualified Data.ByteString.Char8 as S8

listHeads :: FilePath -> Repo -> IO [(Sha, Ref)]
listHeads bundle repo = map gen . S8.lines <$>
	pipeReadStrict [Param "bundle", Param "list-heads", File bundle] repo
  where
	gen l = let (s, r) = separate' (== fromIntegral (ord ' ')) l
		in (Ref s, Ref r)

unbundle :: FilePath -> Repo -> IO ()
unbundle bundle = runQuiet [Param "bundle", Param "unbundle", File bundle]
