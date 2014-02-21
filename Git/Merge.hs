{- git merging
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Merge where

import Common
import Git
import Git.Command
import Git.BuildVersion

{- Avoids recent git's interactive merge. -}
mergeNonInteractive :: Ref -> Repo -> IO Bool
mergeNonInteractive branch
	| older "1.7.7.6" = merge [Param $ fromRef branch]
	| otherwise = merge [Param "--no-edit", Param $ fromRef branch]
  where
	merge ps = runBool $ Param "merge" : ps
