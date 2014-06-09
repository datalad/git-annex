{- git merging
 -
 - Copyright 2012, 2014 Joey Hess <joey@kitenet.net>
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

{- Stage the merge into the index, but do not commit it.-}
stageMerge :: Ref -> Repo -> IO Bool
stageMerge branch = runBool
	[ Param "merge"
	, Param "--quiet"
	, Param "--no-commit"
	-- Without this, a fast-forward merge is done, since it involves no
	-- commit.
	, Param "--no-ff"
	, Param $ fromRef branch
	]
