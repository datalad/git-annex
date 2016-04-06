{- git merging
 -
 - Copyright 2012, 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Merge where

import Common
import Git
import Git.Command
import Git.BuildVersion
import Git.Branch (CommitMode(..))

{- Avoids recent git's interactive merge. -}
mergeNonInteractive :: Ref -> CommitMode -> Repo -> IO Bool
mergeNonInteractive = mergeNonInteractive' []

mergeNonInteractive' :: [CommandParam] -> Ref -> CommitMode -> Repo -> IO Bool
mergeNonInteractive' extraparams branch commitmode
	| older "1.7.7.6" = merge [Param $ fromRef branch]
	| otherwise = merge $ [Param "--no-edit", Param $ fromRef branch]
  where
	merge ps = runBool $ sp ++ [Param "merge"] ++ ps ++ extraparams
	sp
		| commitmode == AutomaticCommit =
			[Param "-c", Param "commit.gpgsign=false"]
		| otherwise = []

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
