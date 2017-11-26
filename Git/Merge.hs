{- git merging
 -
 - Copyright 2012-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Merge (
	MergeConfig(..),
	CommitMode(..),
	merge,
	merge',
	stageMerge,
) where

import Common
import Git
import Git.Command
import qualified Git.BuildVersion
import qualified Git.Version
import Git.Branch (CommitMode(..))

data MergeConfig
	= MergeNonInteractive
	-- ^ avoids recent git's interactive merge
	| MergeUnrelatedHistories
	-- ^ avoids recent git's prevention of merging unrelated histories
	deriving (Eq)

merge :: Ref -> [MergeConfig] -> CommitMode -> Repo -> IO Bool
merge = merge' []

merge' :: [CommandParam] -> Ref -> [MergeConfig] -> CommitMode -> Repo -> IO Bool
merge' extraparams branch mergeconfig commitmode r
	| MergeNonInteractive `notElem` mergeconfig || Git.BuildVersion.older "1.7.7.6" =
		go [Param $ fromRef branch]
	| otherwise = go [Param "--no-edit", Param $ fromRef branch]
  where
	go ps = merge'' (sp ++ [Param "merge"] ++ ps ++ extraparams) mergeconfig r
	sp
		| commitmode == AutomaticCommit =
			[Param "-c", Param "commit.gpgsign=false"]
		| otherwise = []

merge'' :: [CommandParam] -> [MergeConfig] -> Repo -> IO Bool
merge'' ps mergeconfig r
	| MergeUnrelatedHistories `elem` mergeconfig =
		ifM (Git.Version.older "2.9.0")
			( go ps
			, go (ps ++ [Param "--allow-unrelated-histories"])
			)
	| otherwise = go ps
  where
	go ps' = runBool ps' r

{- Stage the merge into the index, but do not commit it.-}
stageMerge :: Ref -> [MergeConfig] -> Repo -> IO Bool
stageMerge branch = merge''
	[ Param "merge"
	, Param "--quiet"
	, Param "--no-commit"
	-- Without this, a fast-forward merge is done, since it involves no
	-- commit.
	, Param "--no-ff"
	, Param $ fromRef branch
	]
