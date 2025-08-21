{- git merging
 -
 - Copyright 2012-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Git.Merge (
	MergeConfig(..),
	CommitMode(..),
	merge,
	merge',
	mergeUnrelatedHistoriesParam,
	stageMerge,
) where

import Common
import Git
import Git.Command
import Git.Branch (CommitMode(..))

data MergeConfig
	= MergeNonInteractive
	-- ^ avoids interactive merge with commit message edit
	| MergeUnrelatedHistories
	-- ^ avoids git's prevention of merging unrelated histories
	| MergeQuiet
	-- ^ avoids usual output when merging, but errors will still be
	-- displayed
	deriving (Eq)

merge :: Ref -> [MergeConfig] -> CommitMode -> Repo -> IO Bool
merge = merge' []

merge' :: [CommandParam] -> Ref -> [MergeConfig] -> CommitMode -> Repo -> IO Bool
merge' extraparams branch mergeconfig commitmode r
	| MergeNonInteractive `notElem` mergeconfig =
		go [Param $ fromRef branch]
	| otherwise = go [Param "--no-edit", Param $ fromRef branch]
  where
	go ps = merge'' (sp ++ [Param "merge"] ++ qp ++ ps ++ extraparams) mergeconfig r
	sp
		| commitmode == AutomaticCommit =
			[Param "-c", Param "commit.gpgsign=false"]
		| otherwise = []
	qp
		| MergeQuiet `notElem` mergeconfig = []
		| otherwise = [Param "--quiet"]

merge'' :: [CommandParam] -> [MergeConfig] -> Repo -> IO Bool
merge'' ps mergeconfig r
	| MergeUnrelatedHistories `elem` mergeconfig =
		go (ps ++ [mergeUnrelatedHistoriesParam])
	| otherwise = go ps
  where
	go ps' = runBool ps' r

mergeUnrelatedHistoriesParam :: CommandParam
mergeUnrelatedHistoriesParam = Param "--allow-unrelated-histories"

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
