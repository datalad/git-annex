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
import Git.BuildVersion
import Git.Branch (CommitMode(..))
import Git.Env

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
	| MergeNonInteractive `notElem` mergeconfig || older "1.7.7.6" =
		go [Param $ fromRef branch]
	| otherwise = go [Param "--no-edit", Param $ fromRef branch]
  where
	go ps = runBool (sp ++ [Param "merge"] ++ ps ++ extraparams)
		=<< cfgRepo mergeconfig r
	sp
		| commitmode == AutomaticCommit =
			[Param "-c", Param "commit.gpgsign=false"]
		| otherwise = []

{- Stage the merge into the index, but do not commit it.-}
stageMerge :: Ref -> [MergeConfig] -> Repo -> IO Bool
stageMerge branch mergeconfig r = runBool
	[ Param "merge"
	, Param "--quiet"
	, Param "--no-commit"
	-- Without this, a fast-forward merge is done, since it involves no
	-- commit.
	, Param "--no-ff"
	, Param $ fromRef branch
	] =<< cfgRepo mergeconfig r

cfgRepo :: [MergeConfig] -> Repo -> IO Repo
cfgRepo mergeconfig r
	| MergeUnrelatedHistories `elem` mergeconfig =
		addGitEnv r "GIT_MERGE_ALLOW_UNRELATED_HISTORIES" "1"
	| otherwise = return r
