{- git merging
 -
 - Copyright 2012-2016 Joey Hess <id@joeyh.name>
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
import qualified Git.Version
import Git.Branch (CommitMode(..))

data MergeConfig
	= MergeNonInteractive
	-- ^ avoids interactive merge
	| MergeUnrelatedHistories
	-- ^ avoids git's prevention of merging unrelated histories
	deriving (Eq)

merge :: Ref -> [MergeConfig] -> CommitMode -> Repo -> IO Bool
merge = merge' []

merge' :: [CommandParam] -> Ref -> [MergeConfig] -> CommitMode -> Repo -> IO Bool
merge' extraparams branch mergeconfig commitmode r
	| MergeNonInteractive `notElem` mergeconfig =
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
	| MergeUnrelatedHistories `elem` mergeconfig = do
		up <- mergeUnrelatedHistoriesParam
		go (ps ++ maybeToList up)
	| otherwise = go ps
  where
	go ps' = runBool ps' r

{- Git used to default to merging unrelated histories; newer versions need
 - an option. -}
mergeUnrelatedHistoriesParam :: IO (Maybe CommandParam)
mergeUnrelatedHistoriesParam = ifM (Git.Version.older "2.9.0")
	( return Nothing
	, return (Just (Param "--allow-unrelated-histories"))
	)

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
