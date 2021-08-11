{- git-annex command
 -
 - Copyright 2011-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Merge where

import Command
import qualified Annex.Branch
import qualified Git
import qualified Git.Branch
import Annex.CurrentBranch
import Command.Sync (prepMerge, mergeLocal, mergeConfig, merge, notOnlyAnnexOption, parseUnrelatedHistoriesOption)
import Git.Types

cmd :: Command
cmd = command "merge" SectionMaintenance
	"merge changes from remotes"
	(paramOptional paramRef) (seek <$$> optParser)

data MergeOptions = MergeOptions
	{ mergeBranches :: [String]
	, allowUnrelatedHistories :: Bool
	}

optParser :: CmdParamsDesc -> Parser MergeOptions
optParser desc = MergeOptions
	<$> cmdParams desc
	<*> parseUnrelatedHistoriesOption

seek :: MergeOptions -> CommandSeek
seek o
	| mergeBranches o == [] = do
		prepMerge
		commandAction mergeAnnexBranch
		commandAction (mergeSyncedBranch o)
	| otherwise = do
		prepMerge
		forM_ (mergeBranches o) $
			commandAction . mergeBranch o . Git.Ref . encodeBS

mergeAnnexBranch :: CommandStart
mergeAnnexBranch = starting "merge" ai si $ do
	_ <- Annex.Branch.update
	-- commit explicitly, in case no remote branches were merged
	Annex.Branch.commit =<< Annex.Branch.commitMessage
	next $ return True
  where
	ai = ActionItemOther (Just (fromRef Annex.Branch.name))
	si = SeekInput []

mergeSyncedBranch :: MergeOptions -> CommandStart
mergeSyncedBranch o = do
	mc <- mergeConfig (allowUnrelatedHistories o)
	mergeLocal mc def =<< getCurrentBranch

mergeBranch :: MergeOptions -> Git.Ref -> CommandStart
mergeBranch o r = starting "merge" ai si $ do
	currbranch <- getCurrentBranch
	mc <- mergeConfig (allowUnrelatedHistories o)
	let so = def { notOnlyAnnexOption = True }
	next $ merge currbranch mc so Git.Branch.ManualCommit r
  where
	ai = ActionItemOther (Just (Git.fromRef r))
	si = SeekInput []
