{- git-annex command
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Repair where

import Common.Annex
import Command
import qualified Annex
import qualified Git.Repair
import qualified Annex.Branch
import qualified Git.Ref
import Git.Types
import Annex.Version

cmd :: Command
cmd = noCommit $ dontCheck repoExists $
	command "repair" SectionMaintenance 
		"recover broken git repository"
		paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing start

start :: CommandStart
start = next $ next $ runRepair =<< Annex.getState Annex.force

runRepair :: Bool -> Annex Bool
runRepair forced = do
	(ok, modifiedbranches) <- inRepo $
		Git.Repair.runRepair isAnnexSyncBranch forced
	-- This command can be run in git repos not using git-annex,
	-- so avoid git annex branch stuff in that case.
	whenM (isJust <$> getVersion) $
		repairAnnexBranch modifiedbranches
	return ok

{- After git repository repair, the .git/annex/index file could
 - still be broken, by pointing to bad objects, or might just be corrupt on
 - its own. Since this index file is not used to stage things
 - for long durations of time, it can safely be deleted if it is broken.
 -
 - Otherwise, if the git-annex branch was modified by the repair,
 - commit the index file to the git-annex branch.
 - This way, if the git-annex branch got rewound to an old version by
 - the repository repair, or was completely deleted, this will get it back
 - to a good state. Note that in the unlikely case where the git-annex
 - branch was rewound to a state that, had new changes from elsewhere not
 - yet reflected in the index, this does properly merge those into the
 - index before committing.
 -}
repairAnnexBranch :: [Branch] -> Annex ()
repairAnnexBranch modifiedbranches
	| Annex.Branch.fullname `elem` modifiedbranches = ifM okindex
		( commitindex
		, do
			nukeindex
			missingbranch
		)
	| otherwise = ifM okindex
		( noop
		, do
			nukeindex
			ifM (null <$> inRepo (Git.Ref.matching [Annex.Branch.fullname]))
				( missingbranch
				, liftIO $ putStrLn "No data was lost."
				)
		)
  where
	okindex = Annex.Branch.withIndex $ inRepo Git.Repair.checkIndex
	commitindex = do
		Annex.Branch.forceCommit "committing index after git repository repair"
		liftIO $ putStrLn "Successfully recovered the git-annex branch using .git/annex/index"
	nukeindex = do
		inRepo $ nukeFile . gitAnnexIndex
		liftIO $ putStrLn "Had to delete the .git/annex/index file as it was corrupt."
	missingbranch = liftIO $ putStrLn "Since the git-annex branch is not up-to-date anymore. It would be a very good idea to run: git annex fsck --fast"

trackingOrSyncBranch :: Ref -> Bool
trackingOrSyncBranch b = Git.Repair.isTrackingBranch b || isAnnexSyncBranch b

isAnnexSyncBranch :: Ref -> Bool
isAnnexSyncBranch b = "refs/synced/" `isPrefixOf` fromRef b
