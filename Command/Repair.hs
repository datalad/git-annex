{- git-annex command
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Repair where

import Common.Annex
import Command
import qualified Annex
import qualified Git.Repair
import qualified Annex.Branch
import Git.Fsck (MissingObjects)

def :: [Command]
def = [noCommit $ dontCheck repoExists $
	command "repair" paramNothing seek SectionMaintenance "recover broken git repository"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = next $ next $ runRepair =<< Annex.getState Annex.force

runRepair :: Bool -> Annex Bool
runRepair forced = do
	(ok, stillmissing) <- inRepo $ Git.Repair.runRepair forced
	when ok $
		repairAnnexBranch stillmissing
	return ok

{- After git repository repair, the .git/annex/index file could
 - still be broken, by pointing to bad objects, or might just be corrupt on
 - its own. Since this index file is not used to stage things
 - for long durations of time, it can safely be deleted if it is broken.
 -
 - Otherwise, commit the index file to the git-annex branch.
 - This way, if the git-annex branch got rewound to an old version by
 - the repository repair, or was completely deleted, this will get it back
 - to a good state. Note that in the unlikely case where the git-annex
 - branch is ok, and has new changes from elsewhere not yet reflected in
 - the index, this does properly merge those into the index before
 - committing.
 -}
repairAnnexBranch :: MissingObjects -> Annex ()
repairAnnexBranch missing = ifM okindex
	( do
		Annex.Branch.forceCommit "committing index after git repository repair"
		liftIO $ putStrLn "Successfully recovered the git-annex branch using .git/annex/index"
	, do
		inRepo $ nukeFile . gitAnnexIndex
		liftIO $ putStrLn "Had to delete the .git/annex/index file as it was corrupt. It would be a very good idea to run: git annex fsck --fast"
	)
  where
	okindex = Annex.Branch.withIndex $
		inRepo $ Git.Repair.checkIndex missing
