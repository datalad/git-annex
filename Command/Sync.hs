{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 - Copyright 2011 Joachim Breitner <mail@joachim-breitner.de>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Command.Sync where

import Common.Annex
import Command
import qualified Remote
import qualified Annex
import qualified Annex.Branch
import qualified Git.Command
import qualified Git.Branch
import qualified Git.Ref
import qualified Git
import qualified Types.Remote
import qualified Remote.Git

import qualified Data.Map as M

def :: [Command]
def = [command "sync" (paramOptional (paramRepeating paramRemote))
	[seek] "synchronize local repository with remotes"]

-- syncing involves several operations, any of which can independently fail
seek :: CommandSeek
seek rs = do
	!branch <- fromMaybe nobranch <$> inRepo Git.Branch.current
	remotes <- syncRemotes rs
	return $ concat
		[ [ commit ]
		, [ mergeLocal branch ]
		, [ pullRemote remote branch | remote <- remotes ]
		, [ mergeAnnex ]
		, [ pushLocal branch ]
		, [ pushRemote remote branch | remote <- remotes ]
		]
	where
		nobranch = error "no branch is checked out"

syncBranch :: Git.Ref -> Git.Ref
syncBranch = Git.Ref.under "refs/heads/synced/"

remoteBranch :: Remote -> Git.Ref -> Git.Ref
remoteBranch remote = Git.Ref.under $ "refs/remotes/" ++ Remote.name remote

syncRemotes :: [String] -> Annex [Remote]
syncRemotes rs = ifM (Annex.getState Annex.fast) ( nub <$> pickfast , wanted )
	where
		pickfast = (++) <$> listed <*> (good =<< fastest <$> available)
		wanted
			| null rs = good =<< concat . byspeed <$> available
			| otherwise = listed
		listed = do
			l <- catMaybes <$> mapM (Remote.byName . Just) rs
			let s = filter special l
			unless (null s) $
				error $ "cannot sync special remotes: " ++
					unwords (map Types.Remote.name s)
			return l
		available = filter nonspecial <$> Remote.enabledRemoteList
		good = filterM $ Remote.Git.repoAvail . Types.Remote.repo
		nonspecial r = Types.Remote.remotetype r == Remote.Git.remote
		special = not . nonspecial
		fastest = fromMaybe [] . headMaybe . byspeed
		byspeed = map snd . sort . M.toList . costmap
		costmap = M.fromListWith (++) . map costpair
		costpair r = (Types.Remote.cost r, [r])

commit :: CommandStart
commit = do
	showStart "commit" ""
	next $ next $ do
		showOutput
		Annex.Branch.commit "update"
		-- Commit will fail when the tree is clean, so ignore failure.
		_ <- inRepo $ Git.Command.runBool "commit"
			[Param "-a", Param "-m", Param "git-annex automatic sync"]
		return True

mergeLocal :: Git.Ref -> CommandStart
mergeLocal branch = go =<< needmerge
	where
		syncbranch = syncBranch branch
		needmerge = do
			unlessM (inRepo $ Git.Ref.exists syncbranch) $
				updateBranch syncbranch
			inRepo $ Git.Branch.changed branch syncbranch
		go False = stop
		go True = do
			showStart "merge" $ Git.Ref.describe syncbranch
			next $ next $ mergeFrom syncbranch

pushLocal :: Git.Ref -> CommandStart
pushLocal branch = do
	updateBranch $ syncBranch branch
	stop

updateBranch :: Git.Ref -> Annex ()
updateBranch syncbranch = 
	unlessM go $ error $ "failed to update " ++ show syncbranch
	where
		go = inRepo $ Git.Command.runBool "branch"
			[ Param "-f"
			, Param $ show $ Git.Ref.base syncbranch
			]

pullRemote :: Remote -> Git.Ref -> CommandStart
pullRemote remote branch = do
	showStart "pull" (Remote.name remote)
	next $ do
		showOutput
		stopUnless fetch $
			next $ mergeRemote remote branch
	where
		fetch = inRepo $ Git.Command.runBool "fetch"
			[Param $ Remote.name remote]

{- The remote probably has both a master and a synced/master branch.
 - Which to merge from? Well, the master has whatever latest changes
 - were committed, while the synced/master may have changes that some
 - other remote synced to this remote. So, merge them both. -}
mergeRemote :: Remote -> Git.Ref -> CommandCleanup
mergeRemote remote branch = all id <$> (mapM merge =<< tomerge)
	where
		merge = mergeFrom . remoteBranch remote
		tomerge = filterM (changed remote) [branch, syncBranch branch]

pushRemote :: Remote -> Git.Ref -> CommandStart
pushRemote remote branch = go =<< needpush
	where
		needpush = anyM (newer remote) [syncbranch, Annex.Branch.name]
		go False = stop
		go True = do
			showStart "push" (Remote.name remote)
			next $ next $ do
				showOutput
				inRepo $ Git.Command.runBool "push"
					[ Param (Remote.name remote)
					, Param (show Annex.Branch.name)
					, Param refspec
					]
		refspec = show (Git.Ref.base branch) ++ ":" ++ show (Git.Ref.base syncbranch)
		syncbranch = syncBranch branch

mergeAnnex :: CommandStart
mergeAnnex = do
	Annex.Branch.forceUpdate
	stop

mergeFrom :: Git.Ref -> CommandCleanup
mergeFrom branch = do
	showOutput
	inRepo $ Git.Command.runBool "merge" [Param $ show branch]

changed :: Remote -> Git.Ref -> Annex Bool
changed remote b = do
	let r = remoteBranch remote b
	ifM (inRepo $ Git.Ref.exists r)
		( inRepo $ Git.Branch.changed b r
		, return False
		)

newer :: Remote -> Git.Ref -> Annex Bool
newer remote b = do
	let r = remoteBranch remote b
	ifM (inRepo $ Git.Ref.exists r)
		( inRepo $ Git.Branch.changed r b
		, return True
		)
