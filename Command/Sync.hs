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
import qualified Annex.Branch
import qualified Git.Command
import qualified Git.Branch
import qualified Git.Config
import qualified Git.Ref
import qualified Git

import qualified Data.ByteString.Lazy.Char8 as L

def :: [Command]
def = [command "sync" (paramOptional (paramRepeating paramRemote))
	[seek] "synchronize local repository with remote repositories"]

-- syncing involves several operations, any of which can independantly fail
seek :: CommandSeek
seek args = do
	!branch <- currentBranch
	remotes <- syncRemotes args
	return $ concat $
		[ [ commit ]
		, [ mergeLocal branch ]
		, [ pullRemote remote branch | remote <- remotes ]
		, [ mergeAnnex ]
		, [ pushLocal branch ]
		, [ pushRemote remote branch | remote <- remotes ]
		]

syncBranch :: Git.Ref -> Git.Ref
syncBranch = Git.Ref.under "refs/heads/synced/"

syncRemotes :: [String] -> Annex [Remote.Remote Annex]
syncRemotes [] = filterM hasurl =<< Remote.remoteList
	where
		hasurl r = not . null <$> geturl r
		geturl r = fromRepo $ Git.Config.get ("remote." ++ Remote.name r ++ ".url") ""
syncRemotes rs = mapM Remote.byName rs

commit :: CommandStart
commit = do
	showStart "commit" ""
	next $ next $ do
		showOutput
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
			next $ next $ mergeFromIfExists syncbranch

pushLocal :: Git.Ref -> CommandStart
pushLocal branch = go =<< inRepo (Git.Ref.exists syncbranch)
	where
		syncbranch = syncBranch branch
		go False = stop
		go True = do
			updateBranch syncbranch
			stop

updateBranch :: Git.Ref -> Annex ()
updateBranch syncbranch = 
	unlessM go $ error $ "failed to update " ++ show syncbranch
	where
		go = inRepo $ Git.Command.runBool "branch"
			[ Param "-f"
			, Param $ show $ Git.Ref.base syncbranch
			]

mergeFromIfExists :: Git.Ref -> CommandCleanup
mergeFromIfExists branch = go =<< inRepo (Git.Ref.exists branch)
	where
		go True = do
			showOutput
			inRepo $ Git.Command.runBool "merge"
				[Param (show branch)]
		go False = do
			showNote $ Git.Ref.describe branch ++
				" does not exist, not merging"
			return False

pullRemote :: Remote.Remote Annex -> Git.Ref -> CommandStart
pullRemote remote branch = do
	showStart "pull" (Remote.name remote)
	next $ do
		showOutput
		fetched <- inRepo $ Git.Command.runBool "fetch"
			[Param $ Remote.name remote]
		if fetched
			then next $ mergeRemote remote branch
			else stop

{- The remote probably has both a master and a synced/master branch.
 - Which to merge from? Well, the master has whatever latest changes
 - were committed, while the synced/master may have changes that some
 - other remote synced to this remote. So, merge them both. -}
mergeRemote :: Remote.Remote Annex -> Git.Ref -> CommandCleanup
mergeRemote remote branch = all (== True) <$> mapM go [branch, syncBranch branch]
	where
		go b = do
			e <- inRepo $ Git.Branch.changed branch b
			if e
				then mergeFromIfExists $ remotebranch b
				else return True
		remotebranch = Git.Ref.under $ "refs/remotes/" ++ Remote.name remote

pushRemote :: Remote.Remote Annex -> Git.Ref -> CommandStart
pushRemote remote branch = go =<< newer
	where
		newer = do
			e <- inRepo (Git.Ref.exists syncbranchRemote)
			if e
				then inRepo $ Git.Branch.changed syncbranchRemote syncbranch
				else return True
		go False = stop
		go True = do
			showStart "push" (Remote.name remote)
			next $ next $ do
				showOutput
				inRepo $ Git.Command.runBool "push" $
					[ Param (Remote.name remote)
					, Param (show $ Annex.Branch.name)
					, Param refspec
					]
		refspec = show (Git.Ref.base branch) ++ ":" ++ show (Git.Ref.base syncbranch)
		syncbranch = syncBranch branch
		syncbranchRemote = Git.Ref.under 
			("refs/remotes/" ++ Remote.name remote) syncbranch

currentBranch :: Annex Git.Ref
currentBranch = Git.Ref . firstLine . L.unpack <$>
	inRepo (Git.Command.pipeRead [Param "symbolic-ref", Param "HEAD"])

mergeAnnex :: CommandStart
mergeAnnex = do
	Annex.Branch.forceUpdate
	stop
