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
	let syncbranch = Git.Ref.under "refs/heads/synced/" branch
	remotes <- syncRemotes syncbranch args
	return $ concat $
		[ [ commit ]
		, [ mergeLocal branch syncbranch ]
		, [ pullRemote remote branch | remote <- remotes ]
		, [ mergeAnnex ]
		, [ pushLocal syncbranch ]
		, [ pushRemote remote branch syncbranch | remote <- remotes ]
		]

syncRemotes :: Git.Ref -> [String] -> Annex [Remote.Remote Annex]
syncRemotes branch [] = defaultSyncRemotes branch
syncRemotes _ rs = mapM Remote.byName rs

defaultSyncRemotes :: Git.Ref -> Annex [Remote.Remote Annex]
defaultSyncRemotes syncbranch = mapM Remote.byName =<<
	map getRemoteName . filter isRemote . map (show . snd) <$> siblings
	where
		siblings = inRepo (Git.Ref.matching $ Git.Ref.base syncbranch)
		getRemoteName = fst . separate (== '/') . snd . separate (== '/') . snd . separate (== '/')
	        isRemote r = "refs/remotes/" `isPrefixOf` r

commit :: CommandStart
commit = do
	showStart "commit" ""
	next $ next $ do
		showOutput
		-- Commit will fail when the tree is clean, so ignore failure.
		_ <- inRepo $ Git.Command.runBool "commit"
			[Param "-a", Param "-m", Param "git-annex automatic sync"]
		return True

mergeLocal :: Git.Ref -> Git.Ref -> CommandStart
mergeLocal branch syncbranch = go =<< needmerge
	where
		needmerge = do
			unlessM (inRepo $ Git.Ref.exists syncbranch) $
				updateBranch syncbranch
			inRepo $ Git.Branch.changed branch syncbranch
		go False = stop
		go True = do
			showStart "merge" $ Git.Ref.describe syncbranch
			next $ next $ mergeFromIfExists syncbranch

pushLocal :: Git.Ref -> CommandStart
pushLocal syncbranch = go =<< inRepo (Git.Ref.exists syncbranch)
	where
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
		checkRemote remote
		showOutput
		fetched <- inRepo $ Git.Command.runBool "fetch"
			[Param $ Remote.name remote]
		if fetched
			then next $ mergeRemote remote branch
			else stop

mergeRemote :: Remote.Remote Annex -> Git.Ref -> CommandCleanup
mergeRemote remote = mergeFromIfExists .
	Git.Ref.under ("refs/remotes/" ++ Remote.name remote ++ "/synced")

pushRemote :: Remote.Remote Annex -> Git.Ref -> Git.Ref -> CommandStart
pushRemote remote branch syncbranch = go =<< newer
	where
		newer = inRepo $ Git.Branch.changed syncbranchRemote syncbranch
		go False = stop
		go True = do
			showStart "push" (Remote.name remote)
			ex <- inRepo $ Git.Ref.exists syncbranchRemote
			next $ next $ do
				showOutput
				inRepo $ Git.Command.runBool "push" $
					[ Param (Remote.name remote)
					, Param (show $ Annex.Branch.name) ] ++ 
					[ Param refspec | ex ]
		refspec = show (Git.Ref.base branch) ++ ":" ++ show (Git.Ref.base syncbranch)
		syncbranchRemote = Git.Ref.under 
			("refs/remotes/" ++ Remote.name remote) syncbranch

currentBranch :: Annex Git.Ref
currentBranch = Git.Ref . firstLine . L.unpack <$>
	inRepo (Git.Command.pipeRead [Param "symbolic-ref", Param "HEAD"])

checkRemote :: Remote.Remote Annex -> Annex ()
checkRemote remote = do
	remoteurl <- fromRepo $
		Git.Config.get ("remote." ++ Remote.name remote ++ ".url") ""
	when (null remoteurl) $
		error $ "No url is configured for the remote: " ++ Remote.name remote

mergeAnnex :: CommandStart
mergeAnnex = do
	Annex.Branch.forceUpdate
	stop
