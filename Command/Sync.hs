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
	remotes <- syncRemotes branch args
	showStart "syncing" $ "branch " ++ Git.Ref.describe branch ++ " with remote repositories " ++ intercalate "," (map Remote.name remotes)
	showOutput
	return $
		[ commit
		, mergeLocal branch
		] ++
		[ fetch remote | remote <- remotes ] ++
		[ mergeRemote remote branch | remote <- remotes ] ++
		[ mergeAnnex ] ++
		[ pushLocal branch ] ++
		[ pushRemote remote branch | remote <- remotes ]

syncRemotes :: Git.Ref -> [String] -> Annex [Remote.Remote Annex]
syncRemotes branch [] = defaultSyncRemotes branch
syncRemotes _ rs = mapM Remote.byName rs

defaultSyncRemotes :: Git.Ref -> Annex [Remote.Remote Annex]
defaultSyncRemotes branch = mapM Remote.byName
		=<< process . L.unpack <$> inRepo showref
	where
		syncbranch = Git.Ref.under "refs/heads/synced/" branch
		showref = Git.Command.pipeRead
			[Param "show-ref", Param $ show $ Git.Ref.base syncbranch]
		process = map getRemoteName . filter isRemote .
			map getBranchName . lines
	        isRemote r = "refs/remotes/" `isPrefixOf` r
		getBranchName = snd . separate (== ' ')
		getRemoteName = fst . separate (== '/') . snd . separate (== '/') . snd . separate (== '/')

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
mergeLocal = mergeFromIfExists . Git.Ref.under "refs/heads/synced"

pushLocal :: Git.Ref -> CommandStart
pushLocal branch = do
	let syncBranch = Git.Ref.under "refs/heads/synced" branch
	ex <- inRepo $ Git.Ref.exists syncBranch
	if ex then do
		showStart "updating" $
			Git.Ref.describe syncBranch ++
			" to the state of " ++ Git.Ref.describe branch ++ "..."
		next $ next $
			inRepo $ Git.Command.runBool "branch"
				[ Param "-f"
				, Param $ show $ Git.Ref.base syncBranch
				]
	else
		return Nothing

mergeFromIfExists :: Git.Ref -> CommandStart
mergeFromIfExists fromBranch = do
	ex <- inRepo $ Git.Ref.exists fromBranch
	if ex then do
		showStart "merging" $ Git.Ref.describe fromBranch ++ "..."
		next $ next $
			inRepo $ Git.Command.runBool "merge" [Param (show fromBranch)]
	else do
		showNote $ Git.Ref.describe fromBranch ++ " does not exist, not merging."
		showOutput
		return Nothing

fetch :: Remote.Remote Annex -> CommandStart
fetch remote = do
	showStart "fetching from" (Remote.name remote)
	next $ next $ do
		showOutput
		checkRemote remote
		inRepo $ Git.Command.runBool "fetch" [Param (Remote.name remote)]

mergeRemote :: Remote.Remote Annex -> Git.Ref -> CommandStart
mergeRemote remote = mergeFromIfExists .
	Git.Ref.under ("refs/remotes/" ++ Remote.name remote ++ "/synced")

pushRemote :: Remote.Remote Annex -> Git.Ref -> CommandStart
pushRemote remote branch = do
	showStart "pushing to" (Remote.name remote)
	let syncbranch = Git.Ref.under "refs/heads/synced" branch
	let syncbranchRemote = Git.Ref.under
		("refs/remotes/" ++ Remote.name remote) syncbranch
	let refspec = show (Git.Ref.base branch) ++ ":" ++ show (Git.Ref.base syncbranch)
	ex <- inRepo $ Git.Ref.exists syncbranchRemote
	next $ next $ do
		showOutput
		inRepo $ Git.Command.runBool "push" $
			[ Param (Remote.name remote)
			, Param (show $ Annex.Branch.name) ] ++ 
			[ Param refspec | ex ]

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
mergeAnnex = next $ next $ do
	Annex.Branch.forceUpdate
	return True
