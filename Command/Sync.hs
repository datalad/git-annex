{- git-annex command
 -
 - Copyright 2011 Joachim Breitner <mail@joachim-breitner.de>
 - Copyright 2011-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Sync where

import Common.Annex
import Command
import qualified Annex
import qualified Annex.Branch
import qualified Remote
import qualified Types.Remote as Remote
import Annex.Direct
import Annex.Hook
import qualified Git.Command
import qualified Git.LsFiles as LsFiles
import qualified Git.Branch
import qualified Git.Ref
import qualified Git
import qualified Types.Remote
import qualified Remote.Git
import Config
import Annex.Wanted
import Annex.Content
import Command.Get (getKeyFile')
import qualified Command.Move
import Logs.Location
import Annex.Drop
import Annex.UUID
import Annex.AutoMerge

import Control.Concurrent.MVar

def :: [Command]
def = [withOptions syncOptions $
	command "sync" (paramOptional (paramRepeating paramRemote))
	seek SectionCommon "synchronize local repository with remotes"]

syncOptions :: [Option]
syncOptions = [ contentOption ]

contentOption :: Option
contentOption = flagOption [] "content" "also transfer file contents"

seek :: CommandSeek
seek rs = do
	prepMerge

	-- There may not be a branch checked out until after the commit,
	-- or perhaps after it gets merged from the remote.
	-- So only look it up once it's needed, and if once there is a
	-- branch, cache it.
	mvar <- liftIO newEmptyMVar
	let getbranch = ifM (liftIO $ isEmptyMVar mvar)
		( do
			branch <- inRepo Git.Branch.current
			when (isJust branch) $
				liftIO $ putMVar mvar branch
			return branch
		, liftIO $ readMVar mvar
		)
	let withbranch a = a =<< getbranch

	remotes <- syncRemotes rs
	let gitremotes = filter Remote.gitSyncableRemote remotes
	let dataremotes = filter (not . remoteAnnexIgnore . Remote.gitconfig) remotes

	-- Syncing involves many actions, any of which can independently
	-- fail, without preventing the others from running.
	seekActions $ return $ concat
		[ [ commit ]
		, [ withbranch mergeLocal ]
		, map (withbranch . pullRemote) gitremotes
		,  [ mergeAnnex ]
		]
	whenM (Annex.getFlag $ optionName contentOption) $
		whenM (seekSyncContent dataremotes) $
			-- Transferring content can take a while,
			-- and other changes can be pushed to the git-annex
			-- branch on the remotes in the meantime, so pull
			-- and merge again to avoid our push overwriting
			-- those changes.
			seekActions $ return $ concat
				[ map (withbranch . pullRemote) gitremotes
				, [ commitAnnex, mergeAnnex ]
				]
	seekActions $ return $ concat
		[ [ withbranch pushLocal ]
		, map (withbranch . pushRemote) gitremotes
		]

{- Merging may delete the current directory, so go to the top
 - of the repo. This also means that sync always acts on all files in the
 - repository, not just on a subdirectory. -}
prepMerge :: Annex ()
prepMerge = liftIO . setCurrentDirectory =<< fromRepo Git.repoPath

syncBranch :: Git.Ref -> Git.Ref
syncBranch = Git.Ref.under "refs/heads/synced" . fromDirectBranch

remoteBranch :: Remote -> Git.Ref -> Git.Ref
remoteBranch remote = Git.Ref.underBase $ "refs/remotes/" ++ Remote.name remote

syncRemotes :: [String] -> Annex [Remote]
syncRemotes rs = ifM (Annex.getState Annex.fast) ( nub <$> pickfast , wanted )
  where
	pickfast = (++) <$> listed <*> (filterM good =<< fastest <$> available)
	wanted
		| null rs = filterM good =<< concat . Remote.byCost <$> available
		| otherwise = listed
	listed = catMaybes <$> mapM (Remote.byName . Just) rs
	available = filter (remoteAnnexSync . Types.Remote.gitconfig)
		. filter (not . Remote.isXMPPRemote)
		<$> Remote.remoteList
	good r
		| Remote.gitSyncableRemote r = Remote.Git.repoAvail $ Types.Remote.repo r
		| otherwise = return True
	fastest = fromMaybe [] . headMaybe . Remote.byCost

commit :: CommandStart
commit = next $ next $ ifM isDirect
	( do
		showStart "commit" ""
		void stageDirect
		void preCommitDirect
		commitStaged commitmessage
	, do
		showStart "commit" ""
		Annex.Branch.commit "update"
		-- Commit will fail when the tree is clean, so ignore failure.
		_ <- inRepo $ tryIO . Git.Command.runQuiet
			[ Param "commit"
			, Param "-a"
			, Param "-m"
			, Param commitmessage
			]
		return True
	)
  where
	commitmessage = "git-annex automatic sync"

commitStaged :: String -> Annex Bool
commitStaged commitmessage = go =<< inRepo Git.Branch.currentUnsafe
  where
	go Nothing = return False
	go (Just branch) = do
		runAnnexHook preCommitAnnexHook
		parent <- inRepo $ Git.Ref.sha branch
		void $ inRepo $ Git.Branch.commit False commitmessage branch
			(maybeToList parent)
		return True

mergeLocal :: Maybe Git.Ref -> CommandStart
mergeLocal Nothing = stop
mergeLocal (Just branch) = go =<< needmerge
  where
	syncbranch = syncBranch branch
	needmerge = ifM isBareRepo
		( return False
		, do
			unlessM (inRepo $ Git.Ref.exists syncbranch) $
				inRepo $ updateBranch syncbranch
			inRepo $ Git.Branch.changed branch syncbranch
		)
	go False = stop
	go True = do
		showStart "merge" $ Git.Ref.describe syncbranch
		next $ next $ autoMergeFrom syncbranch (Just branch)

pushLocal :: Maybe Git.Ref -> CommandStart
pushLocal Nothing = stop
pushLocal (Just branch) = do
	-- Update the sync branch to match the new state of the branch
	inRepo $ updateBranch $ syncBranch branch
	-- In direct mode, we're operating on some special direct mode
	-- branch, rather than the intended branch, so update the indended
	-- branch.
	whenM isDirect $
		inRepo $ updateBranch $ fromDirectBranch branch
	stop

updateBranch :: Git.Ref -> Git.Repo -> IO ()
updateBranch syncbranch g = 
	unlessM go $ error $ "failed to update " ++ Git.fromRef syncbranch
  where
	go = Git.Command.runBool
		[ Param "branch"
		, Param "-f"
		, Param $ Git.fromRef $ Git.Ref.base syncbranch
		] g

pullRemote :: Remote -> Maybe Git.Ref -> CommandStart
pullRemote remote branch = do
	showStart "pull" (Remote.name remote)
	next $ do
		showOutput
		stopUnless fetch $
			next $ mergeRemote remote branch
  where
	fetch = inRepo $ Git.Command.runBool
		[Param "fetch", Param $ Remote.name remote]

{- The remote probably has both a master and a synced/master branch.
 - Which to merge from? Well, the master has whatever latest changes
 - were committed (or pushed changes, if this is a bare remote),
 - while the synced/master may have changes that some
 - other remote synced to this remote. So, merge them both. -}
mergeRemote :: Remote -> Maybe Git.Ref -> CommandCleanup
mergeRemote remote b = case b of
	Nothing -> do
		branch <- inRepo Git.Branch.currentUnsafe
		and <$> mapM (merge Nothing) (branchlist branch)
	Just thisbranch ->
		and <$> (mapM (merge (Just thisbranch)) =<< tomerge (branchlist b))
  where
	merge thisbranch = flip autoMergeFrom thisbranch . remoteBranch remote
	tomerge = filterM (changed remote)
	branchlist Nothing = []
	branchlist (Just branch) = [branch, syncBranch branch]

pushRemote :: Remote -> Maybe Git.Ref -> CommandStart
pushRemote _remote Nothing = stop
pushRemote remote (Just branch) = go =<< needpush
  where
	needpush
		| remoteAnnexReadOnly (Types.Remote.gitconfig remote) = return False
		| otherwise = anyM (newer remote) [syncBranch branch, Annex.Branch.name]
	go False = stop
	go True = do
		showStart "push" (Remote.name remote)
		next $ next $ do
			showOutput
			ok <- inRepo $ pushBranch remote branch
			unless ok $ do
				warning $ unwords [ "Pushing to " ++ Remote.name remote ++ " failed." ]
				showLongNote "(non-fast-forward problems can be solved by setting receive.denyNonFastforwards to false in the remote's git config)"
			return ok

{- Pushes a regular branch like master to a remote. Also pushes the git-annex
 - branch.
 -
 - If the remote is a bare git repository, it's best to push the regular 
 - branch directly to it, so that cloning/pulling will get it.
 - On the other hand, if it's not bare, pushing to the checked out branch
 - will fail, and this is why we push to its syncBranch.
 -
 - Git offers no way to tell if a remote is bare or not, so both methods
 - are tried.
 -
 - The direct push is likely to spew an ugly error message, so stderr is
 - elided. Since git progress display goes to stderr too, the sync push
 - is done first, and actually sends the data. Then the direct push is
 - tried, with stderr discarded, to update the branch ref on the remote.
 -
 - The sync push forces the update of the remote synced/git-annex branch.
 - This is necessary if a transition has rewritten the git-annex branch.
 - Normally any changes to the git-annex branch get pulled and merged before
 - this push, so this forcing is unlikely to overwrite new data pushed
 - in from another repository that is also syncing.
 -
 - But overwriting of data on synced/git-annex can happen, in a race.
 - The only difference caused by using a forced push in that case is that
 - the last repository to push wins the race, rather than the first to push.
 -
 - The sync push will fail to overwrite if receive.denyNonFastforwards is
 - set on the remote.
 -}
pushBranch :: Remote -> Git.Ref -> Git.Repo -> IO Bool
pushBranch remote branch g = tryIO (directpush g) `after` syncpush g
  where
	syncpush = Git.Command.runBool $ pushparams
		[ Git.Branch.forcePush $ refspec Annex.Branch.name
		, refspec branch
		]
	directpush = Git.Command.runQuiet $ pushparams
		[Git.fromRef $ Git.Ref.base $ fromDirectBranch branch]
	pushparams branches =
		[ Param "push"
		, Param $ Remote.name remote
		] ++ map Param branches
	refspec b = concat 
		[ Git.fromRef $ Git.Ref.base b
		,  ":"
		, Git.fromRef $ Git.Ref.base $ syncBranch b
		]

commitAnnex :: CommandStart
commitAnnex = do
	Annex.Branch.commit "update"
	stop

mergeAnnex :: CommandStart
mergeAnnex = do
	void Annex.Branch.forceUpdate
	stop

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

{- If it's preferred content, and we don't have it, get it from one of the
 - listed remotes (preferring the cheaper earlier ones).
 -
 - Send it to each remote that doesn't have it, and for which it's
 - preferred content.
 -
 - Drop it locally if it's not preferred content (honoring numcopies).
 - 
 - Drop it from each remote that has it, where it's not preferred content
 - (honoring numcopies).
 -
 - If any file movements were generated, returns true.
 -}
seekSyncContent :: [Remote] -> Annex Bool
seekSyncContent rs = do
	mvar <- liftIO newEmptyMVar
	mapM_ (go mvar) =<< seekHelper LsFiles.inRepo []
	liftIO $ not <$> isEmptyMVar mvar
  where
	go mvar f = ifAnnexed f
		(\v -> void (liftIO (tryPutMVar mvar ())) >> syncFile rs f v)
		noop

syncFile :: [Remote] -> FilePath -> (Key, Backend) -> Annex ()
syncFile rs f (k, _) = do
	locs <- loggedLocations k
	let (have, lack) = partition (\r -> Remote.uuid r `elem` locs) rs

	got <- anyM id =<< handleget have
	putrs <- catMaybes . snd . unzip <$> (sequence =<< handleput lack)

	u <- getUUID
	let locs' = concat [[u | got], putrs, locs]

	-- Using callCommandAction rather than commandAction for drops,
	-- because a failure to drop does not mean the sync failed.
	handleDropsFrom locs' rs "unwanted" True k (Just f)
		Nothing callCommandAction
  where
  	wantget have = allM id 
		[ pure (not $ null have)
		, not <$> inAnnex k
		, wantGet True (Just k) (Just f)
		]
	handleget have = ifM (wantget have)
		( return [ get have ]
		, return []
		)
	get have = commandAction $ do
		showStart "get" f
		next $ next $ getViaTmp k $ \dest -> getKeyFile' k (Just f) dest have

	wantput r
		| Remote.readonly r || remoteAnnexReadOnly (Types.Remote.gitconfig r) = return False
		| otherwise = wantSend True (Just k) (Just f) (Remote.uuid r)
	handleput lack = ifM (inAnnex k)
		( map put <$> filterM wantput lack
		, return []
		)
	put dest = do
		ok <- commandAction $ do
			showStart "copy" f
			next $ Command.Move.toPerform dest False k (Just f)
		return (ok, if ok then Just (Remote.uuid dest) else Nothing)
