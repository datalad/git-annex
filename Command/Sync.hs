{- git-annex command
 -
 - Copyright 2011 Joachim Breitner <mail@joachim-breitner.de>
 - Copyright 2011-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Sync (
	cmd,
	prepMerge,
	mergeLocal,
	mergeRemote,
	commitStaged,
	commitMsg,
	pushBranch,
	updateBranch,
	syncBranch,
	updateSyncBranch,
) where

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
import qualified Git.Types as Git
import qualified Git.Ref
import qualified Git
import qualified Remote.Git
import Config
import Annex.Wanted
import Annex.Content
import Command.Get (getKeyFile')
import qualified Command.Move
import Logs.Location
import Annex.Drop
import Annex.UUID
import Logs.UUID
import Annex.AutoMerge
import Annex.Ssh
import Annex.BloomFilter
import Utility.Bloom

import Control.Concurrent.MVar
import qualified Data.Map as M

cmd :: Command
cmd = command "sync" SectionCommon 
	"synchronize local repository with remotes"
	(paramRepeating paramRemote) (seek <$$> optParser)

data SyncOptions  = SyncOptions
	{ syncWith :: CmdParams
	, contentOption :: Bool
	, messageOption :: Maybe String
	, keyOptions :: Maybe KeyOptions
	}

optParser :: CmdParamsDesc -> Parser SyncOptions
optParser desc = SyncOptions
	<$> cmdParams desc
	<*> switch
		( long "content"
		<> help "also transfer file contents"
		)
	<*> optional (strOption
		( long "message" <> short 'm' <> metavar "MSG"
		<> help "commit message"
		))
	<*> optional parseAllOption

seek :: SyncOptions -> CommandSeek
seek o = do
	prepMerge

	-- There may not be a branch checked out until after the commit,
	-- or perhaps after it gets merged from the remote, or perhaps
	-- never.
	-- So only look it up once it's needed, and once there is a
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

	remotes <- syncRemotes (syncWith o)
	let gitremotes = filter Remote.gitSyncableRemote remotes
	let dataremotes = filter (not . remoteAnnexIgnore . Remote.gitconfig) remotes

	-- Syncing involves many actions, any of which can independently
	-- fail, without preventing the others from running.
	seekActions $ return $ concat
		[ [ commit o ]
		, [ withbranch mergeLocal ]
		, map (withbranch . pullRemote) gitremotes
		,  [ mergeAnnex ]
		]
	when (contentOption o) $
		whenM (seekSyncContent o dataremotes) $
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
prepMerge = Annex.changeDirectory =<< fromRepo Git.repoPath

syncBranch :: Git.Ref -> Git.Ref
syncBranch = Git.Ref.under "refs/heads/synced" . fromDirectBranch

remoteBranch :: Remote -> Git.Ref -> Git.Ref
remoteBranch remote = Git.Ref.underBase $ "refs/remotes/" ++ Remote.name remote

syncRemotes :: [String] -> Annex [Remote]
syncRemotes ps = do
	-- Get remote list first, doing automatic initialization
	-- of remotes when possible.
	syncRemotes' ps =<< Remote.remoteList' True

syncRemotes' :: [String] -> [Remote] -> Annex [Remote]
syncRemotes' ps remotelist = ifM (Annex.getState Annex.fast) ( nub <$> pickfast , wanted )
  where
	pickfast = (++) <$> listed <*> (filterM good (fastest available))
	
	wanted
		| null ps = filterM good (concat $ Remote.byCost available)
		| otherwise = listed
	
	listed = concat <$> mapM Remote.byNameOrGroup ps
	
	available = filter (remoteAnnexSync . Remote.gitconfig)
		$ filter (not . Remote.isXMPPRemote) remotelist
	
	good r
		| Remote.gitSyncableRemote r = Remote.Git.repoAvail $ Remote.repo r
		| otherwise = return True
	
	fastest = fromMaybe [] . headMaybe . Remote.byCost

commit :: SyncOptions -> CommandStart
commit o = ifM (annexAutoCommit <$> Annex.getGitConfig)
	( go
	, stop
	)
  where
	go = next $ next $ do
		commitmessage <- maybe commitMsg return (messageOption o)
		showStart "commit" ""
		Annex.Branch.commit "update"
		ifM isDirect
			( do
				void stageDirect
				void preCommitDirect
				commitStaged Git.Branch.ManualCommit commitmessage
			, do
				inRepo $ Git.Branch.commitQuiet Git.Branch.ManualCommit
					[ Param "-a"
					, Param "-m"
					, Param commitmessage
					]
				return True
			)

commitMsg :: Annex String
commitMsg = do
	u <- getUUID
	m <- uuidMap
	return $ "git-annex in " ++ fromMaybe "unknown" (M.lookup u m)

commitStaged :: Git.Branch.CommitMode -> String -> Annex Bool
commitStaged commitmode commitmessage = do
	runAnnexHook preCommitAnnexHook
	mb <- inRepo Git.Branch.currentUnsafe
	let (getparent, branch) = case mb of
		Just b -> (Git.Ref.sha b, b)
		Nothing -> (Git.Ref.headSha, Git.Ref.headRef)
	parents <- maybeToList <$> inRepo getparent
	void $ inRepo $ Git.Branch.commit commitmode False commitmessage branch parents
	return True

mergeLocal :: Maybe Git.Ref -> CommandStart
mergeLocal Nothing = stop
mergeLocal (Just branch) = go =<< needmerge
  where
	syncbranch = syncBranch branch
	needmerge = ifM isBareRepo
		( return False
		, ifM (inRepo $ Git.Ref.exists syncbranch)
			( inRepo $ Git.Branch.changed branch syncbranch
			, return False
			)
		)
	go False = stop
	go True = do
		showStart "merge" $ Git.Ref.describe syncbranch
		next $ next $ autoMergeFrom syncbranch (Just branch) Git.Branch.ManualCommit

pushLocal :: Maybe Git.Ref -> CommandStart
pushLocal b = do
	updateSyncBranch b
	stop

updateSyncBranch :: Maybe Git.Ref -> Annex ()
updateSyncBranch Nothing = noop
updateSyncBranch (Just branch) = do
	-- Update the sync branch to match the new state of the branch
	inRepo $ updateBranch $ syncBranch branch
	-- In direct mode, we're operating on some special direct mode
	-- branch, rather than the intended branch, so update the indended
	-- branch.
	whenM isDirect $
		inRepo $ updateBranch $ fromDirectBranch branch

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
	fetch = inRepoWithSshOptionsTo (Remote.repo remote) (Remote.gitconfig remote) $
		Git.Command.runBool
			[Param "fetch", Param $ Remote.name remote]

{- The remote probably has both a master and a synced/master branch.
 - Which to merge from? Well, the master has whatever latest changes
 - were committed (or pushed changes, if this is a bare remote),
 - while the synced/master may have changes that some
 - other remote synced to this remote. So, merge them both. -}
mergeRemote :: Remote -> Maybe Git.Ref -> CommandCleanup
mergeRemote remote b = ifM isBareRepo
	( return True
	, case b of
		Nothing -> do
			branch <- inRepo Git.Branch.currentUnsafe
			and <$> mapM (merge Nothing) (branchlist branch)
		Just thisbranch -> do
			inRepo $ updateBranch $ syncBranch thisbranch
			and <$> (mapM (merge (Just thisbranch)) =<< tomerge (branchlist b))
	)
  where
	merge thisbranch br = autoMergeFrom (remoteBranch remote br) thisbranch Git.Branch.ManualCommit
	tomerge = filterM (changed remote)
	branchlist Nothing = []
	branchlist (Just branch) = [branch, syncBranch branch]

pushRemote :: Remote -> Maybe Git.Ref -> CommandStart
pushRemote _remote Nothing = stop
pushRemote remote (Just branch) = go =<< needpush
  where
	needpush
		| remoteAnnexReadOnly (Remote.gitconfig remote) = return False
		| otherwise = anyM (newer remote) [syncBranch branch, Annex.Branch.name]
	go False = stop
	go True = do
		showStart "push" (Remote.name remote)
		next $ next $ do
			showOutput
			ok <- inRepoWithSshOptionsTo (Remote.repo remote) (Remote.gitconfig remote) $
				pushBranch remote branch
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
		[ Git.fromRef $ Git.Ref.base $ Annex.Branch.name
		, Git.fromRef $ Git.Ref.base $ fromDirectBranch branch
		]
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

{- Without --all, only looks at files in the work tree. With --all,
 - makes 2 passes, first looking at the work tree and then all keys.
 - This ensures that preferred content expressions that match on
 - filenames work, even when in --all mode.
 -
 - If any file movements were generated, returns true.
 -}
seekSyncContent :: SyncOptions -> [Remote] -> Annex Bool
seekSyncContent o rs = do
	mvar <- liftIO newEmptyMVar
	bloom <- case keyOptions o of
		Just WantAllKeys -> Just <$> genBloomFilter (seekworktree mvar [])
		_ -> seekworktree mvar [] (const noop) >> pure Nothing
	withKeyOptions' (keyOptions o) False
		(seekkeys mvar bloom) 
		(const noop)
		[]
	liftIO $ not <$> isEmptyMVar mvar
  where
	seekworktree mvar l bloomfeeder = seekHelper LsFiles.inRepo l >>=
		mapM_ (\f -> ifAnnexed f (go (Right bloomfeeder) mvar (Just f)) noop)
	seekkeys mvar bloom getkeys =
		mapM_ (go (Left bloom) mvar Nothing) =<< getkeys
	go ebloom mvar af k = do
		void $ liftIO $ tryPutMVar mvar ()
		syncFile ebloom rs af k

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
 -}
syncFile :: Either (Maybe (Bloom Key)) (Key -> Annex ()) -> [Remote] -> AssociatedFile -> Key -> Annex ()
syncFile ebloom rs af k = do
	locs <- loggedLocations k
	let (have, lack) = partition (\r -> Remote.uuid r `elem` locs) rs

	got <- anyM id =<< handleget have
	putrs <- handleput lack

	u <- getUUID
	let locs' = concat [[u | got], putrs, locs]

	-- A bloom filter is populated with all the keys in the first pass.
	-- On the second pass, avoid dropping keys that were seen in the
	-- first pass, which would happen otherwise when preferred content
	-- matches on the filename, which is not available in the second
	-- pass.
	--
	-- When there's a false positive in the bloom filter, the result
	-- is keeping a key that preferred content doesn't really want.
	seenbloom <- case ebloom of
		Left Nothing -> pure False
		Left (Just bloom) -> pure (elemB k bloom)
		Right bloomfeeder -> bloomfeeder k >> return False
	unless seenbloom $
		-- Using callCommandAction rather than
		-- includeCommandAction for drops,
		-- because a failure to drop does not mean
		-- the sync failed.
		handleDropsFrom locs' rs "unwanted" True k af
			Nothing callCommandAction
  where
	wantget have = allM id 
		[ pure (not $ null have)
		, not <$> inAnnex k
		, wantGet True (Just k) af
		]
	handleget have = ifM (wantget have)
		( return [ get have ]
		, return []
		)
	get have = includeCommandAction $ do
		showStart' "get" k af
		next $ next $ getViaTmp k $ \dest -> getKeyFile' k af dest have

	wantput r
		| Remote.readonly r || remoteAnnexReadOnly (Remote.gitconfig r) = return False
		| otherwise = wantSend True (Just k) af (Remote.uuid r)
	handleput lack = catMaybes <$> ifM (inAnnex k)
		( forM lack $ \r ->
			ifM (wantput r <&&> put r)
				( return (Just (Remote.uuid r))
				, return Nothing
				)
		, return []
		)
	put dest = includeCommandAction $ do
		showStart' "copy" k af
		Command.Move.toStart' dest False af k
