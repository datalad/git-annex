{- git-annex command
 -
 - Copyright 2011 Joachim Breitner <mail@joachim-breitner.de>
 - Copyright 2011-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Sync (
	cmd,
	CurrBranch,
	getCurrBranch,
	mergeConfig,
	merge,
	prepMerge,
	mergeLocal,
	mergeRemote,
	commitStaged,
	commitMsg,
	pushBranch,
	updateBranch,
	syncBranch,
	updateSyncBranch,
	seekExportContent,
) where

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
import qualified Git.Merge
import qualified Git.Types as Git
import qualified Git.Ref
import qualified Git
import qualified Remote.Git
import Config
import Config.GitConfig
import Config.DynamicConfig
import Config.Files
import Annex.Wanted
import Annex.Content
import Command.Get (getKey')
import qualified Command.Move
import qualified Command.Export
import Annex.Drop
import Annex.UUID
import Logs.UUID
import Logs.Export
import Annex.AutoMerge
import Annex.AdjustedBranch
import Annex.Ssh
import Annex.BloomFilter
import Annex.UpdateInstead
import Annex.Export
import Annex.LockFile
import Annex.TaggedPush
import qualified Database.Export as Export
import Utility.Bloom
import Utility.OptParse

import Control.Concurrent.MVar
import qualified Data.Map as M

cmd :: Command
cmd = withGlobalOptions [jobsOption] $
	command "sync" SectionCommon 
		"synchronize local repository with remotes"
		(paramRepeating paramRemote) (seek <--< optParser)

data SyncOptions  = SyncOptions
	{ syncWith :: CmdParams
	, commitOption :: Bool
	, noCommitOption :: Bool
	, messageOption :: Maybe String
	, pullOption :: Bool
	, pushOption :: Bool
	, contentOption :: Bool
	, noContentOption :: Bool
	, contentOfOption :: [FilePath]
	, cleanupOption :: Bool
	, keyOptions :: Maybe KeyOptions
	, resolveMergeOverride :: ResolveMergeOverride
	}

newtype ResolveMergeOverride = ResolveMergeOverride Bool

instance Default ResolveMergeOverride where
	def = ResolveMergeOverride False

optParser :: CmdParamsDesc -> Parser SyncOptions
optParser desc = SyncOptions
	<$> (many $ argument str
		( metavar desc
		<> completeRemotes
		))
	<*> switch
		( long "commit"
		<> help "commit changes to git"
		)
	<*> switch
		( long "no-commit"
		<> help "avoid git commit" 
		)
	<*> optional (strOption
		( long "message" <> short 'm' <> metavar "MSG"
		<> help "commit message"
		))
	<*> invertableSwitch "pull" True
		( help "avoid git pulls from remotes" 
		)
	<*> invertableSwitch "push" True
		( help "avoid git pushes to remotes" 
		)
	<*> switch 
		( long "content"
		<> help "transfer file contents" 
		)
	<*> switch
		( long "no-content"
		<> help "do not transfer file contents"
		)
	<*> many (strOption
		( long "content-of"
		<> short 'C'
		<> help "transfer file contents of files in a given location"
		<> metavar paramPath
		))
	<*> switch
		( long "cleanup"
		<> help "remove synced/ branches from previous sync"
		)
	<*> optional parseAllOption
	<*> (ResolveMergeOverride <$> invertableSwitch "resolvemerge" True
		( help "do not automatically resolve merge conflicts"
		))

-- Since prepMerge changes the working directory, FilePath options
-- have to be adjusted.
instance DeferredParseClass SyncOptions where
	finishParse v = SyncOptions
		<$> pure (syncWith v)
		<*> pure (commitOption v)
		<*> pure (noCommitOption v)
		<*> pure (messageOption v)
		<*> pure (pullOption v)
		<*> pure (pushOption v)
		<*> pure (contentOption v)
		<*> pure (noContentOption v)
		<*> liftIO (mapM absPath (contentOfOption v))
		<*> pure (cleanupOption v)
		<*> pure (keyOptions v)
		<*> pure (resolveMergeOverride v)

seek :: SyncOptions -> CommandSeek
seek o = allowConcurrentOutput $ do
	prepMerge

	getbranch <- getCurrBranch 
	let withbranch a = a =<< getbranch

	remotes <- syncRemotes (syncWith o)
	let gitremotes = filter Remote.gitSyncableRemote remotes
	(exportremotes, dataremotes) <- partition (exportTree . Remote.config)
		. filter (\r -> Remote.uuid r /= NoUUID)
		<$> filterM (not <$$> liftIO . getDynamicConfig . remoteAnnexIgnore . Remote.gitconfig) remotes

	if cleanupOption o
		then do
			commandAction (withbranch cleanupLocal)
			mapM_ (commandAction . withbranch . cleanupRemote) gitremotes
		else do
			-- Syncing involves many actions, any of which
			-- can independently fail, without preventing
			-- the others from running. 
			-- These actions cannot be run concurrently.
			mapM_ includeCommandAction $ concat
				[ [ commit o ]
				, [ withbranch (mergeLocal mergeConfig (resolveMergeOverride o)) ]
				, map (withbranch . pullRemote o mergeConfig) gitremotes
				,  [ mergeAnnex ]
				]
			
			whenM shouldsynccontent $ do
				syncedcontent <- seekSyncContent o dataremotes
				exportedcontent <- seekExportContent exportremotes
				-- Transferring content can take a while,
				-- and other changes can be pushed to the
				-- git-annex branch on the remotes in the
				-- meantime, so pull and merge again to
				-- avoid our push overwriting those changes.
				when (syncedcontent || exportedcontent) $ do
					mapM_ includeCommandAction $ concat
						[ map (withbranch . pullRemote o mergeConfig) gitremotes
						, [ commitAnnex, mergeAnnex ]
						]
	
			void $ includeCommandAction $ withbranch pushLocal
			-- Pushes to remotes can run concurrently.
			mapM_ (commandAction . withbranch . pushRemote o) gitremotes
  where
	shouldsynccontent = pure (contentOption o)
		<||> pure (not (null (contentOfOption o)))
		<||> (pure (not (noContentOption o)) <&&> getGitConfigVal annexSyncContent)

type CurrBranch = (Maybe Git.Branch, Maybe Adjustment)

{- There may not be a branch checked out until after the commit,
 - or perhaps after it gets merged from the remote, or perhaps
 - never.
 -
 - So only look it up once it's needed, and once there is a
 - branch, cache it.
 -
 - When on an adjusted branch, gets the original branch, and the adjustment.
 -}
getCurrBranch :: Annex (Annex CurrBranch)
getCurrBranch = do
	mvar <- liftIO newEmptyMVar
	return $ ifM (liftIO $ isEmptyMVar mvar)
		( do
			currbranch <- inRepo Git.Branch.current
			case currbranch of
				Nothing -> return (Nothing, Nothing)
				Just b -> do
					let v = case adjustedToOriginal b of
						Nothing -> (Just b, Nothing)
						Just (adj, origbranch) ->
							(Just origbranch, Just adj)
					liftIO $ putMVar mvar v
					return v
		, liftIO $ readMVar mvar
		)

{- Merging may delete the current directory, so go to the top
 - of the repo. This also means that sync always acts on all files in the
 - repository, not just on a subdirectory. -}
prepMerge :: Annex ()
prepMerge = Annex.changeDirectory =<< fromRepo Git.repoPath

mergeConfig :: [Git.Merge.MergeConfig]	
mergeConfig = 
	[ Git.Merge.MergeNonInteractive
	-- In several situations, unrelated histories should be merged
	-- together. This includes pairing in the assistant, and merging
	-- from a remote into a newly created direct mode repo.
	-- (Once direct mode is removed, this could be changed, so only
	-- the assistant uses it.)
	, Git.Merge.MergeUnrelatedHistories
	]

merge :: CurrBranch -> [Git.Merge.MergeConfig] -> ResolveMergeOverride -> Git.Branch.CommitMode -> Git.Branch -> Annex Bool
merge currbranch mergeconfig resolvemergeoverride commitmode tomerge = case currbranch of
	(Just b, Just adj) -> updateAdjustedBranch tomerge (b, adj) mergeconfig canresolvemerge commitmode
	(b, _) -> autoMergeFrom tomerge b mergeconfig canresolvemerge commitmode
  where
	canresolvemerge = case resolvemergeoverride of
		ResolveMergeOverride True -> getGitConfigVal annexResolveMerge
		ResolveMergeOverride False -> return False

syncBranch :: Git.Branch -> Git.Branch
syncBranch = Git.Ref.underBase "refs/heads/synced" . fromDirectBranch . fromAdjustedBranch

remoteBranch :: Remote -> Git.Ref -> Git.Ref
remoteBranch remote = Git.Ref.underBase $ "refs/remotes/" ++ Remote.name remote

-- Do automatic initialization of remotes when possible when getting remote
-- list.
syncRemotes :: [String] -> Annex [Remote]
syncRemotes ps = do
	remotelist <- Remote.remoteList' True
	available <- filterM (liftIO . getDynamicConfig . remoteAnnexSync . Remote.gitconfig)
		(filter (not . Remote.isXMPPRemote) remotelist)
	syncRemotes' ps available

syncRemotes' :: [String] -> [Remote] -> Annex [Remote]
syncRemotes' ps available = 
	ifM (Annex.getState Annex.fast) ( nub <$> pickfast , wanted )
  where
	pickfast = (++) <$> listed <*> (filterM good (fastest available))
	
	wanted
		| null ps = filterM good (concat $ Remote.byCost available)
		| otherwise = listed
	
	listed = concat <$> mapM Remote.byNameOrGroup ps
	
	good r
		| Remote.gitSyncableRemote r = Remote.Git.repoAvail $ Remote.repo r
		| otherwise = return True
	
	fastest = fromMaybe [] . headMaybe . Remote.byCost

commit :: SyncOptions -> CommandStart
commit o = stopUnless shouldcommit $ next $ next $ do
	commitmessage <- maybe commitMsg return (messageOption o)
	showStart "commit" ""
	Annex.Branch.commit "update"
	ifM isDirect
		( do
			void stageDirect
			void preCommitDirect
			commitStaged Git.Branch.ManualCommit commitmessage
		, do
			showOutput
			void $ inRepo $ Git.Branch.commitCommand Git.Branch.ManualCommit
				[ Param "-a"
				, Param "-m"
				, Param commitmessage
				]
			return True
		)
  where
	shouldcommit = pure (commitOption o)
		<||> (pure (not (noCommitOption o)) <&&> getGitConfigVal annexAutoCommit)

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

mergeLocal :: [Git.Merge.MergeConfig] -> ResolveMergeOverride -> CurrBranch -> CommandStart
mergeLocal mergeconfig resolvemergeoverride currbranch@(Just _, _) =
	go =<< needMerge currbranch
  where
	go Nothing = stop
	go (Just syncbranch) = do
		showStart "merge" $ Git.Ref.describe syncbranch
		next $ next $ merge currbranch mergeconfig resolvemergeoverride Git.Branch.ManualCommit syncbranch
mergeLocal _ _ (Nothing, madj) = do
	b <- inRepo Git.Branch.currentUnsafe
	ifM (isJust <$> needMerge (b, madj))
		( do
			warning $ "There are no commits yet in the currently checked out branch, so cannot merge any remote changes into it."
			next $ next $ return False
		, stop
		)

-- Returns the branch that should be merged, if any.
needMerge :: CurrBranch -> Annex (Maybe Git.Branch)
needMerge (Nothing, _) = return Nothing
needMerge (Just branch, madj) = ifM (allM id checks)
	( return (Just syncbranch)
	, return Nothing
	)
  where
	checks =
		[ not <$> isBareRepo
		, inRepo (Git.Ref.exists syncbranch)
		, inRepo (Git.Branch.changed branch' syncbranch)
		]
	syncbranch = syncBranch branch
	branch' = maybe branch (adjBranch . originalToAdjusted branch) madj

pushLocal :: CurrBranch -> CommandStart
pushLocal b = do
	updateSyncBranch b
	stop

updateSyncBranch :: CurrBranch -> Annex ()
updateSyncBranch (Nothing, _) = noop
updateSyncBranch (Just branch, madj) = do
	-- When in an adjusted branch, propigate any changes made to it
	-- back to the original branch.
	maybe noop (propigateAdjustedCommits branch) madj
	-- Update the sync branch to match the new state of the branch
	inRepo $ updateBranch (syncBranch branch) branch
	-- In direct mode, we're operating on some special direct mode
	-- branch, rather than the intended branch, so update the intended
	-- branch.
	whenM isDirect $
		inRepo $ updateBranch (fromDirectBranch branch) branch

updateBranch :: Git.Branch -> Git.Branch -> Git.Repo -> IO ()
updateBranch syncbranch updateto g = 
	unlessM go $ giveup $ "failed to update " ++ Git.fromRef syncbranch
  where
	go = Git.Command.runBool
		[ Param "branch"
		, Param "-f"
		, Param $ Git.fromRef $ Git.Ref.base syncbranch
		, Param $ Git.fromRef $ updateto
		] g

pullRemote :: SyncOptions -> [Git.Merge.MergeConfig] -> Remote -> CurrBranch -> CommandStart
pullRemote o mergeconfig remote branch = stopUnless (pure $ pullOption o && wantpull) $ do
	showStart "pull" (Remote.name remote)
	next $ do
		showOutput
		stopUnless fetch $
			next $ mergeRemote remote branch mergeconfig (resolveMergeOverride o)
  where
	fetch = inRepoWithSshOptionsTo (Remote.repo remote) (Remote.gitconfig remote) $
		Git.Command.runBool
			[Param "fetch", Param $ Remote.name remote]
	wantpull = remoteAnnexPull (Remote.gitconfig remote)

{- The remote probably has both a master and a synced/master branch.
 - Which to merge from? Well, the master has whatever latest changes
 - were committed (or pushed changes, if this is a bare remote),
 - while the synced/master may have changes that some
 - other remote synced to this remote. So, merge them both. -}
mergeRemote :: Remote -> CurrBranch -> [Git.Merge.MergeConfig] -> ResolveMergeOverride -> CommandCleanup
mergeRemote remote currbranch mergeconfig resolvemergeoverride = ifM isBareRepo
	( return True
	, case currbranch of
		(Nothing, _) -> do
			branch <- inRepo Git.Branch.currentUnsafe
			mergelisted (pure (branchlist branch))
		(Just branch, _) -> do
			inRepo $ updateBranch (syncBranch branch) branch
			mergelisted (tomerge (branchlist (Just branch)))
	)
  where
	mergelisted getlist = and <$> 
		(mapM (merge currbranch mergeconfig resolvemergeoverride Git.Branch.ManualCommit . remoteBranch remote) =<< getlist)
	tomerge = filterM (changed remote)
	branchlist Nothing = []
	branchlist (Just branch) = [branch, syncBranch branch]

pushRemote :: SyncOptions -> Remote -> CurrBranch -> CommandStart
pushRemote _o _remote (Nothing, _) = stop
pushRemote o remote (Just branch, _) = stopUnless (pure (pushOption o) <&&> needpush) $ do
	showStart "push" (Remote.name remote)
	next $ next $ do
		showOutput
		ok <- inRepoWithSshOptionsTo (Remote.repo remote) gc $
			pushBranch remote branch
		if ok
			then postpushupdate
			else do
				warning $ unwords [ "Pushing to " ++ Remote.name remote ++ " failed." ]
				showLongNote "(non-fast-forward problems can be solved by setting receive.denyNonFastforwards to false in the remote's git config)"
				return ok
  where
	needpush
		| remoteAnnexReadOnly gc = return False
		| not (remoteAnnexPush gc) = return False
		| otherwise = anyM (newer remote) [syncBranch branch, Annex.Branch.name]
	-- Do updateInstead emulation for remotes on eg removable drives
	-- formatted FAT, where the post-update hook won't run.
	postpushupdate
		| annexCrippledFileSystem (remoteGitConfig (Remote.gitconfig remote)) = 
			case Git.repoWorkTree (Remote.repo remote) of
				Nothing -> return True
				Just wt -> ifM (Remote.Git.onLocal remote needUpdateInsteadEmulation)
					( liftIO $ do
						p <- readProgramFile
						boolSystem' p [Param "post-receive"]
							(\cp -> cp { cwd = Just wt })
					, return True
					)
		| otherwise = return True
	gc = Remote.gitconfig remote

{- Pushes a regular branch like master to a remote. Also pushes the git-annex
 - branch.
 -
 - If the remote is a bare git repository, it's best to push the regular
 - branch directly to it, so that cloning/pulling will get it.
 - On the other hand, if it's not bare, pushing to the checked out branch
 - will generally fail (except with receive.denyCurrentBranch=updateInstead),
 - and this is why we push to its syncBranch.
 -
 - Git offers no way to tell if a remote is bare or not, so both methods
 - are tried.
 -
 - The direct push is likely to spew an ugly error message, so its stderr is
 - often elided. Since git progress display goes to stderr too, the 
 - sync push is done first, and actually sends the data. Then the
 - direct push is tried, with stderr discarded, to update the branch ref
 - on the remote.
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
pushBranch :: Remote -> Git.Branch -> Git.Repo -> IO Bool
pushBranch remote branch g = directpush `after` annexpush `after` syncpush
  where
	syncpush = flip Git.Command.runBool g $ pushparams
		[ Git.Branch.forcePush $ refspec Annex.Branch.name
		, refspec $ fromAdjustedBranch branch
		]
	annexpush = void $ tryIO $ flip Git.Command.runQuiet g $ pushparams
		[ Git.fromRef $ Git.Ref.base $ Annex.Branch.name ]
	directpush = do
		-- Git prints out an error message when this fails.
		-- In the default configuration of receive.denyCurrentBranch,
		-- the error message mentions that config setting
		-- (and should even if it is localized), and is quite long,
		-- and the user was not intending to update the checked out
		-- branch, so in that case, avoid displaying the error
		-- message. Do display other error messages though,
		-- including the error displayed when
		-- receive.denyCurrentBranch=updateInstead -- the user
		-- will want to see that one.
		let p = flip Git.Command.gitCreateProcess g $ pushparams
			[ Git.fromRef $ Git.Ref.base $ fromDirectBranch $ fromAdjustedBranch branch ]
		(transcript, ok) <- processTranscript' p Nothing
		when (not ok && not ("denyCurrentBranch" `isInfixOf` transcript)) $
			hPutStr stderr transcript
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
 - Returns true if any file transfers were made.
 -
 - When concurrency is enabled, files are processed concurrently.
 -}
seekSyncContent :: SyncOptions -> [Remote] -> Annex Bool
seekSyncContent o rs = do
	mvar <- liftIO newEmptyMVar
	bloom <- case keyOptions o of
		Just WantAllKeys -> Just <$> genBloomFilter (seekworktree mvar [])
		_ -> do
			l <- workTreeItems (contentOfOption o)
			seekworktree mvar l (const noop)
			pure Nothing
	withKeyOptions' (keyOptions o) False
		(return (seekkeys mvar bloom))
		(const noop)
		[]
	finishCommandActions
	liftIO $ not <$> isEmptyMVar mvar
  where
	seekworktree mvar l bloomfeeder = seekHelper LsFiles.inRepo l >>=
		mapM_ (\f -> ifAnnexed f (go (Right bloomfeeder) mvar (AssociatedFile (Just f))) noop)
	seekkeys mvar bloom k _ = go (Left bloom) mvar (AssociatedFile Nothing) k
	go ebloom mvar af k = commandAction $ do
		whenM (syncFile ebloom rs af k) $
			void $ liftIO $ tryPutMVar mvar ()
		return Nothing

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
 - Returns True if any file transfers were made.
 -}
syncFile :: Either (Maybe (Bloom Key)) (Key -> Annex ()) -> [Remote] -> AssociatedFile -> Key -> Annex Bool
syncFile ebloom rs af k = onlyActionOn' k $ do
	locs <- Remote.keyLocations k
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
		handleDropsFrom locs' rs "unwanted" True k af []
			callCommandAction
	
	return (got || not (null putrs))
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
		showStart' "get" k (mkActionItem af)
		next $ next $ getKey' k af have

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
	put dest = includeCommandAction $ 
		Command.Move.toStart' dest False af k (mkActionItem af)

{- When a remote has an export-tracking branch, change the export to
 - follow the current content of the branch. Otherwise, transfer any files
 - that were part of an export but are not in the remote yet.
 - 
 - Returns True if any file transfers were made.
 -}
seekExportContent :: [Remote] -> Annex Bool
seekExportContent rs = or <$> forM rs go
  where
	go r = withExclusiveLock (gitAnnexExportLock (Remote.uuid r)) $ do
		db <- Export.openDb (Remote.uuid r)
		ea <- Remote.exportActions r
		exported <- case remoteAnnexExportTracking (Remote.gitconfig r) of
			Nothing -> getExport (Remote.uuid r)
			Just b -> do
				mcur <- inRepo $ Git.Ref.tree b
				case mcur of
					Nothing -> getExport (Remote.uuid r)
					Just cur -> do
						Command.Export.changeExport r ea db cur
						return [Exported cur []]
		Export.closeDb db `after` fillexport r ea db exported

	fillexport _ _ _ [] = return False
	fillexport r ea db (Exported { exportedTreeish = t }:[]) =
		Command.Export.fillExport r ea db t
	fillexport r _ _ _ = do
		warning $ "Export conflict detected. Different trees have been exported to " ++ 
			Remote.name r ++ 
			". Use git-annex export to resolve this conflict."
		return False

cleanupLocal :: CurrBranch -> CommandStart
cleanupLocal (Nothing, _) = stop
cleanupLocal (Just currb, _) = do
	showStart "cleanup" "local"
	next $ next $ do
		delbranch $ syncBranch currb
		delbranch $ syncBranch $ Git.Ref.base $ Annex.Branch.name
		mapM_ (\(s,r) -> inRepo $ Git.Ref.delete s r)
			=<< listTaggedBranches
		return True
  where
	delbranch b = whenM (inRepo $ Git.Ref.exists $ Git.Ref.branchRef b) $
		inRepo $ Git.Branch.delete b

cleanupRemote :: Remote -> CurrBranch -> CommandStart
cleanupRemote _ (Nothing, _) = stop
cleanupRemote remote (Just b, _) = do
	showStart "cleanup" (Remote.name remote)
	next $ next $
		inRepo $ Git.Command.runBool
			[ Param "push"
			, Param "--quiet"
			, Param "--delete"
			, Param $ Remote.name remote
			, Param $ Git.fromRef $ syncBranch b
			, Param $ Git.fromRef $ syncBranch $
				Git.Ref.base $ Annex.Branch.name
			]
