{- git-annex command
 -
 - Copyright 2011 Joachim Breitner <mail@joachim-breitner.de>
 - Copyright 2011-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Sync (
	cmd,
	seek,
	seek',
	CurrBranch,
	mergeConfig,
	merge,
	prepMerge,
	mergeLocal,
	mergeRemote,
	commitStaged,
	commitMsg,
	pushBranch,
	updateBranch,
	updateBranches,
	seekExportContent,
	optParser,
	parseUnrelatedHistoriesOption,
	SyncOptions(..),
	OperationMode(..),
) where

import Command
import qualified Annex
import qualified Annex.Branch
import qualified Remote
import qualified Types.Remote as Remote
import Annex.Hook
import qualified Git.Command
import qualified Git.LsFiles as LsFiles
import qualified Git.Branch
import qualified Git.Merge
import qualified Git.Types as Git
import qualified Git.Ref
import qualified Git
import Git.FilePath
import qualified Remote.Git
import Config
import Config.GitConfig
import Annex.SpecialRemote.Config
import Config.DynamicConfig
import Annex.Path
import Annex.Wanted
import Annex.Content
import Annex.WorkTree
import Command.Get (getKey')
import qualified Command.Move
import qualified Command.Export
import qualified Command.Import
import Annex.Drop
import Annex.UUID
import Logs.UUID
import Logs.Export
import Logs.PreferredContent
import Logs.View
import Annex.AutoMerge
import Annex.AdjustedBranch
import Annex.AdjustedBranch.Merge
import Annex.View
import Annex.Ssh
import Annex.BloomFilter
import Annex.UpdateInstead
import Annex.Export
import Annex.TaggedPush
import Annex.CurrentBranch
import Annex.Import
import Annex.CheckIgnore
import Types.FileMatcher
import Types.GitConfig
import qualified Database.Export as Export
import Utility.Bloom
import Utility.OptParse
import Utility.Process.Transcript
import Utility.Tuple

import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.ByteString as S
import Data.Char

cmd :: Command
cmd = withAnnexOptions [jobsOption, backendOption] $
	command "sync" SectionCommon 
		"synchronize local repository with remotes"
		(paramRepeating paramRemote) (seek <--< optParser SyncMode)

data OperationMode = SyncMode | PullMode | PushMode | SatisfyMode | AssistMode
	deriving (Eq, Show)

data SyncOptions = SyncOptions
	{ syncWith :: CmdParams
	, onlyAnnexOption :: Bool
	, notOnlyAnnexOption :: Bool
	, commitOption :: Bool
	, noCommitOption :: Bool
	, messageOption :: Maybe String
	, pullOption :: Bool
	, pushOption :: Bool
	, contentOption :: Maybe Bool
	, noContentOption :: Maybe Bool
	, contentOfOption :: [FilePath]
	, cleanupOption :: Bool
	, keyOptions :: Maybe KeyOptions
	, resolveMergeOverride :: Bool
	, allowUnrelatedHistories :: Bool
	, operationMode :: OperationMode
	}

instance Default SyncOptions where
	def = SyncOptions
		{ syncWith = []
		, onlyAnnexOption = False
		, notOnlyAnnexOption = False
		, commitOption = False
		, noCommitOption = False
		, messageOption = Nothing
		, pullOption = False
		, pushOption = False
		, contentOption = Just False
		, noContentOption = Just False
		, contentOfOption = []
		, cleanupOption = False
		, keyOptions = Nothing
		, resolveMergeOverride = False
		, allowUnrelatedHistories = False
		, operationMode = SyncMode
		}

optParser :: OperationMode -> CmdParamsDesc -> Parser SyncOptions
optParser mode desc = SyncOptions
	<$> (many $ argument str
		( metavar desc
		<> completeRemotes
		))
	<*> whenmode [SatisfyMode] True 
		(switch 
			( long "only-annex"
			<> short 'a'
			<> help "do not operate on git branches"
			))
	<*> whenmode [SatisfyMode] False 
		( switch 
			( long "not-only-annex"
			<> help "operate on git branches as well as annex"
			))
	<*> case mode of
		SyncMode -> switch
			( long "commit"
			<> help "commit changes to git"
			)
		PushMode -> pure False
		PullMode -> pure False
		SatisfyMode -> pure False
		AssistMode -> pure True
	<*> unlessmode [SyncMode] True 
		(switch
			( long "no-commit"
			<> help "avoid git commit" 
			))
	<*> unlessmode [SyncMode, AssistMode] Nothing
		(optional (strOption
			( long "message" <> short 'm' <> metavar "MSG"
			<> help "commit message"
			)))
	<*> case mode of
		SyncMode -> invertableSwitch "pull" True
			( help "avoid git pulls from remotes" 
			)
		PullMode -> pure True
		PushMode -> pure False
		SatisfyMode -> pure False
		AssistMode -> pure True
	<*> case mode of
		SyncMode -> invertableSwitch "push" True
			( help "avoid git pushes to remotes" 
			)
		PullMode -> pure False
		PushMode -> pure True
		SatisfyMode -> pure False
		AssistMode -> pure True
	<*> whenmode [SatisfyMode] (Just True)
		(optional (flag' True 
			( long "content"
			<> help "transfer annexed file contents" 
			)))
	<*> whenmode [SatisfyMode] Nothing
		(optional (flag' True
			( long "no-content"
			<> short 'g'
			<> help "do not transfer annexed file contents"
			)))
	<*> many (strOption
		( long "content-of"
		<> short 'C'
		<> help "transfer contents of annexed files in a given location"
		<> metavar paramPath
		))
	<*> whenmode [PullMode, SatisfyMode] False 
		(switch
			( long "cleanup"
			<> help "remove synced/ branches"
			))
	<*> optional parseAllOption
	<*> whenmode [PushMode, SatisfyMode] False 
		(invertableSwitch "resolvemerge" True
			( help "do not automatically resolve merge conflicts"
			))
	<*> whenmode [PushMode, SatisfyMode] False
		parseUnrelatedHistoriesOption
	<*> pure mode
  where
	whenmode m v a
		| mode `elem` m = pure v
		| otherwise = a
	unlessmode m v a
		| mode `elem` m = a
		| otherwise = pure v

parseUnrelatedHistoriesOption :: Parser Bool
parseUnrelatedHistoriesOption = 
	invertableSwitch "allow-unrelated-histories" False
		( help "allow merging unrelated histories"
		)

-- Since prepMerge changes the working directory, FilePath options
-- have to be adjusted.
instance DeferredParseClass SyncOptions where
	finishParse v = SyncOptions
		<$> pure (syncWith v)
		<*> pure (onlyAnnexOption v)
		<*> pure (notOnlyAnnexOption v)
		<*> pure (commitOption v)
		<*> pure (noCommitOption v)
		<*> pure (messageOption v)
		<*> pure (pullOption v)
		<*> pure (pushOption v)
		<*> pure (contentOption v)
		<*> pure (noContentOption v)
		<*> liftIO (mapM (fromRawFilePath <$$> absPath . toRawFilePath) (contentOfOption v))
		<*> pure (cleanupOption v)
		<*> pure (keyOptions v)
		<*> pure (resolveMergeOverride v)
		<*> pure (allowUnrelatedHistories v)
		<*> pure (operationMode v)

seek :: SyncOptions -> CommandSeek
seek o = do
	prepMerge
	
	seek' o

seek' :: SyncOptions -> CommandSeek
seek' o = startConcurrency transferStages $ do
	let withbranch a = a =<< getCurrentBranch

	remotes <- syncRemotes (syncWith o)
	warnSyncContentTransition o remotes
	-- Remotes that are git repositories, not (necesarily) special remotes.
	let gitremotes = filter (Remote.gitSyncableRemoteType . Remote.remotetype) remotes
	-- Remotes that contain annex object content.
	contentremotes <- filter (\r -> Remote.uuid r /= NoUUID)
		<$> filterM (not <$$> liftIO . getDynamicConfig . remoteAnnexIgnore . Remote.gitconfig) remotes

	if cleanupOption o
		then do
			commandAction (withbranch cleanupLocal)
			mapM_ (commandAction . withbranch . cleanupRemote) gitremotes
		else do
			mc <- mergeConfig (allowUnrelatedHistories o)

			-- Syncing involves many actions, any of which
			-- can independently fail, without preventing
			-- the others from running. 
			-- These actions cannot be run concurrently.
			mapM_ includeCommandAction $ concat
				[ [ commit o ]
				, [ withbranch (mergeLocal mc o) ]
				, map (withbranch . pullRemote o mc) gitremotes
				, [ mergeAnnex ]
				]
			
			content <- shouldSyncContent o

			forM_ (filter isImport contentremotes) $
				withbranch . importRemote content o
			forM_ (filter isThirdPartyPopulated contentremotes) $
				pullThirdPartyPopulated o
			
			when content $ do
				-- Send content to any exports before other
				-- repositories, in case that lets content
				-- be dropped from other repositories.
				exportedcontent <- withbranch $
					seekExportContent (Just o)
						(filter isExport contentremotes)

				-- Sync content with remotes, including
				-- importing from import remotes (since
				-- importing only downloads new files not
				-- old files)
				let shouldsynccontent r
					| isExport r && not (isImport r) = False
					| otherwise = True
				syncedcontent <- withbranch $
					seekSyncContent o
						(filter shouldsynccontent contentremotes)

				-- Transferring content can take a while,
				-- and other changes can be pushed to the
				-- git-annex branch on the remotes in the
				-- meantime, so pull and merge again to
				-- avoid our push overwriting those changes.
				when (syncedcontent || exportedcontent) $ do
					mapM_ includeCommandAction $ concat
						[ map (withbranch . pullRemote o mc) gitremotes
						, [ commitAnnex, mergeAnnex ]
						]
	
			void $ includeCommandAction $ withbranch $ pushLocal o
			-- Pushes to remotes can run concurrently.
			mapM_ (commandAction . withbranch . pushRemote o) gitremotes

{- Merging may delete the current directory, so go to the top
 - of the repo. This also means that sync always acts on all files in the
 - repository, not just on a subdirectory. -}
prepMerge :: Annex ()
prepMerge = Annex.changeDirectory . fromRawFilePath =<< fromRepo Git.repoPath

mergeConfig :: Bool -> Annex [Git.Merge.MergeConfig]
mergeConfig mergeunrelated = do
	quiet <- commandProgressDisabled
	return $ catMaybes
		[ Just Git.Merge.MergeNonInteractive
		, if mergeunrelated 
			then Just Git.Merge.MergeUnrelatedHistories
			else Nothing
		, if quiet
			then Just Git.Merge.MergeQuiet
			else Nothing
		]

merge :: CurrBranch -> [Git.Merge.MergeConfig] -> SyncOptions -> Git.Branch.CommitMode -> [Git.Branch] -> Annex Bool
merge currbranch mergeconfig o commitmode tomergel = do
	canresolvemerge <- if resolveMergeOverride o
		then getGitConfigVal annexResolveMerge
		else return False
	and <$> case currbranch of
		(Just b, Just adj) -> forM tomergel $ \tomerge ->
			mergeToAdjustedBranch tomerge (b, adj) mergeconfig canresolvemerge commitmode
		(b, _) -> forM tomergel $ \tomerge ->
			autoMergeFrom tomerge b mergeconfig commitmode canresolvemerge

syncBranch :: Git.Branch -> Git.Branch
syncBranch = Git.Ref.underBase "refs/heads/synced" . origBranch

origBranch :: Git.Branch -> Git.Branch
origBranch = fromViewBranch . fromAdjustedBranch

remoteBranch :: Remote -> Git.Ref -> Git.Ref
remoteBranch remote = Git.Ref.underBase $ "refs/remotes/" ++ Remote.name remote

syncRemotes :: [String] -> Annex [Remote]
syncRemotes ps = do
	-- Do automatic initialization of remotes when possible
	-- when getting remote list.
	remotelist <- Remote.remoteList' True
	available <- filterM (liftIO . getDynamicConfig . remoteAnnexSync . Remote.gitconfig) remotelist
	syncRemotes' ps available

syncRemotes' :: [String] -> [Remote] -> Annex [Remote]
syncRemotes' ps available = 
	ifM (Annex.getRead Annex.fast) ( fastest <$> wanted , wanted )
  where
	wanted
		| null ps = filterM good (concat $ Remote.byCost available)
		| otherwise = listed
	
	listed = concat <$> mapM Remote.byNameOrGroup ps
	
	good r
		| Remote.gitSyncableRemoteType (Remote.remotetype r) =
			Remote.Git.repoAvail =<< Remote.getRepo r
		| otherwise = return True
	
	fastest = fromMaybe [] . headMaybe . Remote.byCost

commit :: SyncOptions -> CommandStart
commit o = stopUnless shouldcommit $ starting "commit" ai si $ do
	commitmessage <- maybe commitMsg return (messageOption o)
	Annex.Branch.commit =<< Annex.Branch.commitMessage
	next $ do
		showOutput
		let cmode = Git.Branch.ManualCommit
		cquiet <- Git.Branch.CommitQuiet <$> commandProgressDisabled
		void $ inRepo $ Git.Branch.commitCommand cmode cquiet
			[ Param "-a"
			, Param "-m"
			, Param commitmessage
			]
		return True
  where
	shouldcommit = notOnlyAnnex o <&&>
		( pure (commitOption o)
		<||> (pure (not (noCommitOption o)) <&&> getGitConfigVal annexAutoCommit)
		)
	ai = ActionItemOther Nothing
	si = SeekInput []

commitMsg :: Annex String
commitMsg = do
	u <- getUUID
	m <- uuidDescMap
	return $ "git-annex in " ++ maybe "unknown" fromUUIDDesc (M.lookup u m)

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

mergeLocal :: [Git.Merge.MergeConfig] -> SyncOptions -> CurrBranch -> CommandStart
mergeLocal mergeconfig o currbranch = stopUnless (notOnlyAnnex o) $
	mergeLocal' mergeconfig o currbranch

mergeLocal' :: [Git.Merge.MergeConfig] -> SyncOptions -> CurrBranch -> CommandStart
mergeLocal' mergeconfig o currbranch@(Just branch, _) =
	needMerge currbranch branch >>= \case
		[] -> stop
		tomerge -> do
			let ai = ActionItemOther (Just $ UnquotedString $ unwords $ map Git.Ref.describe tomerge)
			let si = SeekInput []
			starting "merge" ai si $
				next $ merge currbranch mergeconfig o Git.Branch.ManualCommit tomerge
mergeLocal' _ _ currbranch@(Nothing, _) = inRepo Git.Branch.currentUnsafe >>= \case
	Just branch -> needMerge currbranch branch >>= \case
		[] -> stop
		tomerge -> do
			let ai = ActionItemOther (Just $ UnquotedString $ unwords $ map Git.Ref.describe tomerge)
			let si = SeekInput []
			starting "merge" ai si $ do
				warning $ UnquotedString $ "There are no commits yet to branch " ++ Git.fromRef branch ++ ", so cannot merge " ++ unwords (map Git.fromRef tomerge) ++ " into it."
				next $ return False
	Nothing -> stop

-- Returns the branches that should be merged, if any.
--
-- Usually this is the sync branch. However, when in an adjusted branch,
-- it can be either the sync branch or the original branch, or both.
needMerge :: CurrBranch -> Git.Branch -> Annex [Git.Branch]
needMerge currbranch headbranch
	| is_branchView headbranch = return []
	| otherwise = ifM isBareRepo
		( return []
		, do
			syncbranchret <- usewhen syncbranch syncbranchchecks
			adjbranchret <- case currbranch of
				(Just origbranch, Just adj) -> 
					usewhen origbranch $
						canMergeToAdjustedBranch origbranch (origbranch, adj)
				_ -> return []
			return (syncbranchret++adjbranchret)
		)
  where
	usewhen v c = ifM c
		( return [v]
		, return []
		)
	syncbranch = syncBranch headbranch
	syncbranchchecks = case currbranch of
		(Just _, madj) -> syncbranchchanged madj
		(Nothing, _) -> hassyncbranch
	hassyncbranch = inRepo (Git.Ref.exists syncbranch)
	syncbranchchanged madj =
		let branch' = maybe headbranch (adjBranch . originalToAdjusted headbranch) madj
		in hassyncbranch <&&> inRepo (Git.Branch.changed branch' syncbranch)

pushLocal :: SyncOptions -> CurrBranch -> CommandStart
pushLocal o b = stopUnless (notOnlyAnnex o) $ do
	updateBranches b
	stop

updateBranches :: CurrBranch -> Annex ()
updateBranches (Nothing, _) = noop
updateBranches (Just branch, madj) = do
	-- When in a view branch, update it to reflect any changes
	-- of its parent branch or the metadata.
	currentView >>= \case
		Just (view, madj') -> updateView view madj' >>= \case
			Nothing -> noop
			Just newcommit -> do
				ok <- inRepo $ Git.Command.runBool
					[ Param "merge"
					, Param (Git.fromRef newcommit)
					]
				unless ok $
					giveup $ "failed to update view"
				case madj' of
					Nothing -> noop
					Just adj -> updateadjustedbranch adj
		-- When in an adjusted branch, propagate any changes
		-- made to it back to the original branch.
		Nothing -> case madj of
			Just adj -> do
				propigateAdjustedCommits branch adj
				updateadjustedbranch adj
			Nothing -> noop
	
	-- Update the sync branch to match the new state of the branch
	inRepo $ updateBranch (syncBranch branch) (fromViewBranch branch)
  where
	-- The adjusted branch may also need to be updated, if the adjustment
	-- is not stable, and the usual configuration does not update it.
	updateadjustedbranch adj = unless (adjustmentIsStable adj) $
		annexAdjustedBranchRefresh <$> Annex.getGitConfig >>= \case
			0 -> adjustedBranchRefreshFull adj branch
			_ -> return ()

updateBranch :: Git.Branch -> Git.Branch -> Git.Repo -> IO ()
updateBranch syncbranch updateto g = 
	unlessM go $
		giveup $ "failed to update " ++ Git.fromRef syncbranch
  where
	go = Git.Command.runBool
		[ Param "branch"
		, Param "-f"
		, Param $ Git.fromRef $ Git.Ref.base syncbranch
		, Param $ Git.fromRef $ updateto
		] g

pullRemote :: SyncOptions -> [Git.Merge.MergeConfig] -> Remote -> CurrBranch -> CommandStart
pullRemote o mergeconfig remote branch = stopUnless (pure $ pullOption o && wantpull) $
	starting "pull" ai si $ do
		showOutput
		ifM (onlyAnnex o)
			( do
				void $ fetch $ map Git.fromRef 
					[ Annex.Branch.name
					, syncBranch $ Annex.Branch.name
					]
				next $ return True
			, ifM (fetch [])
				( next $ mergeRemote remote branch mergeconfig o
				, next $ return True
				)
			)
  where
	fetch bs = do
		repo <- Remote.getRepo remote
		ms <- Annex.getState Annex.output
		inRepoWithSshOptionsTo repo (Remote.gitconfig remote) $
			Git.Command.runBool $ catMaybes
				[ Just $ Param "fetch"
				, if commandProgressDisabled' ms
					then Just $ Param "--quiet"
					else Nothing
				, Just $ Param $ Remote.name remote
				] ++ map Param bs
	wantpull = remoteAnnexPull (Remote.gitconfig remote)
	ai = ActionItemOther (Just (UnquotedString (Remote.name remote)))
	si = SeekInput []

importRemote :: Bool -> SyncOptions -> Remote -> CurrBranch -> CommandSeek
importRemote importcontent o remote currbranch
	| not (pullOption o) || not wantpull = noop
	| otherwise = case remoteAnnexTrackingBranch (Remote.gitconfig remote) of
		Nothing -> noop
		Just b -> do
			let (branch, subdir) = splitRemoteAnnexTrackingBranchSubdir b
			if canImportKeys remote importcontent
				then do
					Command.Import.seekRemote remote branch subdir importcontent (CheckGitIgnore True)
					-- Importing generates a branch
					-- that is not initially connected
					-- to the current branch, so allow
					-- merging unrelated histories when
					-- mergeing it.
					mc <- mergeConfig True
					void $ mergeRemote remote currbranch mc o
				else warning $ UnquotedString $ "Cannot import from " ++ Remote.name remote ++ " when not syncing content."
  where
	wantpull = remoteAnnexPull (Remote.gitconfig remote)

{- Handle a remote that is populated by a third party, by listing
 - the contents of the remote, and then adding only the files on it that
 - importKey identifies to a tree. The tree is only used to keep track
 - of where keys are located on the remote, no remote tracking branch is
 - updated, because the filenames are the names of annex object files,
 - not suitable for a tracking branch. Does not transfer any content. -}
pullThirdPartyPopulated :: SyncOptions -> Remote -> CommandSeek
pullThirdPartyPopulated o remote
	| not (pullOption o) || not wantpull = noop
	| not (canImportKeys remote False) = noop
	| otherwise = void $ includeCommandAction $ starting "list" ai si $
		Command.Import.listContents' remote ImportTree (CheckGitIgnore False) go
  where
	go (Just importable) = importChanges remote ImportTree False True importable >>= \case
		ImportFinished imported -> do
			(_t, updatestate) <- recordImportTree remote ImportTree imported
			next $ do
				updatestate
				return True
		ImportUnfinished -> next $ return False
	go Nothing = next $ return True -- unchanged from before

	ai = ActionItemOther (Just (UnquotedString (Remote.name remote)))
	si = SeekInput []
	
	wantpull = remoteAnnexPull (Remote.gitconfig remote)

{- The remote probably has both a master and a synced/master branch.
 - Which to merge from? Well, the master has whatever latest changes
 - were committed (or pushed changes, if this is a bare remote),
 - while the synced/master may have changes that some
 - other remote synced to this remote. So, merge them both. -}
mergeRemote :: Remote -> CurrBranch -> [Git.Merge.MergeConfig] -> SyncOptions -> CommandCleanup
mergeRemote remote currbranch mergeconfig o = ifM isBareRepo
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
	mergelisted getlist =
		merge currbranch mergeconfig o Git.Branch.ManualCommit
			=<< map (remoteBranch remote) <$> getlist
	tomerge = filterM (changed remote)
	branchlist Nothing = []
	branchlist (Just branch)
		| is_branchView branch = []
		| otherwise = [origBranch branch, syncBranch branch]

pushRemote :: SyncOptions -> Remote -> CurrBranch -> CommandStart
pushRemote _o _remote (Nothing, _) = stop
pushRemote o remote (Just branch, _) = do
	onlyannex <- onlyAnnex o
	let mainbranch = if onlyannex then Nothing else Just branch
	stopUnless (pure (pushOption o) <&&> needpush mainbranch) $
		starting "push" ai si $ next $ do
			repo <- Remote.getRepo remote
			showOutput
			ms <- Annex.getState Annex.output
			ok <- inRepoWithSshOptionsTo repo gc $
				pushBranch remote mainbranch ms
			if ok
				then postpushupdate repo
				else do
					warning $ UnquotedString $ unwords [ "Pushing to " ++ Remote.name remote ++ " failed." ]
					return ok
  where
	ai = ActionItemOther (Just (UnquotedString (Remote.name remote)))
	si = SeekInput []
	gc = Remote.gitconfig remote
	needpush mainbranch
		| remoteAnnexReadOnly gc = return False
		| not (remoteAnnexPush gc) = return False
		| otherwise = anyM (newer remote) $ catMaybes
			[ syncBranch <$> mainbranch
			, Just (Annex.Branch.name)
			]
	-- Older remotes on crippled filesystems may not have a
	-- post-receive hook set up, so when updateInstead emulation
	-- is needed, run post-receive manually.
	postpushupdate repo = case Git.repoWorkTree repo of
		Nothing -> return True
		Just wt -> ifM needemulation
			( gitAnnexChildProcess "post-receive" []
				(\cp -> cp { cwd = Just (fromRawFilePath wt) })
				(\_ _ _ pid -> waitForProcess pid >>= return . \case
					ExitSuccess -> True
					_ -> False
				)
			, return True
			)
	  where
		needemulation = Remote.Git.onLocalRepo repo $
			(annexCrippledFileSystem <$> Annex.getGitConfig)
				<&&>
			needUpdateInsteadEmulation			

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
 - The sync push first sends the synced/master branch,
 - and then forces the update of the remote synced/git-annex branch.
 -
 - Since some providers like github may treat the first branch sent
 - as the default branch, it's better to make that be synced/master than
 - synced/git-annex. (Although neither is ideal, it's the best that
 - can be managed given the constraints on order.)
 -
 - The forcing is necessary if a transition has rewritten the git-annex branch.
 - Normally any changes to the git-annex branch get pulled and merged before
 - this push, so this forcing is unlikely to overwrite new data pushed
 - in from another repository that is also syncing.
 -
 - But overwriting of data on synced/git-annex can happen, in a race.
 - The only difference caused by using a forced push in that case is that
 - the last repository to push wins the race, rather than the first to push.
 -}
pushBranch :: Remote -> Maybe Git.Branch -> MessageState -> Git.Repo -> IO Bool
pushBranch remote mbranch ms g = directpush `after` annexpush `after` syncpush
  where
	syncpush = flip Git.Command.runBool g $ pushparams $ catMaybes
		[ (refspec . origBranch) <$> mbranch
		, Just $ Git.Branch.forcePush $ refspec Annex.Branch.name
		]
	annexpush = void $ tryIO $ flip Git.Command.runQuiet g $ pushparams
		[ Git.fromRef $ Git.Ref.base $ Annex.Branch.name ]
	directpush = case mbranch of
		Nothing -> noop
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
		Just branch -> do
			let p = flip Git.Command.gitCreateProcess g $ pushparams
				[ Git.fromRef $ Git.Ref.base $ origBranch branch ]
			(transcript, ok) <- processTranscript' p Nothing
			when (not ok && not ("denyCurrentBranch" `isInfixOf` transcript)) $
				hPutStr stderr transcript
	pushparams branches = catMaybes
		[ Just $ Param "push"
		, if commandProgressDisabled' ms
			then Just $ Param "--quiet"
			else Nothing
		, Just $ Param $ Remote.name remote
		] ++ map Param branches
	refspec b = concat 
		[ Git.fromRef $ Git.Ref.base b
		,  ":"
		, Git.fromRef $ Git.Ref.base $ syncBranch b
		]

commitAnnex :: CommandStart
commitAnnex = do
	Annex.Branch.commit =<< Annex.Branch.commitMessage
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

{- Without --all, only looks at files in the work tree.
 - (Or, when in an adjusted branch where some files are hidden, at files in
 - the original branch.)
 -
 - With --all, when preferred content expressions look at filenames,
 - makes a first pass over the files in the work tree so those preferred
 - content expressions will match. The second pass is over all keys,
 - and only preferred content expressions that don't look at filenames
 - will match.
 -
 - Returns true if any file transfers were made.
 -
 - When concurrency is enabled, files are processed concurrently.
 -}
seekSyncContent :: SyncOptions -> [Remote] -> CurrBranch -> Annex Bool
seekSyncContent _ [] _ = return False
seekSyncContent o rs currbranch = do
	mvar <- liftIO newEmptyMVar
	bloom <- case keyOptions o of
		Just WantAllKeys -> ifM preferredcontentmatchesfilenames
			( Just <$> genBloomFilter (seekworktree mvar (WorkTreeItems []))
			, pure Nothing
			)
		_ -> case currbranch of
                	(Just origbranch, Just adj) | adjustmentHidesFiles adj -> do
				l <- workTreeItems' (AllowHidden True) ww (contentOfOption o)
				seekincludinghidden origbranch mvar l (const noop)
				pure Nothing
			_ -> do
				l <- workTreeItems ww (contentOfOption o)
				seekworktree mvar l (const noop)
				pure Nothing
	waitForAllRunningCommandActions
	withKeyOptions' (keyOptions o) False
		(return (const (commandAction . gokey mvar bloom)))
		(const noop)
		(WorkTreeItems [])
	waitForAllRunningCommandActions
	liftIO $ not <$> isEmptyMVar mvar
  where
	seekworktree mvar l bloomfeeder = do
		let seeker = AnnexedFileSeeker
			{ startAction = gofile bloomfeeder mvar
			, checkContentPresent = Nothing
			, usesLocationLog = True
			}
		seekFilteredKeys seeker $
			seekHelper fst3 ww LsFiles.inRepoDetails l

	seekincludinghidden origbranch mvar l bloomfeeder =
		let filterer = \(si, f) -> lookupKey f >>= \case
			Just k -> (commandAction $ gofile bloomfeeder mvar si f k)
			Nothing -> noop
		in seekFiltered (const (pure True)) filterer $
			seekHelper id ww (LsFiles.inRepoOrBranch origbranch) l 

	ww = WarnUnmatchLsFiles $
		case operationMode o of
			SyncMode -> "sync"
			PullMode -> "pull"
			PushMode -> "push"
			SatisfyMode -> "satisfy"
			AssistMode -> "assist"

	gofile bloom mvar _ f k = 
		go (Right bloom) mvar (AssociatedFile (Just f)) k
	
	gokey mvar bloom (_, k, _) =
		go (Left bloom) mvar (AssociatedFile Nothing) k

	go ebloom mvar af k = do
		let ai = OnlyActionOn k (ActionItemKey k)
		startingNoMessage ai $ do
			whenM (syncFile o ebloom rs af k) $
				void $ liftIO $ tryPutMVar mvar ()
			next $ return True

	preferredcontentmatchesfilenames =
		preferredcontentmatchesfilenames' Nothing
		<||> anyM (preferredcontentmatchesfilenames' . Just . Remote.uuid) rs
	preferredcontentmatchesfilenames' =
		introspectPreferredRequiredContent matchNeedsFileName

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
syncFile :: SyncOptions -> Either (Maybe (Bloom Key)) (Key -> Annex ()) -> [Remote] -> AssociatedFile -> Key -> Annex Bool
syncFile o ebloom rs af k = do
	inhere <- inAnnex k
	locs <- map Remote.uuid <$> Remote.keyPossibilities k
	let (have, lack) = partition (\r -> Remote.uuid r `elem` locs) rs

	got <- anyM id =<< handleget have inhere
	let inhere' = inhere || got
	putrs <- handleput lack inhere'

	u <- getUUID
	let locs' = concat [if inhere' then [u] else [], putrs, locs]

	-- To handle --all, a bloom filter is populated with all the keys
	-- of files in the working tree in the first pass. On the second
	-- pass, avoid dropping keys that were seen in the first pass, which
	-- would happen otherwise when preferred content matches on the
	-- filename, which is not available in the second pass.
	-- (When the preferred content expressions do not match on
	-- filenames, the first pass is skipped for speed.)
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
		handleDropsFrom locs' rs "unwanted" True k af si []
			callCommandAction
	
	return (got || not (null putrs))
  where
	wantget have inhere = allM id 
		[ pure (pullOption o || operationMode o == SatisfyMode)
		, pure (not $ null have)
		, pure (not inhere)
		, wantGet True (Just k) af
		]
	handleget have inhere = ifM (wantget have inhere)
		( return [ get have ]
		, return []
		)
	get have = includeCommandAction $ starting "get" ai si $
		stopUnless (getKey' k af have) $
			next $ return True

	wantput r
		| pushOption o == False && operationMode o /= SatisfyMode = return False
		| Remote.readonly r || remoteAnnexReadOnly (Remote.gitconfig r) = return False
		| isExport r = return False
		| isThirdPartyPopulated r = return False
		| otherwise = wantGetBy True (Just k) af (Remote.uuid r)
	handleput lack inhere
		| inhere = catMaybes <$>
			( forM lack $ \r ->
				ifM (wantput r <&&> put r)
					( return (Just (Remote.uuid r))
					, return Nothing
					)
			)
		| otherwise = return []
	put dest = includeCommandAction $ 
		Command.Move.toStart' dest Command.Move.RemoveNever af k ai si

	ai = mkActionItem (k, af)
	si = SeekInput []

{- When a remote has an annex-tracking-branch configuration, and that branch
 - is currently checked out, change the export to contain the current content
 - of the branch. (If the branch is not currently checked out, anything
 - imported from the remote will not yet have been merged into it yet and
 - so exporting would delete files from the remote unexpectedly.)
 - (This is not done in SatifyMode.)
 -
 - Otherwise, transfer any files that were part of a previous export
 - but are not in the remote yet.
 - 
 - Returns True if any file transfers were made.
 -}
seekExportContent :: Maybe SyncOptions -> [Remote] -> CurrBranch -> Annex Bool
seekExportContent o rs (currbranch, _) = or <$> forM rs go
  where
	go r
		| maybe False (\o' -> operationMode o' == SatisfyMode) o =
			case remoteAnnexTrackingBranch (Remote.gitconfig r) of
				Nothing -> return False
				Just b -> withdb r $ \db ->
					cannotupdateexport r db (Just b) False
		| not (maybe True pushOption o) = return False
		| not (remoteAnnexPush (Remote.gitconfig r)) = return False
		| otherwise = withdb r (go' r)
	go' r db = case remoteAnnexTrackingBranch (Remote.gitconfig r) of
		Nothing -> cannotupdateexport r db Nothing True
		Just b -> do
			mtree <- inRepo $ Git.Ref.tree b
			let addsubdir = case snd (splitRemoteAnnexTrackingBranchSubdir b) of
				Just subdir -> \cb -> Git.Ref $
					Git.fromRef' cb  <> ":" <> getTopFilePath subdir
				Nothing -> id
			mcurrtree <- maybe (pure Nothing)
				(inRepo . Git.Ref.tree . addsubdir)
				currbranch
			mtbcommitsha <- Command.Export.getExportCommit r b
			case (mtree, mcurrtree, mtbcommitsha) of
				(Just tree, Just currtree, Just _)
					| tree == currtree -> do
						filteredtree <- Command.Export.filterExport r tree
						Command.Export.changeExport r db filteredtree
						Command.Export.fillExport r db filteredtree mtbcommitsha
					| otherwise -> cannotupdateexport r db (Just b) False
				_ -> cannotupdateexport r db (Just b) True
	
	withdb r a = bracket
		(Export.openDb (Remote.uuid r))
		Export.closeDb
		(\db -> Export.writeLockDbWhile db (a db))
	
	cannotupdateexport r db mtb showwarning = do
		exported <- getExport (Remote.uuid r)
		when showwarning $
			maybe noop (warncannotupdateexport r mtb exported) currbranch
		fillexistingexport r db (exportedTreeishes exported) Nothing
	
	warncannotupdateexport r mtb exported currb = case mtb of
		Nothing -> inRepo (Git.Ref.tree currb) >>= \case
			Just currt | not (any (== currt) (exportedTreeishes exported)) ->
				showLongNote $ UnquotedString $ unwords
					[ notupdating
					, "to reflect changes to the tree, because export"
					, "tracking is not enabled. "
					, "(Set " ++ gitconfig ++ " to enable it.)"
					]
			_ -> noop
		Just b -> showLongNote $ UnquotedString $ unwords
			[ notupdating
			, "because " ++ Git.fromRef b ++ " does not exist."
			, "(As configured by " ++ gitconfig ++ ")"
			]
	  where
		gitconfig = show (remoteAnnexConfig r "tracking-branch")
		notupdating = "Not updating export to " ++ Remote.name r

	fillexistingexport _ _ [] _ = return False
	fillexistingexport r db (tree:[]) mtbcommitsha = do
		-- The tree was already filtered when it was exported, so
		-- does not need be be filtered again now, when we're only
		-- filling in any files that did not get transferred
		-- to the existing exported tree.
		let filteredtree = Command.Export.ExportFiltered tree
		Command.Export.fillExport r db filteredtree mtbcommitsha
	fillexistingexport r _ _ _ = do
		warnExportImportConflict r
		return False

cleanupLocal :: CurrBranch -> CommandStart
cleanupLocal (Nothing, _) = stop
cleanupLocal (Just currb, _) = starting "cleanup" ai si $ next $ do
	delbranch $ syncBranch currb
	delbranch $ syncBranch $ Git.Ref.base $ Annex.Branch.name
	mapM_ (\(s,r) -> inRepo $ Git.Ref.delete s r) =<< listTaggedBranches
	return True
  where
	delbranch b = whenM (inRepo $ Git.Ref.exists $ Git.Ref.branchRef b) $
		inRepo $ Git.Branch.delete b
	ai = ActionItemOther (Just "local")
	si = SeekInput []

cleanupRemote :: Remote -> CurrBranch -> CommandStart
cleanupRemote _ (Nothing, _) = stop
cleanupRemote remote (Just b, _) =
	starting "cleanup" ai si $
		next $ inRepo $ Git.Command.runBool
			[ Param "push"
			, Param "--quiet"
			, Param "--delete"
			, Param $ Remote.name remote
			, Param $ Git.fromRef $ syncBranch b
			, Param $ Git.fromRef $ syncBranch $
				Git.Ref.base $ Annex.Branch.name
			]
  where
	ai = ActionItemOther (Just (UnquotedString (Remote.name remote)))
	si = SeekInput []

shouldSyncContent :: SyncOptions -> Annex Bool
shouldSyncContent o
	| fromMaybe False (noContentOption o) = pure False
	| operationMode o == SatisfyMode = pure True
	-- For git-annex pull and git-annex push and git-annex assist,
	-- annex.syncontent defaults to True unless set
	| operationMode o /= SyncMode = annexsynccontent True
	| fromMaybe False (contentOption o) || not (null (contentOfOption o)) = pure True
	-- For git-annex sync, 
	-- annex.syncontent defaults to False unless set
	| otherwise = annexsynccontent False <||> onlyAnnex o
  where
	annexsynccontent d = 
		getGitConfigVal' annexSyncContent >>= \case
			HasGlobalConfig (Just c) -> return c
			HasGitConfig (Just c) -> return c
			_ -> return d

-- See doc/todo/finish_sync_content_transition.mdwn
warnSyncContentTransition :: SyncOptions -> [Remote] -> Annex ()
warnSyncContentTransition o remotes
	| operationMode o /= SyncMode = noop
	| isJust (noContentOption o) || isJust (contentOption o) = noop
	| not (null (contentOfOption o)) = noop
	| otherwise = getGitConfigVal' annexSyncContent >>= \case
		HasGlobalConfig (Just _) -> noop
		HasGitConfig (Just _) -> noop
		_ -> do
			m <- preferredContentMap
			hereu <- getUUID
			when (any (`M.member` m) (hereu:map Remote.uuid remotes)) $
				showwarning
  where
	showwarning = earlyWarning $
		"git-annex sync will change default behavior in the future to"
		<> " send content to repositories that have"
		<> " preferred content configured. If you do not want this to"
		<> " send any content, use --no-content (or -g)"
		<> " to prepare for that change."
		<> " (Or you can configure annex.synccontent)"

notOnlyAnnex :: SyncOptions -> Annex Bool
notOnlyAnnex o = not <$> onlyAnnex o

onlyAnnex :: SyncOptions -> Annex Bool
onlyAnnex o
	| notOnlyAnnexOption o = pure False
	| onlyAnnexOption o = pure True
	| otherwise = getGitConfigVal annexSyncOnlyAnnex
	
isExport :: Remote -> Bool
isExport = exportTree . Remote.config

isImport :: Remote -> Bool
isImport = importTree . Remote.config

isThirdPartyPopulated :: Remote -> Bool
isThirdPartyPopulated = Remote.thirdPartyPopulated . Remote.remotetype

splitRemoteAnnexTrackingBranchSubdir :: Git.Ref -> (Git.Ref, Maybe TopFilePath)
splitRemoteAnnexTrackingBranchSubdir tb = (branch, subdir)
  where
	(b, p) = separate' (== (fromIntegral (ord ':'))) (Git.fromRef' tb)
	branch = Git.Ref b
	subdir = if S.null p
		then Nothing
		else Just (asTopFilePath p)
