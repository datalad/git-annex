{- git-annex command
 -
 - Copyright 2011 Joachim Breitner <mail@joachim-breitner.de>
 - Copyright 2011,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Sync where

import Common.Annex
import Command
import qualified Remote
import qualified Annex
import qualified Annex.Branch
import qualified Annex.Queue
import Annex.Direct
import Annex.CatFile
import Annex.Link
import qualified Git.Command
import qualified Git.LsFiles as LsFiles
import qualified Git.Merge
import qualified Git.Branch
import qualified Git.Ref
import qualified Git
import Git.Types (BlobType(..))
import qualified Types.Remote
import qualified Remote.Git
import Types.Key
import Config
import Annex.ReplaceFile
import Git.FileMode

import Data.Hash.MD5

def :: [Command]
def = [command "sync" (paramOptional (paramRepeating paramRemote))
	[seek] SectionCommon "synchronize local repository with remotes"]

-- syncing involves several operations, any of which can independently fail
seek :: CommandSeek
seek rs = do
	prepMerge
	branch <- fromMaybe nobranch <$> inRepo Git.Branch.current
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

{- Merging may delete the current directory, so go to the top
 - of the repo. -}
prepMerge :: Annex ()
prepMerge = liftIO . setCurrentDirectory =<< fromRepo Git.repoPath

syncBranch :: Git.Ref -> Git.Ref
syncBranch = Git.Ref.under "refs/heads/synced/"

remoteBranch :: Remote -> Git.Ref -> Git.Ref
remoteBranch remote = Git.Ref.under $ "refs/remotes/" ++ Remote.name remote

syncRemotes :: [String] -> Annex [Remote]
syncRemotes rs = ifM (Annex.getState Annex.fast) ( nub <$> pickfast , wanted )
  where
	pickfast = (++) <$> listed <*> (good =<< fastest <$> available)
	wanted
		| null rs = good =<< concat . Remote.byCost <$> available
		| otherwise = listed
	listed = do
		l <- catMaybes <$> mapM (Remote.byName . Just) rs
		let s = filter (not . Remote.syncableRemote) l
		unless (null s) $
			error $ "cannot sync special remotes: " ++
				unwords (map Types.Remote.name s)
		return l
	available = filter Remote.syncableRemote
		. filter (remoteAnnexSync . Types.Remote.gitconfig)
		<$> Remote.remoteList
	good = filterM $ Remote.Git.repoAvail . Types.Remote.repo
	fastest = fromMaybe [] . headMaybe . Remote.byCost

commit :: CommandStart
commit = next $ next $ do
	ifM isDirect
		( do
			void $ stageDirect
			runcommit []
		, runcommit [Param "-a"]
		)
  where
	runcommit ps = do
		showStart "commit" ""
		showOutput
		Annex.Branch.commit "update"
		-- Commit will fail when the tree is clean, so ignore failure.
		let params = (Param "commit") : ps ++
			[Param "-m", Param "git-annex automatic sync"]
		_ <- inRepo $ tryIO . Git.Command.runQuiet params
		return True

mergeLocal :: Git.Ref -> CommandStart
mergeLocal branch = go =<< needmerge
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
		next $ next $ mergeFrom syncbranch

pushLocal :: Git.Ref -> CommandStart
pushLocal branch = do
	inRepo $ updateBranch $ syncBranch branch
	stop

updateBranch :: Git.Ref -> Git.Repo -> IO ()
updateBranch syncbranch g = 
	unlessM go $ error $ "failed to update " ++ show syncbranch
  where
	go = Git.Command.runBool
		[ Param "branch"
		, Param "-f"
		, Param $ show $ Git.Ref.base syncbranch
		] g

pullRemote :: Remote -> Git.Ref -> CommandStart
pullRemote remote branch = do
	showStart "pull" (Remote.name remote)
	next $ do
		showOutput
		stopUnless fetch $
			next $ mergeRemote remote (Just branch)
  where
	fetch = inRepo $ Git.Command.runBool
		[Param "fetch", Param $ Remote.name remote]

{- The remote probably has both a master and a synced/master branch.
 - Which to merge from? Well, the master has whatever latest changes
 - were committed (or pushed changes, if this is a bare remote),
 - while the synced/master may have changes that some
 - other remote synced to this remote. So, merge them both. -}
mergeRemote :: Remote -> (Maybe Git.Ref) -> CommandCleanup
mergeRemote remote b = case b of
	Nothing -> do
		branch <- inRepo Git.Branch.currentUnsafe
		all id <$> (mapM merge $ branchlist branch)
	Just _ -> all id <$> (mapM merge =<< tomerge (branchlist b))
  where
	merge = mergeFrom . remoteBranch remote
	tomerge branches = filterM (changed remote) branches
	branchlist Nothing = []
	branchlist (Just branch) = [branch, syncBranch branch]

pushRemote :: Remote -> Git.Ref -> CommandStart
pushRemote remote branch = go =<< needpush
  where
	needpush = anyM (newer remote) [syncBranch branch, Annex.Branch.name]
	go False = stop
	go True = do
		showStart "push" (Remote.name remote)
		next $ next $ do
			showOutput
			inRepo $ pushBranch remote branch

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
 -}
pushBranch :: Remote -> Git.Ref -> Git.Repo -> IO Bool
pushBranch remote branch g = tryIO (directpush g) `after` syncpush g
  where
	syncpush = Git.Command.runBool $ pushparams
		[ Git.Branch.forcePush $ refspec Annex.Branch.name
		, refspec branch
		]
	directpush = Git.Command.runQuiet $ pushparams
		[show $ Git.Ref.base branch]
	pushparams branches =
		[ Param "push"
		, Param $ Remote.name remote
		] ++ map Param branches
	refspec b = concat 
		[ show $ Git.Ref.base b
		,  ":"
		, show $ Git.Ref.base $ syncBranch b
		]

mergeAnnex :: CommandStart
mergeAnnex = do
	void $ Annex.Branch.forceUpdate
	stop

{- Merges from a branch into the current branch. -}
mergeFrom :: Git.Ref -> Annex Bool
mergeFrom branch = do
	showOutput
	ifM isDirect
		( maybe go godirect =<< inRepo Git.Branch.current
		, go
		)
  where
	go = runmerge $ inRepo $ Git.Merge.mergeNonInteractive branch
	godirect currbranch = do
		old <- inRepo $ Git.Ref.sha currbranch
		d <- fromRepo gitAnnexMergeDir
		r <- runmerge $ inRepo $ mergeDirect d branch
		new <- inRepo $ Git.Ref.sha currbranch
		case (old, new) of
			(Just oldsha, Just newsha) ->
				mergeDirectCleanup d oldsha newsha
			_ -> noop
		return r
	runmerge a = ifM (a)
		( return True
		, resolveMerge
		)

{- Resolves a conflicted merge. It's important that any conflicts be
 - resolved in a way that itself avoids later merge conflicts, since
 - multiple repositories may be doing this concurrently.
 -
 - Only annexed files are resolved; other files are left for the user to
 - handle.
 -
 - This uses the Keys pointed to by the files to construct new
 - filenames. So when both sides modified file foo, 
 - it will be deleted, and replaced with files foo.KEYA and foo.KEYB.
 -
 - On the other hand, when one side deleted foo, and the other modified it,
 - it will be deleted, and the modified version stored as file
 - foo.KEYA (or KEYB).
 -}
resolveMerge :: Annex Bool
resolveMerge = do
	top <- fromRepo Git.repoPath
	(fs, cleanup) <- inRepo (LsFiles.unmerged [top])
	merged <- all id <$> mapM resolveMerge' fs
	void $ liftIO cleanup

	(deleted, cleanup2) <- inRepo (LsFiles.deleted [top])
	unless (null deleted) $
		Annex.Queue.addCommand "rm" [Params "--quiet -f --"] deleted
	void $ liftIO cleanup2
	
	when merged $ do
		Annex.Queue.flush
		void $ inRepo $ Git.Command.runBool
			[ Param "commit"
			, Param "-m"
			, Param "git-annex automatic merge conflict fix"
			]
	return merged

resolveMerge' :: LsFiles.Unmerged -> Annex Bool
resolveMerge' u
	| issymlink LsFiles.valUs && issymlink LsFiles.valThem =
		withKey LsFiles.valUs $ \keyUs ->
			withKey LsFiles.valThem $ \keyThem -> do
				ifM isDirect
					( maybe noop (\k -> removeDirect k file) keyUs
					, liftIO $ nukeFile file
					)
				Annex.Queue.addCommand "rm" [Params "--quiet -f --"] [file]
				go keyUs keyThem
	| otherwise = return False
  where
	go keyUs keyThem
		| keyUs == keyThem = do
			makelink keyUs
			return True
		| otherwise = do
			makelink keyUs
			makelink keyThem
			return True
	file = LsFiles.unmergedFile u
	issymlink select = any (select (LsFiles.unmergedBlobType u) ==)
		[Just SymlinkBlob, Nothing]
	makelink (Just key) = do
		let dest = mergeFile file key
		l <- inRepo $ gitAnnexLink dest key
		replaceFile dest $ makeAnnexLink l
		stageSymlink dest =<< hashSymlink l
		whenM (isDirect) $
			toDirect key dest
	makelink _ = noop
	withKey select a = do
		let msha = select $ LsFiles.unmergedSha u
		case msha of
			Nothing -> a Nothing
			Just sha -> do
				key <- catKey sha symLinkMode
				maybe (return False) (a . Just) key

{- The filename to use when resolving a conflicted merge of a file,
 - that points to a key.
 -
 - Something derived from the key needs to be included in the filename,
 - but rather than exposing the whole key to the user, a very weak hash
 - is used. There is a very real, although still unlikely, chance of
 - conflicts using this hash.
 -
 - In the event that there is a conflict with the filename generated
 - for some other key, that conflict will itself be handled by the
 - conflicted merge resolution code. That case is detected, and the full
 - key is used in the filename.
 -}
mergeFile :: FilePath -> Key -> FilePath
mergeFile file key
	| doubleconflict = go $ key2file key
	| otherwise = go $ shortHash $ key2file key
  where
	varmarker = ".variant-"
	doubleconflict = varmarker `isInfixOf` file
	go v = takeDirectory file
		</> dropExtension (takeFileName file)
		++ varmarker ++ v
		++ takeExtension file
		
shortHash :: String -> String
shortHash = take 4 . md5s . md5FilePath

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
