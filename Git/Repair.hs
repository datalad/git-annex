{- git repository recovery
 -
 - Copyright 2013-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Repair (
	runRepair,
	runRepairOf,
	removeBadBranches,
	successfulRepair,
	cleanCorruptObjects,
	retrieveMissingObjects,
	resetLocalBranches,
	checkIndex,
	checkIndexFast,
	missingIndex,
	emptyGoodCommits,
	isTrackingBranch,
) where

import Common
import Git
import Git.Command
import Git.Objects
import Git.Sha
import Git.Types
import Git.Fsck
import Git.Index
import qualified Git.Config as Config
import qualified Git.Construct as Construct
import qualified Git.LsTree as LsTree
import qualified Git.LsFiles as LsFiles
import qualified Git.Ref as Ref
import qualified Git.RefLog as RefLog
import qualified Git.UpdateIndex as UpdateIndex
import qualified Git.Branch as Branch
import Utility.Tmp
import Utility.Rsync
import Utility.FileMode

import qualified Data.Set as S
import qualified Data.ByteString.Lazy as L
import Data.Tuple.Utils

{- Given a set of bad objects found by git fsck, which may not
 - be complete, finds and removes all corrupt objects. -}
cleanCorruptObjects :: FsckResults -> Repo -> IO ()
cleanCorruptObjects fsckresults r = do
	void $ explodePacks r
	mapM_ removeLoose (S.toList $ knownMissing fsckresults)
	mapM_ removeBad =<< listLooseObjectShas r
  where
	removeLoose s = nukeFile (looseObjectFile r s)
	removeBad s = do
		void $ tryIO $ allowRead $  looseObjectFile r s
		whenM (isMissing s r) $
			removeLoose s

{- Explodes all pack files, and deletes them.
 -
 - First moves all pack files to a temp dir, before unpacking them each in
 - turn.
 -
 - This is because unpack-objects will not unpack a pack file if it's in the
 - git repo.
 -
 - Also, this prevents unpack-objects from possibly looking at corrupt
 - pack files to see if they contain an object, while unpacking a
 - non-corrupt pack file.
 -}
explodePacks :: Repo -> IO Bool
explodePacks r = go =<< listPackFiles r
  where
	go [] = return False
	go packs = withTmpDir "packs" $ \tmpdir -> do
		putStrLn "Unpacking all pack files."
		forM_ packs $ \packfile -> do
			moveFile packfile (tmpdir </> takeFileName packfile)
			nukeFile $ packIdxFile packfile
		forM_ packs $ \packfile -> do
			let tmp = tmpdir </> takeFileName packfile
			allowRead tmp
			-- May fail, if pack file is corrupt.
			void $ tryIO $
				pipeWrite [Param "unpack-objects", Param "-r"] r $ \h ->
				L.hPut h =<< L.readFile tmp
		return True

{- Try to retrieve a set of missing objects, from the remotes of a
 - repository. Returns any that could not be retreived.
 -
 - If another clone of the repository exists locally, which might not be a
 - remote of the repo being repaired, its path can be passed as a reference
 - repository.
 -}
retrieveMissingObjects :: FsckResults -> Maybe FilePath -> Repo -> IO FsckResults
retrieveMissingObjects missing referencerepo r
	| not (foundBroken missing) = return missing
	| otherwise = withTmpDir "tmprepo" $ \tmpdir -> do
		unlessM (boolSystem "git" [Param "init", File tmpdir]) $
			error $ "failed to create temp repository in " ++ tmpdir
		tmpr <- Config.read =<< Construct.fromAbsPath tmpdir
		stillmissing <- pullremotes tmpr (remotes r) fetchrefstags missing
		if S.null (knownMissing stillmissing)
			then return stillmissing
			else pullremotes tmpr (remotes r) fetchallrefs stillmissing
  where
	pullremotes tmpr [] fetchrefs stillmissing = case referencerepo of
		Nothing -> return stillmissing
		Just p -> ifM (fetchfrom p fetchrefs tmpr)
			( do
				void $ explodePacks tmpr
				void $ copyObjects tmpr r
				case stillmissing of
					FsckFailed -> return $ FsckFailed
					FsckFoundMissing s t -> FsckFoundMissing 
						<$> findMissing (S.toList s) r
						<*> pure t
			, return stillmissing
			)
	pullremotes tmpr (rmt:rmts) fetchrefs ms
		| not (foundBroken ms) = return ms
		| otherwise = do
			putStrLn $ "Trying to recover missing objects from remote " ++ repoDescribe rmt ++ "."
			ifM (fetchfrom (repoLocation rmt) fetchrefs tmpr)
				( do
					void $ explodePacks tmpr
					void $ copyObjects tmpr r
					case ms of
						FsckFailed -> pullremotes tmpr rmts fetchrefs ms
						FsckFoundMissing s t -> do
							stillmissing <- findMissing (S.toList s) r
							pullremotes tmpr rmts fetchrefs (FsckFoundMissing stillmissing t)
				, pullremotes tmpr rmts fetchrefs ms
				)
	fetchfrom fetchurl ps fetchr = runBool ps' fetchr'
	  where
		ps' = 
			[ Param "fetch"
			, Param fetchurl
			, Param "--force"
			, Param "--update-head-ok"
			, Param "--quiet"
			] ++ ps
		fetchr' = fetchr { gitGlobalOpts = gitGlobalOpts fetchr ++ nogc }
		nogc = [ Param "-c", Param "gc.auto=0" ]

	-- fetch refs and tags
	fetchrefstags = [ Param "+refs/heads/*:refs/heads/*", Param "--tags"]
	-- Fetch all available refs (more likely to fail,
	-- as the remote may have refs it refuses to send).
	fetchallrefs = [ Param "+*:*" ]

{- Copies all objects from the src repository to the dest repository.
 - This is done using rsync, so it copies all missing objects, and all
 - objects they rely on. -}
copyObjects :: Repo -> Repo -> IO Bool
copyObjects srcr destr = rsync
	[ Param "-qr"
	, File $ addTrailingPathSeparator $ objectsDir srcr
	, File $ addTrailingPathSeparator $ objectsDir destr
	]

{- To deal with missing objects that cannot be recovered, resets any
 - local branches to point to an old commit before the missing
 - objects. Returns all branches that were changed, and deleted.
 -}
resetLocalBranches :: MissingObjects -> GoodCommits -> Repo -> IO ([Branch], [Branch], GoodCommits)
resetLocalBranches missing goodcommits r =
	go [] [] goodcommits =<< filter islocalbranch <$> getAllRefs r
  where
	islocalbranch b = "refs/heads/" `isPrefixOf` fromRef b
	go changed deleted gcs [] = return (changed, deleted, gcs)
	go changed deleted gcs (b:bs) = do
		(mc, gcs') <- findUncorruptedCommit missing gcs b r
		case mc of
			Just c
				| c == b -> go changed deleted gcs' bs
				| otherwise -> do
					reset b c
					go (b:changed) deleted gcs' bs
			Nothing -> do
				nukeBranchRef b r
				go changed (b:deleted) gcs' bs
	reset b c = do
		nukeBranchRef b	r
		void $ runBool
			[ Param "branch"
			, Param (fromRef $ Ref.base b)
			, Param (fromRef c)
			] r

isTrackingBranch :: Ref -> Bool
isTrackingBranch b = "refs/remotes/" `isPrefixOf` fromRef b

{- To deal with missing objects that cannot be recovered, removes
 - any branches (filtered by a predicate) that reference them
 - Returns a list of all removed branches.
 -}
removeBadBranches :: (Ref -> Bool) -> Repo -> IO [Branch]
removeBadBranches removablebranch r = fst <$> removeBadBranches' removablebranch S.empty emptyGoodCommits r

removeBadBranches' :: (Ref -> Bool) -> MissingObjects -> GoodCommits -> Repo -> IO ([Branch], GoodCommits)
removeBadBranches' removablebranch missing goodcommits r =
	go [] goodcommits =<< filter removablebranch <$> getAllRefs r
  where
	go removed gcs [] = return (removed, gcs)
	go removed gcs (b:bs) = do
		(ok, gcs') <- verifyCommit missing gcs b r
		if ok
			then go removed gcs' bs
			else do
				nukeBranchRef b r
				go (b:removed) gcs' bs

badBranches :: MissingObjects -> Repo -> IO [Branch]
badBranches missing r = filterM isbad =<< getAllRefs r
  where
	isbad b = not . fst <$> verifyCommit missing emptyGoodCommits b r

{- Gets all refs, including ones that are corrupt.
 - git show-ref does not output refs to commits that are directly
 - corrupted, so it is not used.
 -
 - Relies on packed refs being exploded before it's called.
 -}
getAllRefs :: Repo -> IO [Ref]
getAllRefs r = getAllRefs' (localGitDir r </> "refs")

getAllRefs' :: FilePath -> IO [Ref]
getAllRefs' refdir = do
	let topsegs = length (splitPath refdir) - 1
	let toref = Ref . joinPath . drop topsegs . splitPath
	map toref <$> dirContentsRecursive refdir

explodePackedRefsFile :: Repo -> IO ()
explodePackedRefsFile r = do
	let f = packedRefsFile r
	whenM (doesFileExist f) $ do
		rs <- mapMaybe parsePacked . lines
			<$> catchDefaultIO "" (safeReadFile f)
		forM_ rs makeref
		nukeFile f
  where
	makeref (sha, ref) = do
		let dest = localGitDir r </> fromRef ref
		createDirectoryIfMissing True (parentDir dest)
		unlessM (doesFileExist dest) $
			writeFile dest (fromRef sha)

packedRefsFile :: Repo -> FilePath
packedRefsFile r = localGitDir r </> "packed-refs"

parsePacked :: String -> Maybe (Sha, Ref)
parsePacked l = case words l of
	(sha:ref:[])
		| isJust (extractSha sha) && Ref.legal True ref ->
			Just (Ref sha, Ref ref)
	_ -> Nothing

{- git-branch -d cannot be used to remove a branch that is directly
 - pointing to a corrupt commit. -}
nukeBranchRef :: Branch -> Repo -> IO ()
nukeBranchRef b r = nukeFile $ localGitDir r </> fromRef b

{- Finds the most recent commit to a branch that does not need any
 - of the missing objects. If the input branch is good as-is, returns it.
 - Otherwise, tries to traverse the commits in the branch to find one
 - that is ok. That might fail, if one of them is corrupt, or if an object
 - at the root of the branch is missing. Finally, looks for an old version
 - of the branch from the reflog.
 -}
findUncorruptedCommit :: MissingObjects -> GoodCommits -> Branch -> Repo -> IO (Maybe Sha, GoodCommits)
findUncorruptedCommit missing goodcommits branch r = do
	(ok, goodcommits') <- verifyCommit missing goodcommits branch r
	if ok
		then return (Just branch, goodcommits')
		else do
			(ls, cleanup) <- pipeNullSplit
				[ Param "log"
				, Param "-z"
				, Param "--format=%H"
				, Param (fromRef branch)
				] r
			let branchshas = catMaybes $ map extractSha ls
			reflogshas <- RefLog.get branch r
			-- XXX Could try a bit harder here, and look
			-- for uncorrupted old commits in branches in the
			-- reflog.
			cleanup `after` findfirst goodcommits (branchshas ++ reflogshas)
  where
	findfirst gcs [] = return (Nothing, gcs)
	findfirst gcs (c:cs) = do
		(ok, gcs') <- verifyCommit missing gcs c r
		if ok
			then return (Just c, gcs')
			else findfirst gcs' cs

{- Verifies that none of the missing objects in the set are used by
 - the commit. Also adds to a set of commit shas that have been verified to
 - be good, which can be passed into subsequent calls to avoid
 - redundant work when eg, chasing down branches to find the first
 - uncorrupted commit. -}
verifyCommit :: MissingObjects -> GoodCommits -> Sha -> Repo -> IO (Bool, GoodCommits)
verifyCommit missing goodcommits commit r
	| checkGoodCommit commit goodcommits = return (True, goodcommits)
	| otherwise = do
		(ls, cleanup) <- pipeNullSplit
			[ Param "log"
			, Param "-z"
			, Param "--format=%H %T"
			, Param (fromRef commit)
			] r
		let committrees = map parse ls
		if any isNothing committrees || null committrees
			then do
				void cleanup
				return (False, goodcommits)
			else do
				let cts = catMaybes committrees
				ifM (cleanup <&&> check cts)
					( return (True, addGoodCommits (map fst cts) goodcommits)
					, return (False, goodcommits)
					)
  where
	parse l = case words l of
		(commitsha:treesha:[]) -> (,)
			<$> extractSha commitsha
			<*> extractSha treesha
		_ -> Nothing
	check [] = return True
	check ((c, t):rest)
		| checkGoodCommit c goodcommits = return True
		| otherwise = verifyTree missing t r <&&> check rest

{- Verifies that a tree is good, including all trees and blobs
 - referenced by it. -}
verifyTree :: MissingObjects -> Sha -> Repo -> IO Bool
verifyTree missing treesha r
	| S.member treesha missing = return False
	| otherwise = do
		(ls, cleanup) <- pipeNullSplit (LsTree.lsTreeParams treesha []) r
		let objshas = map (LsTree.sha . LsTree.parseLsTree) ls
		if any (`S.member` missing) objshas
			then do
				void cleanup
				return False
			-- as long as ls-tree succeeded, we're good
			else cleanup

{- Checks that the index file only refers to objects that are not missing,
 - and is not itself corrupt. Note that a missing index file is not
 - considered a problem (repo may be new). -}
checkIndex :: Repo -> IO Bool
checkIndex r = do
	(bad, _good, cleanup) <- partitionIndex r
	if null bad
		then cleanup
		else do
			void cleanup
			return False

{- Does not check every object the index refers to, but only that the index
 - itself is not corrupt. -}
checkIndexFast :: Repo -> IO Bool
checkIndexFast r = do
	(indexcontents, cleanup) <- LsFiles.stagedDetails [repoPath r] r
	length indexcontents `seq` cleanup

missingIndex :: Repo -> IO Bool
missingIndex r = not <$> doesFileExist (localGitDir r </> "index")

{- Finds missing and ok files staged in the index. -}
partitionIndex :: Repo -> IO ([LsFiles.StagedDetails], [LsFiles.StagedDetails], IO Bool)
partitionIndex r = do
	(indexcontents, cleanup) <- LsFiles.stagedDetails [repoPath r] r
	l <- forM indexcontents $ \i -> case i of
		(_file, Just sha, Just _mode) -> (,) <$> isMissing sha r <*> pure i
		_ -> pure (False, i)
	let (bad, good) = partition fst l
	return (map snd bad, map snd good, cleanup)

{- Rewrites the index file, removing from it any files whose blobs are
 - missing. Returns the list of affected files. -}
rewriteIndex :: Repo -> IO [FilePath]
rewriteIndex r
	| repoIsLocalBare r = return []
	| otherwise = do
		(bad, good, cleanup) <- partitionIndex r
		unless (null bad) $ do
			nukeFile (indexFile r)
			UpdateIndex.streamUpdateIndex r
				=<< (catMaybes <$> mapM reinject good)
		void cleanup
		return $ map fst3 bad
  where
	reinject (file, Just sha, Just mode) = case toBlobType mode of
		Nothing -> return Nothing
		Just blobtype -> Just <$>
			UpdateIndex.stageFile sha blobtype file r
	reinject _ = return Nothing

newtype GoodCommits = GoodCommits (S.Set Sha)

emptyGoodCommits :: GoodCommits
emptyGoodCommits = GoodCommits S.empty

checkGoodCommit :: Sha -> GoodCommits -> Bool
checkGoodCommit sha (GoodCommits s) = S.member sha s

addGoodCommits :: [Sha] -> GoodCommits -> GoodCommits
addGoodCommits shas (GoodCommits s) = GoodCommits $
	S.union s (S.fromList shas)

displayList :: [String] -> String -> IO ()
displayList items header
	| null items = return ()
	| otherwise = do
		putStrLn header
		putStr $ unlines $ map (\i -> "\t" ++ i) truncateditems
  where
	numitems = length items
	truncateditems
		| numitems > 10 = take 10 items ++ ["(and " ++ show (numitems - 10) ++ " more)"]
		| otherwise = items

{- Fix problems that would prevent repair from working at all
 -
 - A missing or corrupt .git/HEAD makes git not treat the repository as a
 - git repo. If there is a git repo in a parent directory, it may move up
 - the tree and use that one instead. So, cannot use `git show-ref HEAD` to
 - test it.
 -
 - Explode the packed refs file, to simplify dealing with refs, and because
 - fsck can complain about bad refs in it.
 -}
preRepair :: Repo -> IO ()
preRepair g = do
	unlessM (validhead <$> catchDefaultIO "" (safeReadFile headfile)) $ do
		nukeFile headfile
		writeFile headfile "ref: refs/heads/master"
	explodePackedRefsFile g
	unless (repoIsLocalBare g) $ do
		let f = indexFile g
		void $ tryIO $ allowWrite f
  where
	headfile = localGitDir g </> "HEAD"
	validhead s = "ref: refs/" `isPrefixOf` s || isJust (extractSha s)

{- Put it all together. -}
runRepair :: (Ref -> Bool) -> Bool -> Repo -> IO (Bool, [Branch])
runRepair removablebranch forced g = do
	preRepair g
	putStrLn "Running git fsck ..."
	fsckresult <- findBroken False g
	if foundBroken fsckresult
		then runRepair' removablebranch fsckresult forced Nothing g
		else do
			bad <- badBranches S.empty g
			if null bad
				then do
					putStrLn "No problems found."
					return (True, [])
				else runRepair' removablebranch fsckresult forced Nothing g

runRepairOf :: FsckResults -> (Ref -> Bool) -> Bool -> Maybe FilePath -> Repo -> IO (Bool, [Branch])
runRepairOf fsckresult removablebranch forced referencerepo g = do
	preRepair g
	runRepair' removablebranch fsckresult forced referencerepo g

runRepair' :: (Ref -> Bool) -> FsckResults -> Bool -> Maybe FilePath -> Repo -> IO (Bool, [Branch])
runRepair' removablebranch fsckresult forced referencerepo g = do
	cleanCorruptObjects fsckresult g
	missing <- findBroken False g
	stillmissing <- retrieveMissingObjects missing referencerepo g
	case stillmissing of
		FsckFoundMissing s t
			| S.null s -> if repoIsLocalBare g
				then checkbadbranches s
				else ifM (checkIndex g)
					( checkbadbranches s
					, do
						putStrLn "No missing objects found, but the index file is corrupt!"
						if forced
							then corruptedindex
							else needforce
					)
			| otherwise -> if forced
				then ifM (checkIndex g)
					( forcerepair s t
					, corruptedindex
					)
				else do
					putStrLn $ unwords
						[ show (S.size s)
						, "missing objects could not be recovered!"
						]
					unsuccessfulfinish
		FsckFailed
			| forced -> ifM (pure (repoIsLocalBare g) <||> checkIndex g)
				( do
					cleanCorruptObjects FsckFailed g
					stillmissing' <- findBroken False g
					case stillmissing' of
						FsckFailed -> return (False, [])
						FsckFoundMissing s t -> forcerepair s t
				, corruptedindex
				)
			| otherwise -> unsuccessfulfinish
  where
	repairbranches missing = do
		(removedbranches, goodcommits) <- removeBadBranches' removablebranch missing emptyGoodCommits g
		let remotebranches = filter isTrackingBranch removedbranches
		unless (null remotebranches) $
			putStrLn $ unwords
				[ "Removed"
				, show (length remotebranches)
				, "remote tracking branches that referred to missing objects."
				]
		(resetbranches, deletedbranches, _) <- resetLocalBranches missing goodcommits g
		displayList (map fromRef resetbranches)
			"Reset these local branches to old versions before the missing objects were committed:"
		displayList (map fromRef deletedbranches)
			"Deleted these local branches, which could not be recovered due to missing objects:"
		return (resetbranches ++ deletedbranches)

	checkbadbranches missing = do
		bad <- badBranches missing g
		case (null bad, forced) of
			(True, _) -> successfulfinish []
			(False, False) -> do
				displayList (map fromRef bad)
					"Some git branches refer to missing objects:"
				unsuccessfulfinish
			(False, True) -> successfulfinish =<< repairbranches missing

	forcerepair missing fscktruncated = do
		modifiedbranches <- repairbranches missing
		deindexedfiles <- rewriteIndex g
		displayList deindexedfiles
			"Removed these missing files from the index. You should look at what files are present in your working tree and git add them back to the index when appropriate."

		-- When the fsck results were truncated, try
		-- fscking again, and as long as different
		-- missing objects are found, continue
		-- the repair process.
		if fscktruncated
			then do
				fsckresult' <- findBroken False g
				case fsckresult' of
					FsckFailed -> do
						putStrLn "git fsck is failing"
						return (False, modifiedbranches)
					FsckFoundMissing s _
						| S.null s -> successfulfinish modifiedbranches
						| S.null (s `S.difference` missing) -> do
							putStrLn $ unwords
								[ show (S.size s)
								, "missing objects could not be recovered!"
								]
							return (False, modifiedbranches)	
						| otherwise -> do
							(ok, modifiedbranches') <- runRepairOf fsckresult' removablebranch forced referencerepo g
							return (ok, modifiedbranches++modifiedbranches')
			else successfulfinish modifiedbranches

	corruptedindex = do
		nukeFile (indexFile g)
		-- The corrupted index can prevent fsck from finding other
		-- problems, so re-run repair.
		fsckresult' <- findBroken False g
		result <- runRepairOf fsckresult' removablebranch forced referencerepo g
		putStrLn "Removed the corrupted index file. You should look at what files are present in your working tree and git add them back to the index when appropriate."
		return result

	successfulfinish modifiedbranches
		| null modifiedbranches = do
			mapM_ putStrLn
				[ "Successfully recovered repository!"
				, "You should run \"git fsck\" to make sure, but it looks like everything was recovered ok."
				]
			return (True, modifiedbranches)
		| otherwise = do
			unless (repoIsLocalBare g) $ do
				mcurr <- Branch.currentUnsafe g
				case mcurr of
					Nothing -> return ()
					Just curr -> when (any (== curr) modifiedbranches) $ do
						putStrLn $ unwords
							[ "You currently have"
							, fromRef curr
							, "checked out. You may have staged changes in the index that can be committed to recover the lost state of this branch!"
							]
			putStrLn "Successfully recovered repository!"
			putStrLn "Please carefully check that the changes mentioned above are ok.."
			return (True, modifiedbranches)
	
	unsuccessfulfinish = do
		if repoIsLocalBare g
			then do
				putStrLn "If you have a clone of this bare repository, you should add it as a remote of this repository, and retry."
				putStrLn "If there are no clones of this repository, you can instead retry with the --force parameter to force recovery to a possibly usable state."
				return (False, [])
			else needforce
	needforce = do
		putStrLn "To force a recovery to a usable state, retry with the --force parameter."
		return (False, [])

successfulRepair :: (Bool, [Branch]) -> Bool
successfulRepair = fst

safeReadFile :: FilePath -> IO String
safeReadFile f = do
	allowRead f
	readFileStrict f
