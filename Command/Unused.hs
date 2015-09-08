{- git-annex command
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Command.Unused where

import qualified Data.Map as M

import Common.Annex
import Command
import Logs.Unused
import Annex.Content
import Logs.Location
import qualified Annex
import qualified Git
import qualified Git.Command
import qualified Git.Ref
import qualified Git.Branch
import qualified Git.RefLog
import qualified Git.LsFiles as LsFiles
import qualified Git.DiffTree as DiffTree
import qualified Backend
import qualified Remote
import qualified Annex.Branch
import Annex.CatFile
import Types.Key
import Types.RefSpec
import Git.FilePath
import Git.Types
import Logs.View (is_branchView)
import Annex.BloomFilter

cmd :: Command
cmd = -- withGlobalOptions [unusedFromOption, refSpecOption] $
	command "unused" SectionMaintenance 
		"look for unused file content"
		paramNothing (seek <$$> optParser)

data UnusedOptions = UnusedOptions
	{ fromRemote :: Maybe RemoteName
	, refSpecOption :: Maybe RefSpec
	}

optParser :: CmdParamsDesc -> Parser UnusedOptions
optParser _ = UnusedOptions
	<$> optional (strOption
		( long "from" <> short 'f' <> metavar paramRemote
		<> help "remote to check for unused content"
		))
	<*> optional (option (eitherReader parseRefSpec)
		( long "used-refspec" <> metavar paramRefSpec
		<> help "refs to consider used (default: all branches)"
		))

seek :: UnusedOptions -> CommandSeek
seek = commandAction . start

start :: UnusedOptions -> CommandStart
start o = do
	cfgrefspec <- fromMaybe allRefSpec . annexUsedRefSpec
		<$> Annex.getGitConfig
	let refspec = fromMaybe cfgrefspec (refSpecOption o)
	let (name, perform) = case fromRemote o of
		Nothing -> (".", checkUnused refspec)
		Just "." -> (".", checkUnused refspec)
		Just "here" -> (".", checkUnused refspec)
		Just n -> (n, checkRemoteUnused n refspec)
	showStart "unused" name
	next perform

checkUnused :: RefSpec -> CommandPerform
checkUnused refspec = chain 0
	[ check "" unusedMsg $ findunused =<< Annex.getState Annex.fast
	, check "bad" staleBadMsg $ staleKeysPrune gitAnnexBadDir False
	, check "tmp" staleTmpMsg $ staleKeysPrune gitAnnexTmpObjectDir True
	]
  where
	findunused True = do
		showNote "fast mode enabled; only finding stale files"
		return []
	findunused False = do
		showAction "checking for unused data"
		-- InAnnex, not InRepository because if a direct mode
		-- file exists, it is obviously not unused.
		excludeReferenced refspec =<< getKeysPresent InAnnex
	chain _ [] = next $ return True
	chain v (a:as) = do
		v' <- a v
		chain v' as

checkRemoteUnused :: String -> RefSpec -> CommandPerform
checkRemoteUnused name refspec = go =<< fromJust <$> Remote.byNameWithUUID (Just name)
  where
	go r = do
		showAction "checking for unused data"
		_ <- check "" (remoteUnusedMsg r) (remoteunused r) 0
		next $ return True
	remoteunused r = excludeReferenced refspec <=< loggedKeysFor $ Remote.uuid r

check :: FilePath -> ([(Int, Key)] -> String) -> Annex [Key] -> Int -> Annex Int
check file msg a c = do
	l <- a
	let unusedlist = number c l
	unless (null l) $ showLongNote $ msg unusedlist
	updateUnusedLog file $ M.fromList unusedlist
	return $ c + length l

number :: Int -> [a] -> [(Int, a)]
number _ [] = []
number n (x:xs) = (n+1, x) : number (n+1) xs

table :: [(Int, Key)] -> [String]
table l = "  NUMBER  KEY" : map cols l
  where
	cols (n,k) = "  " ++ pad 6 (show n) ++ "  " ++ key2file k
	pad n s = s ++ replicate (n - length s) ' '

staleTmpMsg :: [(Int, Key)] -> String
staleTmpMsg t = unlines $ 
	["Some partially transferred data exists in temporary files:"]
	++ table t ++ [dropMsg Nothing]

staleBadMsg :: [(Int, Key)] -> String
staleBadMsg t = unlines $ 
	["Some corrupted files have been preserved by fsck, just in case:"]
	++ table t ++ [dropMsg Nothing]

unusedMsg :: [(Int, Key)] -> String
unusedMsg u = unusedMsg' u
	["Some annexed data is no longer used by any files:"]
	[dropMsg Nothing]
unusedMsg' :: [(Int, Key)] -> [String] -> [String] -> String
unusedMsg' u mheader mtrailer = unlines $
	mheader ++
	table u ++
	["(To see where data was previously used, try: git log --stat -S'KEY')"] ++
	mtrailer

remoteUnusedMsg :: Remote -> [(Int, Key)] -> String
remoteUnusedMsg r u = unusedMsg' u
	["Some annexed data on " ++ name ++ " is not used by any files:"]
	[dropMsg $ Just r]
  where
	name = Remote.name r 

dropMsg :: Maybe Remote -> String
dropMsg Nothing = dropMsg' ""
dropMsg (Just r) = dropMsg' $ " --from " ++ Remote.name r
dropMsg' :: String -> String
dropMsg' s = "\nTo remove unwanted data: git-annex dropunused" ++ s ++ " NUMBER\n"

{- Finds keys in the list that are not referenced in the git repository.
 -
 - Strategy:
 -
 - * Build a bloom filter of all keys referenced by symlinks. This 
 -   is the fastest one to build and will filter out most keys.
 - * If keys remain, build a second bloom filter of keys referenced by
 -   branches maching the RefSpec.
 - * The list is streamed through these bloom filters lazily, so both will
 -   exist at the same time. This means that twice the memory is used,
 -   but they're relatively small, so the added complexity of using a
 -   mutable bloom filter does not seem worthwhile.
 - * Generating the second bloom filter can take quite a while, since
 -   it needs enumerating all keys in all git branches. But, the common
 -   case, if the second filter is needed, is for some keys to be globally
 -   unused, and in that case, no short-circuit is possible.
 -   Short-circuiting if the first filter filters all the keys handles the
 -   other common case.
 -}
excludeReferenced :: RefSpec -> [Key] -> Annex [Key]
excludeReferenced refspec ks = runfilter firstlevel ks >>= runfilter secondlevel
  where
	runfilter _ [] = return [] -- optimisation
	runfilter a l = bloomFilter l <$> genBloomFilter a
	firstlevel = withKeysReferencedM
	secondlevel = withKeysReferencedInGit refspec

{- Given an initial value, folds it with each key referenced by
 - symlinks in the git repo. -}
withKeysReferenced :: v -> (Key -> v -> v) -> Annex v
withKeysReferenced initial a = withKeysReferenced' Nothing initial folda
  where
	folda k _ v = return $ a k v

{- Runs an action on each referenced key in the git repo. -}
withKeysReferencedM :: (Key -> Annex ()) -> Annex ()
withKeysReferencedM a = withKeysReferenced' Nothing () calla
  where
	calla k _ _ = a k

{- Folds an action over keys and files referenced in a particular directory. -}
withKeysFilesReferencedIn :: FilePath -> v -> (Key -> FilePath -> v -> Annex v) -> Annex v
withKeysFilesReferencedIn = withKeysReferenced' . Just

withKeysReferenced' :: Maybe FilePath -> v -> (Key -> FilePath -> v -> Annex v) -> Annex v
withKeysReferenced' mdir initial a = do
	(files, clean) <- getfiles
	r <- go initial files
	liftIO $ void clean
	return r
  where
	getfiles = case mdir of
		Nothing -> ifM isBareRepo
			( return ([], return True)
			, do
				top <- fromRepo Git.repoPath
				inRepo $ LsFiles.allFiles [top]
			)
		Just dir -> inRepo $ LsFiles.inRepo [dir]
	go v [] = return v
	go v (f:fs) = do
		x <- Backend.lookupFile f
		case x of
			Nothing -> go v fs
			Just k -> do
				!v' <- a k f v
				go v' fs

withKeysReferencedInGit :: RefSpec -> (Key -> Annex ()) -> Annex ()
withKeysReferencedInGit refspec a = do
	current <- inRepo Git.Branch.currentUnsafe
	shaHead <- maybe (return Nothing) (inRepo . Git.Ref.sha) current
	rs <- relevantrefs (shaHead, current)
		<$> inRepo (Git.Command.pipeReadStrict [Param "show-ref"])
	usedrefs <- applyRefSpec refspec rs (getreflog rs)
	forM_ usedrefs $
		withKeysReferencedInGitRef a
  where
	relevantrefs headRef = addHead headRef .
		filter ourbranches .
		map (separate (== ' ')) .
		lines
	nubRefs = map (Git.Ref . snd) . nubBy (\(x, _) (y, _) -> x == y)
	ourbranchend = '/' : Git.fromRef Annex.Branch.name
	ourbranches (_, b) = not (ourbranchend `isSuffixOf` b)
		&& not ("refs/synced/" `isPrefixOf` b)
		&& not (is_branchView (Git.Ref b))
	addHead headRef refs = case headRef of
		-- if HEAD diverges from all branches (except the branch it
		-- points to), run the actions on staged keys (and keys
		-- that are only present in the work tree if the repo is
		-- non bare)
		(Just (Git.Ref x), Just (Git.Ref b))
			| all (\(x',b') -> x /= x' || b == b') refs ->
				Git.Ref.headRef
				: nubRefs (filter ((/= x) . fst) refs)
		_ -> nubRefs refs
	getreflog rs = inRepo $ Git.RefLog.getMulti rs

{- Runs an action on keys referenced in the given Git reference which
 - differ from those referenced in the work tree. -}
withKeysReferencedInGitRef :: (Key -> Annex ()) -> Git.Ref -> Annex ()
withKeysReferencedInGitRef a ref = do
	showAction $ "checking " ++ Git.Ref.describe ref
	bare <- isBareRepo
	(ts,clean) <- inRepo $ if bare
		then DiffTree.diffIndex ref
		else DiffTree.diffWorkTree ref
	let lookAtWorkingTree = not bare && ref == Git.Ref.headRef
	forM_ ts $ tKey lookAtWorkingTree >=> maybe noop a
	liftIO $ void clean
  where
	tKey True = Backend.lookupFile . getTopFilePath . DiffTree.file
	tKey False = fileKey . takeFileName . decodeBS <$$>
		catFile ref . getTopFilePath . DiffTree.file

data UnusedMaps = UnusedMaps
	{ unusedMap :: UnusedMap
	, unusedBadMap :: UnusedMap
	, unusedTmpMap :: UnusedMap
	}

withUnusedMaps :: (UnusedMaps -> Int -> CommandStart) -> CmdParams -> CommandSeek
withUnusedMaps a params = do
	unused <- readUnusedMap ""
	unusedbad <- readUnusedMap "bad"
	unusedtmp <- readUnusedMap "tmp"
	let m = unused `M.union` unusedbad `M.union` unusedtmp
	let unusedmaps = UnusedMaps unused unusedbad unusedtmp
	seekActions $ return $ map (a unusedmaps) $
		concatMap (unusedSpec m) params

unusedSpec :: UnusedMap -> String -> [Int]
unusedSpec m spec
	| spec == "all" = if M.null m
		then []
		else [fst (M.findMin m)..fst (M.findMax m)]
	| "-" `isInfixOf` spec = range $ separate (== '-') spec
	| otherwise = maybe badspec (: []) (readish spec)
  where
	range (a, b) = case (readish a, readish b) of
		(Just x, Just y) -> [x..y]
		_ -> badspec
	badspec = error $ "Expected number or range, not \"" ++ spec ++ "\""

{- Seek action for unused content. Finds the number in the maps, and
 - calls one of 3 actions, depending on the type of unused file. -}
startUnused :: String
	-> (Key -> CommandPerform)
	-> (Key -> CommandPerform) 
	-> (Key -> CommandPerform)
	-> UnusedMaps -> Int -> CommandStart
startUnused message unused badunused tmpunused maps n = search
	[ (unusedMap maps, unused)
	, (unusedBadMap maps, badunused)
	, (unusedTmpMap maps, tmpunused)
	]
  where
	search [] = error $ show n ++ " not valid (run git annex unused for list)"
	search ((m, a):rest) =
		case M.lookup n m of
			Nothing -> search rest
			Just key -> do
				showStart message (show n)
				next $ a key
