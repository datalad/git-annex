{- git-annex command
 -
 - Copyright 2011-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns, DeriveDataTypeable, PackageImports #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Command.Info where

import "mtl" Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.ByteString.Short (fromShort)
import System.PosixCompat.Files (isDirectory)
import Data.Ord
import qualified Data.Semigroup as Sem
import Prelude

import Command
import qualified Git
import qualified Annex
import qualified Remote
import qualified Types.Remote as Remote
import qualified Annex.SpecialRemote as SpecialRemote
import Utility.DataUnits
import Utility.DiskFree
import Annex.Content
import Annex.UUID
import Annex.CatFile
import Annex.WorkTree
import Logs.UUID
import Logs.Trust
import Logs.Location
import Annex.Branch (UnmergedBranches(..), getUnmergedRefs)
import Annex.NumCopies
import Git.Config (boolConfig)
import qualified Git.LsTree as LsTree
import Utility.Percentage
import Utility.Aeson
import Types.Transfer
import Logs.Transfer
import Types.Key
import Types.TrustLevel
import Types.FileMatcher
import Types.Availability
import qualified Limit
import Messages.JSON (DualDisp(..), ObjectMap(..))
import Annex.BloomFilter
import Annex.RepoSize
import qualified Command.Unused
import qualified Utility.RawFilePath as R

-- a named computation that produces a statistic
type Stat = StatState (Maybe (String, StatState String))

-- data about a set of keys
data KeyInfo = KeyInfo
	{ countKeys :: Integer
	, sizeKeys :: Integer
	, unknownSizeKeys :: Integer
	, backendsKeys :: M.Map KeyVariety Integer
	}
	
instance Sem.Semigroup KeyInfo where
	a <> b = KeyInfo
		{ countKeys = countKeys a + countKeys b
		, sizeKeys = sizeKeys a + sizeKeys b
		, unknownSizeKeys = unknownSizeKeys a + unknownSizeKeys b
		, backendsKeys = backendsKeys a <> backendsKeys b
		}

instance Monoid KeyInfo where
	mempty = KeyInfo 0 0 0 M.empty

data NumCopiesStats = NumCopiesStats
	{ numCopiesVarianceMap :: M.Map Variance Integer
	}

newtype Variance = Variance Int
	deriving (Eq, Ord)

instance Show Variance where
	show (Variance n)
		| n >= 0 = "+" ++ show n
		| otherwise = show n

-- cached info that multiple Stats use
data StatInfo = StatInfo
	{ presentData :: Maybe KeyInfo
	, referencedData :: Maybe KeyInfo
	, repoData :: M.Map UUID KeyInfo
	, allRepoData :: Maybe KeyInfo
	, numCopiesStats :: Maybe NumCopiesStats
	, infoOptions :: InfoOptions
	}

emptyStatInfo :: InfoOptions -> StatInfo
emptyStatInfo = StatInfo Nothing Nothing M.empty Nothing Nothing

-- a state monad for running Stats in
type StatState = StateT StatInfo Annex

cmd :: Command
cmd = noCommit $ withAnnexOptions [jsonOptions, annexedMatchingOptions] $
	command "info" SectionQuery
		"information about an item or the repository"
		(paramRepeating paramItem) (seek <$$> optParser)

data InfoOptions = InfoOptions
	{ infoFor :: CmdParams
	, bytesOption :: Bool
	, batchOption :: BatchMode
	, autoenableOption :: Bool
	, deadrepositoriesOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser InfoOptions
optParser desc = InfoOptions
	<$> cmdParamsWithCompleter desc (completeFiles <> completeRemotes)
	<*> switch
		( long "bytes"
		<> help "display file sizes in bytes"
		)
	<*> parseBatchOption False
	<*> switch
		( long "autoenable"
		<> help "list special remotes that are configured to autoenable"
		)
	<*> switch
		( long "dead-repositories"
		<> help "list repositories that have been marked as dead"
		)

seek :: InfoOptions -> CommandSeek
seek o = case batchOption o of
	NoBatch -> withWords (commandAction . start o) (infoFor o)
	Batch fmt -> batchOnly Nothing (infoFor o) $
		batchInput fmt (pure . Right) (itemInfo o)

start :: InfoOptions -> [String] -> CommandStart
start o [] = do
	if autoenableOption o
		then autoenableInfo
		else if deadrepositoriesOption o
			then deadrepositoriesInfo o
			else globalInfo o
	stop
start o ps = do
	mapM_ (\p -> itemInfo o (SeekInput [p], p)) ps
	stop

globalInfo :: InfoOptions -> Annex ()
globalInfo o = do
	u <- getUUID
	whenM ((==) DeadTrusted <$> lookupTrust u) $
		earlyWarning "Warning: This repository is currently marked as dead."
	stats <- selStats global_fast_stats global_slow_stats
	showCustom "info" (SeekInput []) $ do
		evalStateT (mapM_ showStat stats) (emptyStatInfo o)
		return True

autoenableInfo :: Annex ()
autoenableInfo = showCustom "info" (SeekInput []) $ do
	m <- SpecialRemote.specialRemoteNameMap
		<$> SpecialRemote.autoEnableable
	descm <- M.unionWith Remote.addName
		<$> uuidDescMap
		<*> pure (M.map toUUIDDesc m)
	s <- Remote.prettyPrintUUIDsDescs
		"autoenable special remotes"
		descm (M.keys m)
	showRaw (encodeBS s)
	return True

deadrepositoriesInfo :: InfoOptions -> Annex ()
deadrepositoriesInfo o = showCustom "info" (SeekInput []) $ do
	evalStateT (showStat (repo_list DeadTrusted)) (emptyStatInfo o)
	return True

itemInfo :: InfoOptions -> (SeekInput, String) -> Annex ()
itemInfo o (si, p) = ifM (isdir (toRawFilePath p))
	( dirInfo o p si
	, Remote.byName' p >>= \case
		Right r -> remoteInfo o r si
		Left _ -> Remote.nameToUUID' p >>= \case
			([], _) -> do
				relp <- liftIO $ relPathCwdToFile (toOsPath p)
				lookupKey relp >>= \case
					Just k -> fileInfo o (fromOsPath relp) si k
					Nothing -> treeishInfo o p si
			([u], _) -> uuidInfo o u si
			(_us, msg) -> noInfo p si msg
	)
  where
	isdir = liftIO . catchBoolIO . (isDirectory <$$> R.getFileStatus)

noInfo :: String -> SeekInput -> String -> Annex ()
noInfo s si msg = do
	-- The string may not really be a file, but use ActionItemTreeFile,
	-- rather than ActionItemOther to avoid breaking back-compat of
	-- json output.
	let ai = ActionItemTreeFile (toOsPath s)
	showStartMessage (StartMessage "info" ai si)
	showNote (UnquotedString msg)
	showEndFail
	Annex.incError

dirInfo :: InfoOptions -> FilePath -> SeekInput -> Annex ()
dirInfo o dir si = showCustom (unwords ["info", dir]) si $ do
	stats <- selStats
		(tostats (dir_name:tree_fast_stats True))
		(tostats tree_slow_stats)
	evalStateT (mapM_ showStat stats) =<< getDirStatInfo o dir
	return True
  where
	tostats = map (\s -> s dir)

treeishInfo :: InfoOptions -> String -> SeekInput -> Annex ()
treeishInfo o t si = do
	mi <- getTreeStatInfo o (Git.Ref (encodeBS t))
	case mi of
		Nothing -> noInfo t si
			"not a directory or an annexed file or a treeish or a remote or a uuid"
		Just i -> showCustom (unwords ["info", t]) si $ do
			stats <- selStats 
				(tostats (tree_name:tree_fast_stats False)) 
				(tostats tree_slow_stats)
			evalStateT (mapM_ showStat stats) i
			return True
  where
	tostats = map (\s -> s t)

fileInfo :: InfoOptions -> FilePath -> SeekInput -> Key -> Annex ()
fileInfo o file si k = do
	matcher <- Limit.getMatcher
	let file' = toOsPath file
	whenM (matcher $ MatchingFile $ FileInfo file' file' (Just k)) $
		showCustom (unwords ["info", file]) si $ do
			evalStateT (mapM_ showStat (file_stats file k)) (emptyStatInfo o)
			return True

remoteInfo :: InfoOptions -> Remote -> SeekInput -> Annex ()
remoteInfo o r si = showCustom (unwords ["info", Remote.name r]) si $ do
	i <- map (\(k, v) -> simpleStat k (pure v)) <$> Remote.getInfo r
	let u = Remote.uuid r
	l <- selStats 
		(uuid_fast_stats u ++ remote_fast_stats r ++ i)
		(uuid_slow_stats u)
	evalStateT (mapM_ showStat l) (emptyStatInfo o)
	return True

uuidInfo :: InfoOptions -> UUID -> SeekInput -> Annex ()
uuidInfo o u si = showCustom (unwords ["info", fromUUID u]) si $ do
	l <- selStats (uuid_fast_stats u) (uuid_slow_stats u)
	evalStateT (mapM_ showStat l) (emptyStatInfo o)
	return True

selStats :: [Stat] -> [Stat] -> Annex [Stat]
selStats fast_stats slow_stats = do
	fast <- Annex.getRead Annex.fast
	return $ if fast
		then fast_stats
		else fast_stats ++ slow_stats

{- Order is significant. Less expensive operations, and operations
 - that share data go together.
 -}
global_fast_stats :: [Stat]
global_fast_stats = 
	[ repo_list Trusted
	, repo_list SemiTrusted
	, repo_list UnTrusted
	, transfer_list
	, disk_size
	]

global_slow_stats :: [Stat]
global_slow_stats = 
	[ tmp_size
	, bad_data_size
	, local_annex_keys
	, local_annex_size
	, known_annex_files True
	, known_annex_size True
	, total_annex_size
	, reposizes_stats_global
	, backend_usage
	, bloom_info
	]

tree_fast_stats :: Bool -> [FilePath -> Stat]
tree_fast_stats isworktree =
	[ const local_annex_keys
	, const local_annex_size
	, const (known_annex_files isworktree)
	, const (known_annex_size isworktree)
	]

tree_slow_stats :: [FilePath -> Stat]
tree_slow_stats =
	[ const numcopies_stats
	, const reposizes_stats_tree
	, const reposizes_total
	]

file_stats :: FilePath -> Key -> [Stat]
file_stats f k =
	[ file_name f
	, key_size k
	, key_name k
	, content_present k
	]

remote_fast_stats :: Remote -> [Stat]
remote_fast_stats r = map (\s -> s r)
	[ remote_name
	, remote_cost
	, remote_type
	, remote_availabile
	]

uuid_fast_stats :: UUID -> [Stat]
uuid_fast_stats u = map (\s -> s u)
	[ repo_uuid
	, repo_description
	, repo_trust
	]

uuid_slow_stats :: UUID -> [Stat]
uuid_slow_stats u = map (\s -> s u)
	[ repo_annex_keys
	, repo_annex_size
	]

stat :: String -> (String -> StatState String) -> Stat
stat desc a = return $ Just (desc, a desc)

-- The json simply contains the same string that is displayed.
simpleStat :: String -> StatState String -> Stat
simpleStat desc getval = stat desc $ json id getval

nostat :: Stat
nostat = return Nothing

json :: ToJSON' j => (j -> String) -> StatState j -> String -> StatState String
json fmt a desc = do
	j <- a
	lift $ maybeShowJSON $ JSONChunk [(desc, j)]
	return $ fmt j

nojson :: StatState String -> String -> StatState String
nojson a _ = a

showStat :: Stat -> StatState ()
showStat s = maybe noop calc =<< s
  where
	calc (desc, a) = do
		(lift . showHeader . encodeBS) desc
		lift . showRaw . encodeBS =<< a

repo_list :: TrustLevel -> Stat
repo_list level = stat n $ nojson $ lift $ do
	us <- filter (/= NoUUID) . M.keys 
		<$> (M.union <$> (M.map fromUUIDDesc <$> uuidDescMap) <*> Remote.remoteMap Remote.name)
	rs <- fst <$> trustPartition level us
	countRepoList (length rs)
		-- This also handles json display.
		<$> Remote.prettyPrintUUIDs n rs
  where
	n = showTrustLevel level ++ " repositories"

countRepoList :: Int -> String -> String
countRepoList _ [] = "0"
countRepoList n s = show n ++ "\n" ++ beginning s

dispRepoList :: String -> String
dispRepoList [] = ""
dispRepoList s = "\n" ++ beginning s

dir_name :: FilePath -> Stat
dir_name dir = simpleStat "directory" $ pure dir

tree_name :: String -> Stat
tree_name t = simpleStat "tree" $ pure t

file_name :: FilePath -> Stat
file_name file = simpleStat "file" $ pure file

remote_name :: Remote -> Stat
remote_name r = simpleStat "remote" $ pure (Remote.name r)

repo_description :: UUID -> Stat
repo_description = simpleStat "description" . lift . Remote.prettyUUID

repo_uuid :: UUID -> Stat
repo_uuid = simpleStat "uuid" . pure . fromUUID

repo_trust :: UUID -> Stat
repo_trust u = simpleStat "trust" $ lift $ showTrustLevel <$> lookupTrust u

remote_cost :: Remote -> Stat
remote_cost r = simpleStat "cost" $ pure $
	show $ Remote.cost r

remote_type :: Remote -> Stat
remote_type r = simpleStat "type" $ pure $
	Remote.typename $ Remote.remotetype r

remote_availabile :: Remote -> Stat
remote_availabile r = simpleStat "available" $ lift $
	either show (\av -> boolConfig (av /= Unavailable))
		<$> tryNonAsync (Remote.availability r)

local_annex_keys :: Stat
local_annex_keys = stat "local annex keys" $ json show $
	countKeys <$> cachedPresentData

local_annex_size :: Stat
local_annex_size = simpleStat "local annex size" $
	showSizeKeys =<< cachedPresentData

-- "remote" is in the name for JSON backwards-compatibility
repo_annex_keys :: UUID -> Stat
repo_annex_keys u = stat "remote annex keys" $ \d ->
	cachedRemoteData u >>= \case
		Right rd -> json show (pure (countKeys rd)) d
		Left n-> json id (pure n) d

-- "remote" is in the name for JSON backwards-compatibility
repo_annex_size :: UUID -> Stat
repo_annex_size u = simpleStat "remote annex size" $
	cachedRemoteData u >>= \case
		Right d -> showSizeKeys d
		Left n -> pure n

known_annex_files :: Bool -> Stat
known_annex_files isworktree = 
	stat ("annexed files in " ++ treeDesc isworktree) $ json show $
		countKeys <$> cachedReferencedData

known_annex_size :: Bool -> Stat
known_annex_size isworktree = 
	simpleStat ("size of annexed files in " ++ treeDesc isworktree) $
		showSizeKeys =<< cachedReferencedData

total_annex_size :: Stat
total_annex_size = 
	simpleStat "combined annex size of all repositories" $
		showSizeKeys . fromMaybe mempty . allRepoData
		=<< cachedAllRepoData
  
treeDesc :: Bool -> String
treeDesc True = "working tree"
treeDesc False = "tree"

tmp_size :: Stat
tmp_size = staleSize "temporary object directory size" gitAnnexTmpObjectDir

bad_data_size :: Stat
bad_data_size = staleSize "bad keys size" gitAnnexBadDir

key_size :: Key -> Stat
key_size k = simpleStat "size" $ showSizeKeys $ addKey k emptyKeyInfo

key_name :: Key -> Stat
key_name k = simpleStat "key" $ pure $ serializeKey k

content_present :: Key -> Stat
content_present k = stat "present" $ json boolConfig $ lift $ inAnnex k

bloom_info :: Stat
bloom_info = simpleStat "bloom filter size" $ do
	localkeys <- countKeys <$> cachedPresentData
	capacity <- fromIntegral <$> lift bloomCapacity
	let note = aside $
		if localkeys >= capacity
		then "appears too small for this repository; adjust annex.bloomcapacity"
		else showPercentage 1 (percentage capacity localkeys) ++ " full"

	-- Two bloom filters are used at the same time when running
	-- git-annex unused, so double the size of one.
	sizer <- mkSizer
	size <- sizer committeeUnits False . (* 2) . fromIntegral . fst <$>
		lift bloomBitsHashes

	return $ size ++ note

transfer_list :: Stat
transfer_list = stat desc $ nojson $ lift $ do
	uuidmap <- Remote.remoteMap id
	ts <- getTransfers
	maybeShowJSON $ JSONChunk [(desc, V.fromList $ map (uncurry jsonify) ts)]
	qp <- coreQuotePath <$> Annex.getGitConfig
	return $ if null ts
		then "none"
		else multiLine $
			map (uncurry $ line qp uuidmap) $ sort ts
  where
	desc = "transfers in progress"
	line qp uuidmap t i = unwords
		[ decodeBS $ fromShort (formatDirection (transferDirection t)) <> "ing"
		, decodeBS $ quote qp $ actionItemDesc $ mkActionItem
			(transferKey t, associatedFile i)
		, if transferDirection t == Upload then "to" else "from"
		, maybe (fromUUID $ transferUUID t) Remote.name $
			M.lookup (transferUUID t) uuidmap
		]
	jsonify t i = object $ map (\(k, v) -> (textKey (packString k), v)) $
		[ ("transfer", toJSON' (fromShort (formatDirection (transferDirection t))))
		, ("key", toJSON' (transferKey t))
		, ("file", toJSON' ((fromOsPath <$> afile) :: Maybe FilePath))
		, ("remote", toJSON' (fromUUID (transferUUID t) :: String))
		]
	  where
		AssociatedFile afile = associatedFile i

disk_size :: Stat
disk_size = simpleStat "available local disk space" $
	calcfree
		<$> (lift $ annexDiskReserve <$> Annex.getGitConfig)
		<*> (lift $ inRepo $ getDiskFree . fromOsPath . gitAnnexDir)
		<*> mkSizer
  where
	calcfree reserve (Just have) sizer = unwords
		[ sizer storageUnits False $ nonneg $ have - reserve
		, "(+" ++ sizer storageUnits False reserve
		, "reserved)"
		]			
	calcfree _ _ _ = "unknown"

	nonneg x
		| x >= 0 = x
		| otherwise = 0

backend_usage :: Stat
backend_usage = stat "backend usage" $ json fmt $
	ObjectMap . (M.mapKeys (decodeBS . formatKeyVariety)) . backendsKeys
		<$> cachedReferencedData
  where
	fmt = multiLine . map (\(b, n) -> b ++ ": " ++ show n) . sort . M.toList . fromObjectMap

numcopies_stats :: Stat
numcopies_stats = stat "numcopies stats" $ json fmt $
	calc <$> (maybe M.empty numCopiesVarianceMap <$> cachedNumCopiesStats)
  where
	calc = V.fromList
		. map (\(variance, count) -> (show variance, count)) 
		. sortBy (flip (comparing fst))
		. M.toList
	fmt = multiLine 
		. map (\(variance, count) -> "numcopies " ++ variance ++ ": " ++ show count)
		. V.toList

reposizes_stats_tree :: Stat
reposizes_stats_tree = reposizes_stats True "repositories containing these files"
	=<< cachedRepoData

reposizes_stats_global :: Stat
reposizes_stats_global = reposizes_stats False "annex sizes of repositories" 
	. repoData =<< cachedAllRepoData

reposizes_stats :: Bool -> String -> M.Map UUID KeyInfo -> Stat
reposizes_stats count desc m = stat desc $ nojson $ do
	sizer <- mkSizer
	let l = map (\(u, kd) -> (u, sizer storageUnits True (sizeKeys kd))) $
		sortBy (flip (comparing (sizeKeys . snd))) $
		M.toList m
	let maxlen = maximum (map (length . snd) l)
	descm <- lift Remote.uuidDescriptions
	-- This also handles json display.
	s <- lift $ Remote.prettyPrintUUIDsWith (Just "size") desc descm
		(\sz -> Just $ show sz ++ ": ") $
		map (\(u, sz) -> (u, Just $ mkdisp sz maxlen)) l
	return $ if count
		then countRepoList (length l) s
		else dispRepoList s
  where
	mkdisp sz maxlen = DualDisp
		{ dispNormal = lpad maxlen sz
		, dispJson = sz
		}
	lpad n s = (replicate (n - length s) ' ') ++ s

reposizes_total :: Stat
reposizes_total = simpleStat "combined size of repositories containing these files" $
	showSizeKeys . mconcat . M.elems =<< cachedRepoData

cachedPresentData :: StatState KeyInfo
cachedPresentData = do
	s <- get
	case presentData s of
		Just v -> return v
		Nothing -> do
			matcher <- lift getKeyOnlyMatcher
			v <- foldl' (flip addKey) emptyKeyInfo
				<$> lift (listKeys' InAnnex (matchOnKey matcher))
			put s { presentData = Just v }
			return v

cachedRemoteData :: UUID -> StatState (Either String KeyInfo)
cachedRemoteData u = do
	s <- get
	case M.lookup u (repoData s) of
		Just v -> return (Right v)
		Nothing -> do
			matcher <- lift getKeyOnlyMatcher
			let combinedata d uk = finishCheck uk >>= \case
				Nothing -> return d
				Just k -> ifM (matchOnKey matcher k)
					( return (addKey k d)
					, return d
					)
			lift (loggedKeysFor' u) >>= \case
				Just (ks, cleanup) -> do
					v <- lift $ foldM combinedata emptyKeyInfo ks
					liftIO $ void cleanup
					put s { repoData = M.insert u v (repoData s) }
					return (Right v)
				Nothing -> return (Left "not available in this read-only repository with unmerged git-annex branches")

cachedReferencedData :: StatState KeyInfo
cachedReferencedData = do
	s <- get
	case referencedData s of
		Just v -> return v
		Nothing -> do
			matcher <- lift getKeyOnlyMatcher
			let combinedata k _f d = ifM (matchOnKey matcher k)
				( return (addKey k d)
				, return d
				)
			!v <- lift $ Command.Unused.withKeysReferenced
				emptyKeyInfo combinedata
			put s { referencedData = Just v }
			return v

cachedAllRepoData :: StatState StatInfo
cachedAllRepoData = do
	s <- get
	case allRepoData s of
		Just _ -> return s
		Nothing -> do
			s' <- ifM (lift Limit.limited)
				( limitedcalc s
				, usereposizes s
				)
			put s'
			return s'
  where
 	usereposizes s = do
		sizemap <- lift $ getRepoSizes True
		deadset <- lift $ S.fromList <$> trustGet DeadTrusted
		let sizemap' = M.filter (> 0) $ M.withoutKeys sizemap deadset
		lift $ unlessM (null <$> getUnmergedRefs)
			warnunmerged
		return $ s
			{ allRepoData = Just $
				convsize (sum (M.elems sizemap'))
			, repoData = M.map convsize sizemap'
			}
	
	limitedcalc s = do
		matcher <- lift getKeyOnlyMatcher
		r <- lift $ overLocationLogs False False (emptyKeyInfo, mempty) $ \k locs (d, rd) -> do
			ifM (matchOnKey matcher k)
				( do
					alivelocs <- snd
						<$> trustPartition DeadTrusted locs
					let !d' = addKeyCopies (genericLength alivelocs) k d
					let !rd' = foldl' (flip (accumrepodata k)) rd alivelocs
					return (d', rd')
				, return (d, rd)
				)
		(!(d, rd), _) <- case r of
			NoUnmergedBranches v ->
				return v
			UnmergedBranches v -> do
				lift warnunmerged
				return v
		return $ s { allRepoData = Just d, repoData = rd }

	accumrepodata k = M.alter (Just . addKey k . fromMaybe emptyKeyInfo)

	convsize (RepoSize sz) = emptyKeyInfo { sizeKeys = sz }
	
	warnunmerged = warning "There are unmerged git-annex branches. Information from those branches is not included here."

cachedNumCopiesStats :: StatState (Maybe NumCopiesStats)
cachedNumCopiesStats = numCopiesStats <$> get

cachedRepoData :: StatState (M.Map UUID KeyInfo)
cachedRepoData = repoData <$> get

getDirStatInfo :: InfoOptions -> FilePath -> Annex StatInfo
getDirStatInfo o dir = do
	fast <- Annex.getRead Annex.fast
	matcher <- Limit.getMatcher
	(presentdata, referenceddata, numcopiesstats, repodata) <-
		Command.Unused.withKeysFilesReferencedIn (toOsPath dir) initial
			(update matcher fast)
	return $ StatInfo
		(Just presentdata)
		(Just referenceddata)
		repodata
		Nothing
		(Just numcopiesstats)
		o
  where
	initial = (emptyKeyInfo, emptyKeyInfo, emptyNumCopiesStats, M.empty)
	update matcher fast key file vs@(presentdata, referenceddata, numcopiesstats, repodata) =
		ifM (matcher $ MatchingFile $ FileInfo file file (Just key))
			( do
				!presentdata' <- ifM (inAnnex key)
					( return $ addKey key presentdata
					, return presentdata
					)
				let !referenceddata' = addKey key referenceddata
				(!numcopiesstats', !repodata') <- if fast
					then return (numcopiesstats, repodata)
					else do
						locs <- Remote.keyLocations key
						nc <- updateNumCopiesStats file numcopiesstats locs
						return (nc, updateRepoData key locs repodata)
				return $! (presentdata', referenceddata', numcopiesstats', repodata')
			, return vs
			)

getTreeStatInfo :: InfoOptions -> Git.Ref -> Annex (Maybe StatInfo)
getTreeStatInfo o r = do
	fast <- Annex.getRead Annex.fast
	-- git lstree filenames start with a leading "./" that prevents
	-- matching, and also things like --include are supposed to
	-- match relative to the current directory, which does not make
	-- sense when matching against files in some arbitrary tree.
	matcher <- getKeyOnlyMatcher
	(ls, cleanup) <- inRepo $ LsTree.lsTree
		LsTree.LsTreeRecursive
		(LsTree.LsTreeLong False)
		r
	(presentdata, referenceddata, repodata) <- go fast matcher ls initial
	ifM (liftIO cleanup)
		( return $ Just $
			StatInfo (Just presentdata) (Just referenceddata) repodata Nothing Nothing o
		, return Nothing
		)
  where
	initial = (emptyKeyInfo, emptyKeyInfo, M.empty)
	go _ _ [] vs = return vs
	go fast matcher (l:ls) vs@(presentdata, referenceddata, repodata) =
		catKey (LsTree.sha l) >>= \case
			Nothing -> go fast matcher ls vs
			Just key -> ifM (matchOnKey matcher key)
				( do
					!presentdata' <- ifM (inAnnex key)
						( return $ addKey key presentdata
						, return presentdata
						)
					let !referenceddata' = addKey key referenceddata
					!repodata' <- if fast
						then return repodata
						else do
							locs <- Remote.keyLocations key
							return (updateRepoData key locs repodata)
					go fast matcher ls $! (presentdata', referenceddata', repodata')
				, go fast matcher ls vs
				)

emptyKeyInfo :: KeyInfo
emptyKeyInfo = KeyInfo 0 0 0 M.empty

emptyNumCopiesStats :: NumCopiesStats
emptyNumCopiesStats = NumCopiesStats M.empty

addKey :: Key -> KeyInfo -> KeyInfo
addKey = addKeyCopies 1

addKeyCopies :: Integer -> Key -> KeyInfo -> KeyInfo
addKeyCopies numcopies key (KeyInfo count size unknownsize backends) =
	KeyInfo count' size' unknownsize' backends'
  where
	{- All calculations strict to avoid thunks when repeatedly
	 - applied to many keys. -}
	!count' = count + 1
	!backends' = M.insertWith (+) (fromKey keyVariety key) 1 backends
	!size' = maybe size (\sz -> sz * numcopies + size) ks
	!unknownsize' = maybe (unknownsize + 1) (const unknownsize) ks
	!ks = fromKey keySize key

updateRepoData :: Key -> [UUID] -> M.Map UUID KeyInfo -> M.Map UUID KeyInfo
updateRepoData key locs m = m'
  where
	!m' = M.unionWith (\_old new -> new) m $
		M.fromList $ zip locs (map update locs)
	update loc = addKey key (fromMaybe emptyKeyInfo $ M.lookup loc m)

updateNumCopiesStats :: OsPath -> NumCopiesStats -> [UUID] -> Annex NumCopiesStats
updateNumCopiesStats file (NumCopiesStats m) locs = do
	have <- trustExclude UnTrusted locs
	!variance <- Variance <$> numCopiesCheck' file (-) have
	let !m' = M.insertWith (+) variance 1 m
	let !ret = NumCopiesStats m'
	return ret

showSizeKeys :: KeyInfo -> StatState String
showSizeKeys d = do
	sizer <- mkSizer
	return $ total sizer ++ missingnote
  where
	total sizer = sizer storageUnits False $ sizeKeys d
	missingnote
		| unknownSizeKeys d == 0 = ""
		| otherwise = aside $
			"+ " ++ show (unknownSizeKeys d) ++
			" unknown size"

staleSize :: String -> (Git.Repo -> OsPath) -> Stat
staleSize label dirspec = go =<< lift (dirKeys dirspec)
  where
	go [] = nostat
	go keys = onsize =<< sum <$> keysizes keys
	onsize 0 = nostat
	onsize size = stat label $
		json (++ aside "clean up with git-annex unused") $ do
			sizer <- mkSizer
			return $ sizer storageUnits False size
	keysizes keys = do
		dir <- lift $ fromRepo dirspec
		liftIO $ forM keys $ \k -> 
			catchDefaultIO 0 $ getFileSize (dir </> keyFile k)

aside :: String -> String
aside s = " (" ++ s ++ ")"

multiLine :: [String] -> String
multiLine = concatMap (\l -> "\n\t" ++ l)

mkSizer :: StatState ([Unit] -> Bool -> ByteSize -> String)
mkSizer = ifM (bytesOption . infoOptions <$> get)
	( return (const $ const show)
	, return roughSize
	)
			
getKeyOnlyMatcher :: Annex (MatchInfo -> Annex Bool)
getKeyOnlyMatcher = do
	whenM (Limit.introspect matchNeedsFileName) $ do
		warning "File matching options cannot be applied when getting this info."
		giveup "Unable to continue."
	Limit.getMatcher

matchOnKey :: (MatchInfo -> Annex Bool) -> Key -> Annex Bool
matchOnKey matcher k = matcher $ MatchingInfo $ ProvidedInfo
	{ providedFilePath = Nothing
	, providedKey = Just k
	, providedFileSize = Nothing
	, providedMimeType = Nothing
	, providedMimeEncoding = Nothing
	, providedLinkType = Nothing
	}
