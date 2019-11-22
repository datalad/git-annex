{- git-annex command
 -
 - Copyright 2011-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}

module Command.Info where

import "mtl" Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.Ord
import qualified Data.Semigroup as Sem
import Prelude

import Command
import qualified Git
import qualified Annex
import qualified Remote
import qualified Types.Remote as Remote
import Utility.DataUnits
import Utility.DiskFree
import Annex.Content
import Annex.UUID
import Annex.CatFile
import Logs.UUID
import Logs.Trust
import Logs.Location
import Annex.NumCopies
import Remote
import Git.Config (boolConfig)
import qualified Git.LsTree as LsTree
import Utility.Percentage
import Utility.Aeson hiding (json)
import Types.Transfer
import Logs.Transfer
import Types.Key
import Types.TrustLevel
import Types.FileMatcher
import Types.ActionItem
import qualified Limit
import Messages.JSON (DualDisp(..), ObjectMap(..))
import Annex.BloomFilter
import qualified Command.Unused

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
	, numCopiesStats :: Maybe NumCopiesStats
	, infoOptions :: InfoOptions
	}

emptyStatInfo :: InfoOptions -> StatInfo
emptyStatInfo = StatInfo Nothing Nothing M.empty Nothing

-- a state monad for running Stats in
type StatState = StateT StatInfo Annex

cmd :: Command
cmd = noCommit $ withGlobalOptions [jsonOptions, annexedMatchingOptions] $
	command "info" SectionQuery
		"information about an item or the repository"
		(paramRepeating paramItem) (seek <$$> optParser)

data InfoOptions = InfoOptions
	{ infoFor :: CmdParams
	, bytesOption :: Bool
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser InfoOptions
optParser desc = InfoOptions
	<$> cmdParams desc
	<*> switch
		( long "bytes"
		<> help "display file sizes in bytes"
		)
	<*> parseBatchOption

seek :: InfoOptions -> CommandSeek
seek o = case batchOption o of
	NoBatch -> withWords (commandAction . start o) (infoFor o)
	Batch fmt -> batchInput fmt Right (itemInfo o)

start :: InfoOptions -> [String] -> CommandStart
start o [] = do
	globalInfo o
	stop
start o ps = do
	mapM_ (itemInfo o) ps
	stop

globalInfo :: InfoOptions -> Annex ()
globalInfo o = do
	disallowMatchingOptions
	u <- getUUID
	whenM ((==) DeadTrusted <$> lookupTrust u) $
		earlyWarning "Warning: This repository is currently marked as dead."
	stats <- selStats global_fast_stats global_slow_stats
	showCustom "info" $ do
		evalStateT (mapM_ showStat stats) (emptyStatInfo o)
		return True

itemInfo :: InfoOptions -> String -> Annex ()
itemInfo o p = ifM (isdir p)
	( dirInfo o p
	, do
		disallowMatchingOptions
		v <- Remote.byName' p
		case v of
			Right r -> remoteInfo o r
			Left _ -> do
				v' <- Remote.nameToUUID' p
				case v' of
					Right u -> uuidInfo o u
					Left _ -> ifAnnexed p 
						(fileInfo o p)
						(treeishInfo o p)
	)
  where
	isdir = liftIO . catchBoolIO . (isDirectory <$$> getFileStatus)

noInfo :: String -> Annex ()
noInfo s = do
	showStart "info" s
	showNote $ "not a directory or an annexed file or a treeish or a remote or a uuid"
	showEndFail

disallowMatchingOptions :: Annex ()
disallowMatchingOptions = whenM Limit.limited $
	giveup "File matching options can only be used when getting info on a directory."

dirInfo :: InfoOptions -> FilePath -> Annex ()
dirInfo o dir = showCustom (unwords ["info", dir]) $ do
	stats <- selStats
		(tostats (dir_name:tree_fast_stats True))
		(tostats tree_slow_stats)
	evalStateT (mapM_ showStat stats) =<< getDirStatInfo o dir
	return True
  where
	tostats = map (\s -> s dir)

treeishInfo :: InfoOptions -> String -> Annex ()
treeishInfo o t = do
	mi <- getTreeStatInfo o (Git.Ref t)
	case mi of
		Nothing -> noInfo t
		Just i -> showCustom (unwords ["info", t]) $ do
			stats <- selStats 
				(tostats (tree_name:tree_fast_stats False)) 
				(tostats tree_slow_stats)
			evalStateT (mapM_ showStat stats) i
			return True
  where
	tostats = map (\s -> s t)

fileInfo :: InfoOptions -> FilePath -> Key -> Annex ()
fileInfo o file k = showCustom (unwords ["info", file]) $ do
	evalStateT (mapM_ showStat (file_stats file k)) (emptyStatInfo o)
	return True

remoteInfo :: InfoOptions -> Remote -> Annex ()
remoteInfo o r = showCustom (unwords ["info", Remote.name r]) $ do
	i <- map (\(k, v) -> simpleStat k (pure v)) <$> Remote.getInfo r
	let u = Remote.uuid r
	l <- selStats 
		(uuid_fast_stats u ++ remote_fast_stats r ++ i)
		(uuid_slow_stats u)
	evalStateT (mapM_ showStat l) (emptyStatInfo o)
	return True

uuidInfo :: InfoOptions -> UUID -> Annex ()
uuidInfo o u = showCustom (unwords ["info", fromUUID u]) $ do
	l <- selStats (uuid_fast_stats u) (uuid_slow_stats u)
	evalStateT (mapM_ showStat l) (emptyStatInfo o)
	return True

selStats :: [Stat] -> [Stat] -> Annex [Stat]
selStats fast_stats slow_stats = do
	fast <- Annex.getState Annex.fast
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
	, bloom_info
	, backend_usage
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
	, const reposizes_stats
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
		(lift . showHeader) desc
		lift . showRaw =<< a

repo_list :: TrustLevel -> Stat
repo_list level = stat n $ nojson $ lift $ do
	us <- filter (/= NoUUID) . M.keys 
		<$> (M.union <$> (M.map fromUUIDDesc <$> uuidDescMap) <*> remoteMap Remote.name)
	rs <- fst <$> trustPartition level us
	countRepoList (length rs)
		-- This also handles json display.
		<$> prettyPrintUUIDs n rs
  where
	n = showTrustLevel level ++ " repositories"

countRepoList :: Int -> String -> String
countRepoList _ [] = "0"
countRepoList n s = show n ++ "\n" ++ beginning s

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

local_annex_keys :: Stat
local_annex_keys = stat "local annex keys" $ json show $
	countKeys <$> cachedPresentData

local_annex_size :: Stat
local_annex_size = simpleStat "local annex size" $
	showSizeKeys =<< cachedPresentData

-- "remote" is in the name for JSON backwards-compatibility
repo_annex_keys :: UUID -> Stat
repo_annex_keys u = stat "remote annex keys" $ json show $
	countKeys <$> cachedRemoteData u

-- "remote" is in the name for JSON backwards-compatibility
repo_annex_size :: UUID -> Stat
repo_annex_size u = simpleStat "remote annex size" $
	showSizeKeys =<< cachedRemoteData u

known_annex_files :: Bool -> Stat
known_annex_files isworktree = 
	stat ("annexed files in " ++ treeDesc isworktree) $ json show $
		countKeys <$> cachedReferencedData

known_annex_size :: Bool -> Stat
known_annex_size isworktree = 
	simpleStat ("size of annexed files in " ++ treeDesc isworktree) $
		showSizeKeys =<< cachedReferencedData
  
treeDesc :: Bool -> String
treeDesc True = "working tree"
treeDesc False = "tree"

tmp_size :: Stat
tmp_size = staleSize "temporary object directory size" gitAnnexTmpObjectDir

bad_data_size :: Stat
bad_data_size = staleSize "bad keys size" gitAnnexBadDir

key_size :: Key -> Stat
key_size k = simpleStat "size" $ showSizeKeys $ foldKeys [k]

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
	size <- sizer memoryUnits False . (* 2) . fromIntegral . fst <$>
		lift bloomBitsHashes

	return $ size ++ note

transfer_list :: Stat
transfer_list = stat desc $ nojson $ lift $ do
	uuidmap <- Remote.remoteMap id
	ts <- getTransfers
	maybeShowJSON $ JSONChunk [(desc, V.fromList $ map (uncurry jsonify) ts)]
	return $ if null ts
		then "none"
		else multiLine $
			map (uncurry $ line uuidmap) $ sort ts
  where
	desc = "transfers in progress"
	line uuidmap t i = unwords
		[ formatDirection (transferDirection t) ++ "ing"
		, actionItemDesc $ mkActionItem
			(transferKey t, associatedFile i)
		, if transferDirection t == Upload then "to" else "from"
		, maybe (fromUUID $ transferUUID t) Remote.name $
			M.lookup (transferUUID t) uuidmap
		]
	jsonify t i = object $ map (\(k, v) -> (packString k, v)) $
		[ ("transfer", toJSON' (formatDirection (transferDirection t)))
		, ("key", toJSON' (transferKey t))
		, ("file", toJSON' afile)
		, ("remote", toJSON' (fromUUID (transferUUID t) :: String))
		]
	  where
		AssociatedFile afile = associatedFile i

disk_size :: Stat
disk_size = simpleStat "available local disk space" $
	calcfree
		<$> (lift $ annexDiskReserve <$> Annex.getGitConfig)
		<*> (lift $ inRepo $ getDiskFree . gitAnnexDir)
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

reposizes_stats :: Stat
reposizes_stats = stat desc $ nojson $ do
	sizer <- mkSizer
	l <- map (\(u, kd) -> (u, sizer storageUnits True (sizeKeys kd)))
		. sortBy (flip (comparing (sizeKeys . snd)))
		. M.toList
		<$> cachedRepoData
	let maxlen = maximum (map (length . snd) l)
	descm <- lift uuidDescriptions
	-- This also handles json display.
	s <- lift $ prettyPrintUUIDsWith (Just "size") desc descm (Just . show) $
		map (\(u, sz) -> (u, Just $ mkdisp sz maxlen)) l
	return $ countRepoList (length l) s
  where
	desc = "repositories containing these files"
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
			v <- foldKeys <$> lift (listKeys InAnnex)
			put s { presentData = Just v }
			return v

cachedRemoteData :: UUID -> StatState KeyInfo
cachedRemoteData u = do
	s <- get
	case M.lookup u (repoData s) of
		Just v -> return v
		Nothing -> do
			let combinedata d uk = finishCheck uk >>= \case
				Nothing -> return d
				Just k -> return $ addKey k d
			v <- lift $ foldM combinedata emptyKeyInfo
				=<< loggedKeysFor' u
			put s { repoData = M.insert u v (repoData s) }
			return v

cachedReferencedData :: StatState KeyInfo
cachedReferencedData = do
	s <- get
	case referencedData s of
		Just v -> return v
		Nothing -> do
			!v <- lift $ Command.Unused.withKeysReferenced
				emptyKeyInfo addKey
			put s { referencedData = Just v }
			return v

-- currently only available for directory info
cachedNumCopiesStats :: StatState (Maybe NumCopiesStats)
cachedNumCopiesStats = numCopiesStats <$> get

-- currently only available for directory info
cachedRepoData :: StatState (M.Map UUID KeyInfo)
cachedRepoData = repoData <$> get

getDirStatInfo :: InfoOptions -> FilePath -> Annex StatInfo
getDirStatInfo o dir = do
	fast <- Annex.getState Annex.fast
	matcher <- Limit.getMatcher
	(presentdata, referenceddata, numcopiesstats, repodata) <-
		Command.Unused.withKeysFilesReferencedIn dir initial
			(update matcher fast)
	return $ StatInfo (Just presentdata) (Just referenceddata) repodata (Just numcopiesstats) o
  where
	initial = (emptyKeyInfo, emptyKeyInfo, emptyNumCopiesStats, M.empty)
	update matcher fast key file vs@(presentdata, referenceddata, numcopiesstats, repodata) =
		ifM (matcher $ MatchingFile $ FileInfo file file)
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
	fast <- Annex.getState Annex.fast
	(ls, cleanup) <- inRepo $ LsTree.lsTree LsTree.LsTreeRecursive r
	(presentdata, referenceddata, repodata) <- go fast ls initial
	ifM (liftIO cleanup)
		( return $ Just $
			StatInfo (Just presentdata) (Just referenceddata) repodata Nothing o
		, return Nothing
		)
  where
	initial = (emptyKeyInfo, emptyKeyInfo, M.empty)
	go _ [] vs = return vs
	go fast (l:ls) vs@(presentdata, referenceddata, repodata) = do
		mk <- catKey (LsTree.sha l)
		case mk of
			Nothing -> go fast ls vs
			Just key -> do
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
				go fast ls $! (presentdata', referenceddata', repodata')

emptyKeyInfo :: KeyInfo
emptyKeyInfo = KeyInfo 0 0 0 M.empty

emptyNumCopiesStats :: NumCopiesStats
emptyNumCopiesStats = NumCopiesStats M.empty

foldKeys :: [Key] -> KeyInfo
foldKeys = foldl' (flip addKey) emptyKeyInfo

addKey :: Key -> KeyInfo -> KeyInfo
addKey key (KeyInfo count size unknownsize backends) =
	KeyInfo count' size' unknownsize' backends'
  where
	{- All calculations strict to avoid thunks when repeatedly
	 - applied to many keys. -}
	!count' = count + 1
	!backends' = M.insertWith (+) (fromKey keyVariety key) 1 backends
	!size' = maybe size (+ size) ks
	!unknownsize' = maybe (unknownsize + 1) (const unknownsize) ks
	ks = fromKey keySize key

updateRepoData :: Key -> [UUID] -> M.Map UUID KeyInfo -> M.Map UUID KeyInfo
updateRepoData key locs m = m'
  where
	!m' = M.unionWith (\_old new -> new) m $
		M.fromList $ zip locs (map update locs)
	update loc = addKey key (fromMaybe emptyKeyInfo $ M.lookup loc m)

updateNumCopiesStats :: FilePath -> NumCopiesStats -> [UUID] -> Annex NumCopiesStats
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

staleSize :: String -> (Git.Repo -> FilePath) -> Stat
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
		liftIO $ forM keys $ \k -> catchDefaultIO 0 $
			getFileSize (dir </> keyFile k)

aside :: String -> String
aside s = " (" ++ s ++ ")"

multiLine :: [String] -> String
multiLine = concatMap (\l -> "\n\t" ++ l)

mkSizer :: StatState ([Unit] -> Bool -> ByteSize -> String)
mkSizer = ifM (bytesOption . infoOptions <$> get)
	( return (const $ const show)
	, return roughSize
	)
