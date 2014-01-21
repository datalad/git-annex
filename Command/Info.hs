{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Command.Info where

import "mtl" Control.Monad.State.Strict
import qualified Data.Map as M
import Text.JSON
import Data.Tuple
import Data.Ord
import System.PosixCompat.Files

import Common.Annex
import qualified Remote
import qualified Command.Unused
import qualified Git
import qualified Annex
import GitAnnex.Options
import Command
import Utility.DataUnits
import Utility.DiskFree
import Annex.Content
import Types.Key
import Logs.UUID
import Logs.Trust
import Config.NumCopies
import Remote
import Config
import Utility.Percentage
import Logs.Transfer
import Types.TrustLevel
import Types.FileMatcher
import qualified Limit

-- a named computation that produces a statistic
type Stat = StatState (Maybe (String, StatState String))

-- data about a set of keys
data KeyData = KeyData
	{ countKeys :: Integer
	, sizeKeys :: Integer
	, unknownSizeKeys :: Integer
	, backendsKeys :: M.Map String Integer
	}

data NumCopiesStats = NumCopiesStats
	{ numCopiesVarianceMap :: M.Map Variance Integer
	}

newtype Variance = Variance Int
	deriving (Eq, Ord)

instance Show Variance where
	show (Variance n)
		| n >= 0 = "numcopies +" ++ show n
		| otherwise = "numcopies " ++ show n

-- cached info that multiple Stats use
data StatInfo = StatInfo
	{ presentData :: Maybe KeyData
	, referencedData :: Maybe KeyData
	, numCopiesStats :: Maybe NumCopiesStats
	}

-- a state monad for running Stats in
type StatState = StateT StatInfo Annex

def :: [Command]
def = [noCommit $ withOptions [jsonOption] $
	command "info" paramPaths seek SectionQuery
	"shows general information about the annex"]

seek :: CommandSeek
seek = withWords start

start :: [FilePath] -> CommandStart
start [] = do
	globalInfo
	stop
start ps = do
	mapM_ localInfo =<< filterM isdir ps
	stop
  where
	isdir = liftIO . catchBoolIO . (isDirectory <$$> getFileStatus)

globalInfo :: Annex ()
globalInfo = do
	stats <- selStats global_fast_stats global_slow_stats
	showCustom "info" $ do
		evalStateT (mapM_ showStat stats) (StatInfo Nothing Nothing Nothing)
		return True

localInfo :: FilePath -> Annex ()
localInfo dir = showCustom (unwords ["info", dir]) $ do
	stats <- selStats (tostats local_fast_stats) (tostats local_slow_stats)
	evalStateT (mapM_ showStat stats) =<< getLocalStatInfo dir
	return True
  where
  	tostats = map (\s -> s dir)

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
	[ repository_mode
	, remote_list Trusted
	, remote_list SemiTrusted
	, remote_list UnTrusted
	, transfer_list
	, disk_size
	]
global_slow_stats :: [Stat]
global_slow_stats = 
	[ tmp_size
	, bad_data_size
	, local_annex_keys
	, local_annex_size
	, known_annex_files
	, known_annex_size
	, bloom_info
	, backend_usage
	]
local_fast_stats :: [FilePath -> Stat]
local_fast_stats =
	[ local_dir
	, const local_annex_keys
	, const local_annex_size
	, const known_annex_files
	, const known_annex_size
	]
local_slow_stats :: [FilePath -> Stat]
local_slow_stats =
	[ const numcopies_stats
	]

stat :: String -> (String -> StatState String) -> Stat
stat desc a = return $ Just (desc, a desc)

nostat :: Stat
nostat = return Nothing

json :: JSON j => (j -> String) -> StatState j -> String -> StatState String
json serialize a desc = do
	j <- a
	lift $ maybeShowJSON [(desc, j)]
	return $ serialize j

nojson :: StatState String -> String -> StatState String
nojson a _ = a

showStat :: Stat -> StatState ()
showStat s = maybe noop calc =<< s
  where
	calc (desc, a) = do
		(lift . showHeader) desc
		lift . showRaw =<< a

repository_mode :: Stat
repository_mode = stat "repository mode" $ json id $ lift $
	ifM isDirect 
		( return "direct", return "indirect" )

remote_list :: TrustLevel -> Stat
remote_list level = stat n $ nojson $ lift $ do
	us <- M.keys <$> (M.union <$> uuidMap <*> remoteMap Remote.name)
	rs <- fst <$> trustPartition level us
	s <- prettyPrintUUIDs n rs
	return $ if null s then "0" else show (length rs) ++ "\n" ++ beginning s
  where
	n = showTrustLevel level ++ " repositories"
	
local_dir :: FilePath -> Stat
local_dir dir = stat "directory" $ json id $ return dir

local_annex_keys :: Stat
local_annex_keys = stat "local annex keys" $ json show $
	countKeys <$> cachedPresentData

local_annex_size :: Stat
local_annex_size = stat "local annex size" $ json id $
	showSizeKeys <$> cachedPresentData

known_annex_files :: Stat
known_annex_files = stat "annexed files in working tree" $ json show $
	countKeys <$> cachedReferencedData

known_annex_size :: Stat
known_annex_size = stat "size of annexed files in working tree" $ json id $
	showSizeKeys <$> cachedReferencedData

tmp_size :: Stat
tmp_size = staleSize "temporary directory size" gitAnnexTmpDir

bad_data_size :: Stat
bad_data_size = staleSize "bad keys size" gitAnnexBadDir

bloom_info :: Stat
bloom_info = stat "bloom filter size" $ json id $ do
	localkeys <- countKeys <$> cachedPresentData
	capacity <- fromIntegral <$> lift Command.Unused.bloomCapacity
	let note = aside $
		if localkeys >= capacity
		then "appears too small for this repository; adjust annex.bloomcapacity"
		else showPercentage 1 (percentage capacity localkeys) ++ " full"

	-- Two bloom filters are used at the same time, so double the size
	-- of one.
	size <- roughSize memoryUnits False . (* 2) . fromIntegral . fst <$>
		lift Command.Unused.bloomBitsHashes

	return $ size ++ note

transfer_list :: Stat
transfer_list = stat "transfers in progress" $ nojson $ lift $ do
	uuidmap <- Remote.remoteMap id
	ts <- getTransfers
	return $ if null ts
		then "none"
		else multiLine $
			map (uncurry $ line uuidmap) $ sort ts
  where
	line uuidmap t i = unwords
		[ showLcDirection (transferDirection t) ++ "ing"
		, fromMaybe (key2file $ transferKey t) (associatedFile i)
		, if transferDirection t == Upload then "to" else "from"
		, maybe (fromUUID $ transferUUID t) Remote.name $
			M.lookup (transferUUID t) uuidmap
		]

disk_size :: Stat
disk_size = stat "available local disk space" $ json id $ lift $
	calcfree
		<$> (annexDiskReserve <$> Annex.getGitConfig)
		<*> inRepo (getDiskFree . gitAnnexDir)
  where
	calcfree reserve (Just have) = unwords
		[ roughSize storageUnits False $ nonneg $ have - reserve
		, "(+" ++ roughSize storageUnits False reserve
		, "reserved)"
		]			
	calcfree _ _ = "unknown"

	nonneg x
		| x >= 0 = x
		| otherwise = 0

backend_usage :: Stat
backend_usage = stat "backend usage" $ nojson $
	calc
		<$> (backendsKeys <$> cachedReferencedData)
		<*> (backendsKeys <$> cachedPresentData)
  where
	calc x y = multiLine $
		map (\(n, b) -> b ++ ": " ++ show n) $
		reverse $ sort $ map swap $ M.toList $
		M.unionWith (+) x y

numcopies_stats :: Stat
numcopies_stats = stat "numcopies stats" $ nojson $
	calc <$> (maybe M.empty numCopiesVarianceMap <$> cachedNumCopiesStats)
  where
	calc = multiLine
		. map (\(variance, count) -> show variance ++ ": " ++ show count)
		. reverse . sortBy (comparing snd) . M.toList

cachedPresentData :: StatState KeyData
cachedPresentData = do
	s <- get
	case presentData s of
		Just v -> return v
		Nothing -> do
			v <- foldKeys <$> lift getKeysPresent
			put s { presentData = Just v }
			return v

cachedReferencedData :: StatState KeyData
cachedReferencedData = do
	s <- get
	case referencedData s of
		Just v -> return v
		Nothing -> do
			!v <- lift $ Command.Unused.withKeysReferenced
				emptyKeyData addKey
			put s { referencedData = Just v }
			return v

-- currently only available for local info
cachedNumCopiesStats :: StatState (Maybe NumCopiesStats)
cachedNumCopiesStats = numCopiesStats <$> get

getLocalStatInfo :: FilePath -> Annex StatInfo
getLocalStatInfo dir = do
	fast <- Annex.getState Annex.fast
	matcher <- Limit.getMatcher
	(presentdata, referenceddata, numcopiesstats) <-
		Command.Unused.withKeysFilesReferencedIn dir initial
			(update matcher fast)
	return $ StatInfo (Just presentdata) (Just referenceddata) (Just numcopiesstats)
  where
	initial = (emptyKeyData, emptyKeyData, emptyNumCopiesStats)
	update matcher fast key file vs@(presentdata, referenceddata, numcopiesstats) =
		ifM (matcher $ MatchingFile $ FileInfo file file)
			( do
				!presentdata' <- ifM (inAnnex key)
					( return $ addKey key presentdata
					, return presentdata
					)
				let !referenceddata' = addKey key referenceddata
				!numcopiesstats' <- if fast
					then return numcopiesstats
					else updateNumCopiesStats key file numcopiesstats
				return $! (presentdata', referenceddata', numcopiesstats')
			, return vs
			)

emptyKeyData :: KeyData
emptyKeyData = KeyData 0 0 0 M.empty

emptyNumCopiesStats :: NumCopiesStats
emptyNumCopiesStats = NumCopiesStats M.empty

foldKeys :: [Key] -> KeyData
foldKeys = foldl' (flip addKey) emptyKeyData

addKey :: Key -> KeyData -> KeyData
addKey key (KeyData count size unknownsize backends) =
	KeyData count' size' unknownsize' backends'
  where
	{- All calculations strict to avoid thunks when repeatedly
	 - applied to many keys. -}
	!count' = count + 1
	!backends' = M.insertWith' (+) (keyBackendName key) 1 backends
	!size' = maybe size (+ size) ks
	!unknownsize' = maybe (unknownsize + 1) (const unknownsize) ks
	ks = keySize key

updateNumCopiesStats :: Key -> FilePath -> NumCopiesStats -> Annex NumCopiesStats
updateNumCopiesStats key file (NumCopiesStats m) = do
	!variance <- Variance <$> numCopiesCheck file key (-)
	let !m' = M.insertWith' (+) variance 1 m
	let !ret = NumCopiesStats m'
	return ret

showSizeKeys :: KeyData -> String
showSizeKeys d = total ++ missingnote
  where
	total = roughSize storageUnits False $ sizeKeys d
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
		json (++ aside "clean up with git-annex unused") $
			return $ roughSize storageUnits False size
	keysizes keys = do
		dir <- lift $ fromRepo dirspec
		liftIO $ forM keys $ \k -> catchDefaultIO 0 $
			fromIntegral . fileSize 
				<$> getFileStatus (dir </> keyFile k)

aside :: String -> String
aside s = " (" ++ s ++ ")"

multiLine :: [String] -> String
multiLine = concatMap (\l -> "\n\t" ++ l)
