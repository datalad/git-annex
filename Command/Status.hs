{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Status where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Text.JSON

import Common.Annex
import qualified Types.Backend as B
import qualified Types.Remote as R
import qualified Remote
import qualified Command.Unused
import qualified Git
import qualified Annex
import Command
import Utility.DataUnits
import Annex.Content
import Types.Key
import Backend
import Logs.UUID
import Logs.Trust
import Remote

-- a named computation that produces a statistic
type Stat = StatState (Maybe (String, StatState String))

-- cached info that multiple Stats may need
data StatInfo = StatInfo
	{ keysPresentCache :: Maybe (Set Key)
	, keysReferencedCache :: Maybe (Set Key)
	}

-- a state monad for running Stats in
type StatState = StateT StatInfo Annex

def :: [Command]
def = [command "status" paramNothing seek
	"shows status information about the annex"]

seek :: [CommandSeek]
seek = [withNothing start]

{- Order is significant. Less expensive operations, and operations
 - that share data go together.
 -}
fast_stats :: [Stat]
fast_stats = 
	[ supported_backends
	, supported_remote_types
	, remote_list Trusted "trusted"
	, remote_list SemiTrusted "semitrusted"
	, remote_list UnTrusted "untrusted"
	, remote_list DeadTrusted "dead"
	]
slow_stats :: [Stat]
slow_stats = 
	[ tmp_size
	, bad_data_size
	, local_annex_keys
	, local_annex_size
	, visible_annex_keys
	, visible_annex_size
	, backend_usage
	]

start :: CommandStart
start = do
	fast <- Annex.getState Annex.fast
	let stats = if fast then fast_stats else fast_stats ++ slow_stats
	showCustom "status" $ do
		evalStateT (mapM_ showStat stats) (StatInfo Nothing Nothing)
		return True
	stop

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
showStat s = calc =<< s
	where
		calc (Just (desc, a)) = do
			(lift . showHeader) desc
			lift . showRaw =<< a
		calc Nothing = return ()

supported_backends :: Stat
supported_backends = stat "supported backends" $ json unwords $
	return $ map B.name Backend.list

supported_remote_types :: Stat
supported_remote_types = stat "supported remote types" $ json unwords $
	return $ map R.typename Remote.remoteTypes

remote_list :: TrustLevel -> String -> Stat
remote_list level desc = stat n $ nojson $ lift $ do
	us <- M.keys <$> (M.union <$> uuidMap <*> remoteMap)
	rs <- fst <$> trustPartition level us
	s <- prettyPrintUUIDs n rs
	return $ if null s then "0" else show (length rs) ++ "\n" ++ beginning s
	where
		n = desc ++ " repositories"

local_annex_size :: Stat
local_annex_size = stat "local annex size" $ json id $
	keySizeSum <$> cachedKeysPresent

local_annex_keys :: Stat
local_annex_keys = stat "local annex keys" $ json show $
	S.size <$> cachedKeysPresent

visible_annex_size :: Stat
visible_annex_size = stat "visible annex size" $ json id $
	keySizeSum <$> cachedKeysReferenced

visible_annex_keys :: Stat
visible_annex_keys = stat "visible annex keys" $ json show $
	S.size <$> cachedKeysReferenced

tmp_size :: Stat
tmp_size = staleSize "temporary directory size" gitAnnexTmpDir

bad_data_size :: Stat
bad_data_size = staleSize "bad keys size" gitAnnexBadDir

backend_usage :: Stat
backend_usage = stat "backend usage" $ nojson $
	calc <$> cachedKeysReferenced <*> cachedKeysPresent
	where
		calc a b = pp "" $ reverse . sort $ map swap $ splits $ S.toList $ S.union a b
		splits :: [Key] -> [(String, Integer)]
		splits ks = M.toList $ M.fromListWith (+) $ map tcount ks
		tcount k = (keyBackendName k, 1)
		swap (a, b) = (b, a)
		pp c [] = c
		pp c ((n, b):xs) = "\n\t" ++ b ++ ": " ++ show n ++ pp c xs

cachedKeysPresent :: StatState (Set Key)
cachedKeysPresent = do
	s <- get
	case keysPresentCache s of
		Just v -> return v
		Nothing -> do
			keys <- S.fromList <$> lift getKeysPresent
			put s { keysPresentCache = Just keys }
			return keys

cachedKeysReferenced :: StatState (Set Key)
cachedKeysReferenced = do
	s <- get
	case keysReferencedCache s of
		Just v -> return v
		Nothing -> do
			keys <- S.fromList <$> lift Command.Unused.getKeysReferenced
			put s { keysReferencedCache = Just keys }
			return keys

keySizeSum :: Set Key -> String
keySizeSum s = total ++ missingnote
	where
		knownsizes = mapMaybe keySize $ S.toList s
		total = roughSize storageUnits False $ sum knownsizes
		missing = S.size s - genericLength knownsizes
		missingnote
			| missing == 0 = ""
			| otherwise = aside $
				"+ " ++ show missing ++
				" keys of unknown size"

staleSize :: String -> (Git.Repo -> FilePath) -> Stat
staleSize label dirspec = do
	keys <- lift (Command.Unused.staleKeys dirspec)
	if null keys
		then nostat
		else stat label $ json (++ aside "clean up with git-annex unused") $
			return $ keySizeSum $ S.fromList keys

aside :: String -> String
aside s = " (" ++ s ++ ")"
