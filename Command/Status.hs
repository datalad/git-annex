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

import Annex.Common
import qualified Types.Backend as B
import qualified Types.Remote as R
import qualified Remote
import qualified Command.Unused
import qualified Git
import Command
import Utility.DataUnits
import Annex.Content
import Types.Key
import Backend
import UUID
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

command :: [Command]
command = [repoCommand "status" paramNothing seek
	"shows status information about the annex"]

seek :: [CommandSeek]
seek = [withNothing start]

{- Order is significant. Less expensive operations, and operations
 - that share data go together.
 -}
stats :: [Stat]
stats = 
	[ supported_backends
	, supported_remote_types
	, remote_list
	, tmp_size
	, bad_data_size
	, local_annex_keys
	, local_annex_size
	, total_annex_keys
	, total_annex_size
	, backend_usage
	]

start :: CommandStart
start = do
	evalStateT (mapM_ showStat stats) (StatInfo Nothing Nothing)
	stop

stat :: String -> StatState String -> Stat
stat desc a = return $ Just (desc, a)

nostat :: Stat
nostat = return Nothing

showStat :: Stat -> StatState ()
showStat s = calc =<< s
	where
		calc (Just (desc, a)) = do
			liftIO $ putStr $ desc ++ ": "
			liftIO $ hFlush stdout
			liftIO . putStrLn =<< a
		calc Nothing = return ()

supported_backends :: Stat
supported_backends = stat "supported backends" $ 
	return $ unwords $ map B.name Backend.list

supported_remote_types :: Stat
supported_remote_types = stat "supported remote types" $
	return $ unwords $ map R.typename Remote.remoteTypes

remote_list :: Stat
remote_list = stat "known repositories" $ lift $ do
	s <- prettyPrintUUIDs "repos" =<< M.keys <$> uuidMap
	return $ '\n':init s

local_annex_size :: Stat
local_annex_size = stat "local annex size" $
	keySizeSum <$> cachedKeysPresent

total_annex_size :: Stat
total_annex_size = stat "total annex size" $
	keySizeSum <$> cachedKeysReferenced

local_annex_keys :: Stat
local_annex_keys = stat "local annex keys" $
	show . S.size <$> cachedKeysPresent

total_annex_keys :: Stat
total_annex_keys = stat "total annex keys" $
	show . S.size <$> cachedKeysReferenced

tmp_size :: Stat
tmp_size = staleSize "temporary directory size" gitAnnexTmpDir

bad_data_size :: Stat
bad_data_size = staleSize "bad keys size" gitAnnexBadDir

backend_usage :: Stat
backend_usage = stat "backend usage" $ usage <$> cachedKeysReferenced
	where
		usage ks = pp "" $ reverse . sort $ map swap $ splits $ S.toList ks
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
		else stat label $ do
			let s = keySizeSum $ S.fromList keys
			return $ s ++ aside "clean up with git-annex unused"

aside :: String -> String
aside s = " (" ++ s ++ ")"
