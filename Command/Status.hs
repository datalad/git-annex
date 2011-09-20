{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Status where

import Control.Monad.State
import Control.Applicative
import Data.Maybe
import System.IO
import Data.List
import qualified Data.Map as M

import qualified Annex
import qualified Types.Backend as B
import qualified Types.Remote as R
import qualified Remote
import qualified Command.Unused
import qualified Git
import Command
import Types
import Utility.DataUnits
import Utility.Conditional
import Content
import Types.Key
import Locations
import Backend
import Messages

-- a named computation that produces a statistic
type Stat = StatState (Maybe (String, Bool, StatState String))

-- cached info that multiple Stats may need
data StatInfo = StatInfo
	{ keysPresentCache :: Maybe (SizeList Key)
	, keysReferencedCache :: Maybe (SizeList Key)
	}

-- a state monad for running Stats in
type StatState = StateT StatInfo Annex

-- a list with a known length
-- (Integer is used for the length to avoid
-- blowing up if someone annexed billions of files..)
type SizeList a = ([a], Integer)

sizeList :: [a] -> SizeList a
sizeList l = (l, genericLength l)

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
	fastmode_note
	stop

fastmode_note :: Annex ()
fastmode_note = whenM (Annex.getState Annex.fast) $
	showLongNote "(*) approximate due to fast mode"

stat :: String -> Bool -> StatState String -> Stat
stat desc approx a = return $ Just (desc, approx, a)

nostat :: Stat
nostat = return Nothing

showStat :: Stat -> StatState ()
showStat s = calc =<< s
	where
		calc (Just (desc, approx, a)) = do
			fast <- lift $ Annex.getState Annex.fast
			let star = if fast && approx then "(*)" else ""
			liftIO $ putStr $ desc ++ star ++ ": "
			liftIO $ hFlush stdout
			liftIO . putStrLn =<< a
		calc Nothing = return ()

supported_backends :: Stat
supported_backends = stat "supported backends" False $ 
	return $ unwords $ map B.name Backend.list

supported_remote_types :: Stat
supported_remote_types = stat "supported remote types" False $
	return $ unwords $ map R.typename Remote.remoteTypes

local_annex_size :: Stat
local_annex_size = stat "local annex size" False $
	cachedKeysPresent >>= keySizeSum

total_annex_size :: Stat
total_annex_size = stat "total annex size" True $
	cachedKeysReferenced >>= keySizeSum

local_annex_keys :: Stat
local_annex_keys = stat "local annex keys" False $
	show . snd <$> cachedKeysPresent

total_annex_keys :: Stat
total_annex_keys = stat "total annex keys" True $
	show . snd <$> cachedKeysReferenced

tmp_size :: Stat
tmp_size = staleSize "temporary directory size" gitAnnexTmpDir

bad_data_size :: Stat
bad_data_size = staleSize "bad keys size" gitAnnexBadDir

backend_usage :: Stat
backend_usage = stat "backend usage" True $ usage <$> cachedKeysReferenced
	where
		usage (ks, _) = pp "" $ sort $ map swap $ splits ks
		splits :: [Key] -> [(String, Integer)]
		splits ks = M.toList $ M.fromListWith (+) $ map tcount ks
		tcount k = (keyBackendName k, 1)
		swap (a, b) = (b, a)
		pp c [] = c
		pp c ((n, b):xs) = "\n\t" ++ b ++ ": " ++ show n ++ pp c xs

cachedKeysPresent :: StatState (SizeList Key)
cachedKeysPresent = do
	s <- get
	case keysPresentCache s of
		Just v -> return v
		Nothing -> do
			keys <- lift getKeysPresent
			let v = sizeList keys
			put s { keysPresentCache = Just v }
			return v

cachedKeysReferenced :: StatState (SizeList Key)
cachedKeysReferenced = do
	s <- get
	case keysReferencedCache s of
		Just v -> return v
		Nothing -> do
			-- A given key may be referenced repeatedly,
			-- so nub is needed for accuracy, but is slow.
			keys <- lift Command.Unused.getKeysReferenced
			fast <- lift $ Annex.getState Annex.fast
			let v = sizeList $ if fast then keys else nub keys
			put s { keysReferencedCache = Just v }
			return v

keySizeSum :: SizeList Key -> StatState String
keySizeSum (keys, len) = do
	let knownsizes = mapMaybe keySize keys
	let total = roughSize storageUnits False $ sum knownsizes
	let missing = len - genericLength knownsizes
	return $ total ++
		if missing > 0
			then aside $ "but " ++ show missing ++ " keys have unknown size"
			else ""

staleSize :: String -> (Git.Repo -> FilePath) -> Stat
staleSize label dirspec = do
	keys <- lift (Command.Unused.staleKeys dirspec)
	if null keys
		then nostat
		else stat label False $ do
			s <- keySizeSum $ sizeList keys
			return $ s ++ aside "clean up with git-annex unused"

aside :: String -> String
aside s = "\t(" ++ s ++ ")"
