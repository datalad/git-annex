{- git-annex unused log file
 -
 - This file is stored locally in .git/annex/, not in the git-annex branch.
 -
 - The format: "int key timestamp"
 -
 - The int is a short, stable identifier that the user can use to
 - refer to this key. (Equivilant to a filename.)
 -
 - The timestamp indicates when the key was first determined to be unused.
 - Older versions of the log omit the timestamp.
 -
 - Copyright 2010-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Logs.Unused (
	UnusedMap,
	updateUnusedLog,
	readUnusedLog,
	readUnusedMap,
	dateUnusedLog,
	unusedKeys,
	unusedKeys',
	setUnusedKeys,
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Clock.POSIX
import Data.Time

import Annex.Common
import qualified Annex
import Utility.Tmp
import Logs.TimeStamp

-- everything that is stored in the unused log
type UnusedLog = M.Map Key (Int, Maybe POSIXTime)

-- used to look up unused keys specified by the user
type UnusedMap = M.Map Int Key

log2map :: UnusedLog -> UnusedMap
log2map = M.fromList . map (\(k, (i, _t)) -> (i, k)) . M.toList

map2log :: POSIXTime -> UnusedMap -> UnusedLog
map2log t = M.fromList . map (\(i, k) -> (k, (i, Just t))) . M.toList

{- Only keeps keys that are in the new log, but uses any timestamps
 - those keys had in the old log. -}
preserveTimestamps :: UnusedLog -> UnusedLog -> UnusedLog
preserveTimestamps oldl newl = M.intersection (M.unionWith oldts oldl newl) newl
  where
	oldts _old@(_, ts) _new@(int, _) = (int, ts)

updateUnusedLog :: FilePath -> UnusedMap -> Annex ()
updateUnusedLog prefix m = do
	oldl <- readUnusedLog prefix
	newl <- preserveTimestamps oldl . flip map2log m <$> liftIO getPOSIXTime
	writeUnusedLog prefix newl

writeUnusedLog :: FilePath -> UnusedLog -> Annex ()
writeUnusedLog prefix l = do
	logfile <- fromRepo $ gitAnnexUnusedLog prefix
	liftIO $ viaTmp writeFile logfile $ unlines $ map format $ M.toList l
  where
	format (k, (i, Just t)) = show i ++ " " ++ key2file k ++ " " ++ show t
	format (k, (i, Nothing)) = show i ++ " " ++ key2file k

readUnusedLog :: FilePath -> Annex UnusedLog
readUnusedLog prefix = do
	f <- fromRepo $ gitAnnexUnusedLog prefix
	ifM (liftIO $ doesFileExist f)
		( M.fromList . mapMaybe parse . lines
			<$> liftIO (readFileStrict f)
		, return M.empty
		)
  where
	parse line = case (readish sint, file2key skey, parsePOSIXTime ts) of
		(Just int, Just key, mtimestamp) -> Just (key, (int, mtimestamp))
		_ -> Nothing
	  where
		(sint, rest) = separate (== ' ') line
		(rts, rskey) = separate (== ' ') (reverse rest)
		skey = reverse rskey
		ts = reverse rts

readUnusedMap :: FilePath -> Annex UnusedMap
readUnusedMap = log2map <$$> readUnusedLog

dateUnusedLog :: FilePath -> Annex (Maybe UTCTime)
#if MIN_VERSION_directory(1,2,0)
dateUnusedLog prefix = do
	f <- fromRepo $ gitAnnexUnusedLog prefix
	liftIO $ catchMaybeIO $ getModificationTime f
#else
-- old ghc's getModificationTime returned a ClockTime
dateUnusedLog _prefix = return Nothing
#endif

{- Set of unused keys. This is cached for speed. -}
unusedKeys :: Annex (S.Set Key)
unusedKeys = maybe (setUnusedKeys =<< unusedKeys') return
	=<< Annex.getState Annex.unusedkeys

unusedKeys' :: Annex [Key]
unusedKeys' = M.keys <$> readUnusedLog ""

setUnusedKeys :: [Key] -> Annex (S.Set Key)
setUnusedKeys ks = do
	let v = S.fromList ks
	Annex.changeState $ \s -> s { Annex.unusedkeys = Just v }
	return v
