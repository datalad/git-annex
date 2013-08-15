{- git-annex unused log file
 -
 - Copyright 2010,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Unused (
	UnusedMap,
	writeUnusedLog,
	readUnusedLog,
	unusedKeys,
) where

import qualified Data.Map as M

import Common.Annex
import Types.Key
import Utility.Tmp

type UnusedMap = M.Map Int Key

writeUnusedLog :: FilePath -> [(Int, Key)] -> Annex ()
writeUnusedLog prefix l = do
	logfile <- fromRepo $ gitAnnexUnusedLog prefix
	liftIO $ viaTmp writeFile logfile $
		unlines $ map (\(n, k) -> show n ++ " " ++ key2file k) l

readUnusedLog :: FilePath -> Annex UnusedMap
readUnusedLog prefix = do
	f <- fromRepo $ gitAnnexUnusedLog prefix
	ifM (liftIO $ doesFileExist f)
		( M.fromList . mapMaybe parse . lines
			<$> liftIO (readFile f)
		, return M.empty
		)
  where
	parse line = case (readish tag, file2key rest) of
		(Just num, Just key) -> Just (num, key)
		_ -> Nothing
	  where
		(tag, rest) = separate (== ' ') line

unusedKeys :: Annex [Key]
unusedKeys = M.elems <$> readUnusedLog ""
