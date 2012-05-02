{- git-annex unused log file
 -
 - Copyright 2010,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Unused (
	UnusedMap,
	UnusedMaps(..),
	writeUnusedLog,
	readUnusedLog,
	withUnusedMaps,
	startUnused,
) where

import qualified Data.Map as M

import Common.Annex
import Command
import Types.Key
import Utility.TempFile

writeUnusedLog :: FilePath -> [(Int, Key)] -> Annex ()
writeUnusedLog prefix l = do
	logfile <- fromRepo $ gitAnnexUnusedLog prefix
	liftIO $ viaTmp writeFile logfile $
		unlines $ map (\(n, k) -> show n ++ " " ++ show k) l

readUnusedLog :: FilePath -> Annex UnusedMap
readUnusedLog prefix = do
	f <- fromRepo $ gitAnnexUnusedLog prefix
	ifM (liftIO $ doesFileExist f)
		( M.fromList . catMaybes . map parse . lines
			<$> liftIO (readFile f)
		, return M.empty
		)
	where
		parse line =
			case (readish tag, readKey rest) of
				(Just num, Just key) -> Just (num, key)
				_ -> Nothing
			where
				(tag, rest) = separate (== ' ') line

type UnusedMap = M.Map Int Key

data UnusedMaps = UnusedMaps
	{ unusedMap :: UnusedMap
	, unusedBadMap :: UnusedMap
	, unusedTmpMap :: UnusedMap
	}

{- Read unused logs once, and pass the maps to each start action. -}
withUnusedMaps :: (UnusedMaps -> Int -> CommandStart) -> CommandSeek
withUnusedMaps a params = do
	unused <- readUnusedLog ""
	unusedbad <- readUnusedLog "bad"
	unusedtmp <- readUnusedLog "tmp"
	return $ map (a $ UnusedMaps unused unusedbad unusedtmp) $
		concatMap unusedSpec params

unusedSpec :: String -> [Int]
unusedSpec spec
	| "-" `isInfixOf` spec = range $ separate (== '-') spec
	| otherwise = catMaybes [readish spec]
	where
		range (a, b) = case (readish a, readish b) of
			(Just x, Just y) -> [x..y]
			_ -> []

{- Start action for unused content. Finds the number in the maps, and
 - calls either of 3 actions, depending on the type of unused file. -}
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
		search [] = stop
		search ((m, a):rest) =
			case M.lookup n m of
				Nothing -> search rest
				Just key -> do
					showStart message (show n)
					next $ a key
