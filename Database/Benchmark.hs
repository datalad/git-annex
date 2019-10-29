{- git-annex database benchmarks
 -
 - Copyright 2016-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Benchmark (benchmarkDbs) where

import Annex.Common
import Types.Benchmark
#ifdef WITH_BENCHMARK
import qualified Database.Keys.SQL as SQL
import qualified Database.Queue as H
import Database.Init
import Database.Types
import Utility.Tmp.Dir
import Git.FilePath
import Types.Key

import Criterion.Main
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B8
import System.Random
#endif

benchmarkDbs :: CriterionMode -> Annex ()
#ifdef WITH_BENCHMARK
benchmarkDbs mode = withTmpDirIn "." "benchmark" $ \tmpdir -> do
	-- benchmark different sizes of databases
	dbs <- mapM (benchDb tmpdir)
		[ 1000
		, 10000
		-- , 100000
		]
	liftIO $ runMode mode
		[ bgroup "keys database" $ flip concatMap dbs $ \db ->
			[ getAssociatedFilesHitBench db
			, getAssociatedFilesMissBench db
			, getAssociatedKeyHitBench db
			, getAssociatedKeyMissBench db
			, addAssociatedFileOldBench db
			, addAssociatedFileNewBench db
			]
		]
#else
benchmarkDbs _ = error "not built with criterion, cannot benchmark"
#endif

#ifdef WITH_BENCHMARK

getAssociatedFilesHitBench :: BenchDb -> Benchmark
getAssociatedFilesHitBench (BenchDb h num) = bench ("getAssociatedFiles from " ++ show num ++ " (hit)") $ nfIO $ do
	n <- getStdRandom (randomR (1,num))
	SQL.getAssociatedFiles (toIKey (keyN n)) (SQL.ReadHandle h)

getAssociatedFilesMissBench :: BenchDb -> Benchmark
getAssociatedFilesMissBench (BenchDb h num) = bench ("getAssociatedFiles from " ++ show num ++ " (miss)") $ nfIO $
	SQL.getAssociatedFiles (toIKey keyMiss) (SQL.ReadHandle h)

getAssociatedKeyHitBench :: BenchDb -> Benchmark
getAssociatedKeyHitBench (BenchDb h num) = bench ("getAssociatedKey from " ++ show num ++ " (hit)") $ nfIO $ do
	n <- getStdRandom (randomR (1,num))
	-- fromIKey because this ends up being used to get a Key
	map fromIKey <$> SQL.getAssociatedKey (fileN n) (SQL.ReadHandle h)

getAssociatedKeyMissBench :: BenchDb -> Benchmark
getAssociatedKeyMissBench (BenchDb h num) = bench ("getAssociatedKey from " ++ show num ++ " (miss)") $ nfIO $
	-- fromIKey because this ends up being used to get a Key
	map fromIKey <$> SQL.getAssociatedKey fileMiss (SQL.ReadHandle h)

addAssociatedFileOldBench :: BenchDb -> Benchmark
addAssociatedFileOldBench (BenchDb h num) = bench ("addAssociatedFile to " ++ show num ++ " (old)") $ nfIO $ do
	n <- getStdRandom (randomR (1,num))
	SQL.addAssociatedFile (toIKey (keyN n)) (fileN n) (SQL.WriteHandle h)
	H.flushDbQueue h

addAssociatedFileNewBench :: BenchDb -> Benchmark
addAssociatedFileNewBench (BenchDb h num) = bench ("addAssociatedFile to " ++ show num ++ " (new)") $ nfIO $ do
	n <- getStdRandom (randomR (1,num))
	SQL.addAssociatedFile (toIKey (keyN n)) (fileN (n+1)) (SQL.WriteHandle h)
	H.flushDbQueue h

populateAssociatedFiles :: H.DbQueue -> Int -> IO ()
populateAssociatedFiles h num = do
	forM_ [1..num] $ \n ->
		SQL.addAssociatedFile (toIKey (keyN n)) (fileN n) (SQL.WriteHandle h)
	H.flushDbQueue h

keyN :: Int -> Key
keyN n = stubKey
	{ keyName = B8.pack $ "key" ++ show n
	, keyVariety = OtherKey "BENCH"
	}

fileN :: Int -> TopFilePath
fileN n = asTopFilePath ("file" ++ show n)

keyMiss :: Key
keyMiss = keyN 0 -- 0 is never stored

fileMiss :: TopFilePath
fileMiss = fileN 0 -- 0 is never stored

data BenchDb = BenchDb H.DbQueue Int

benchDb :: FilePath -> Int -> Annex BenchDb
benchDb tmpdir num = do
	liftIO $ putStrLn $ "setting up database with " ++ show num
	initDb db SQL.createTables
	h <- liftIO $ H.openDbQueue H.MultiWriter db SQL.containedTable
	liftIO $ populateAssociatedFiles h num
	return (BenchDb h num)
  where
	db = tmpdir </> show num </> "db"

#endif /* WITH_BENCHMARK */
