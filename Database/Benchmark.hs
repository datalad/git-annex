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
import Utility.Tmp.Dir
import Git.FilePath
import Types.Key
import Utility.DataUnits

import Criterion.Main
import qualified Data.ByteString.Short as S (toShort)
import qualified Data.ByteString.Char8 as B8
import System.Random
import Control.Concurrent
#endif

benchmarkDbs :: CriterionMode -> Integer -> Annex ()
#ifdef WITH_BENCHMARK
benchmarkDbs mode n = withTmpDirIn (literalOsPath ".") (literalOsPath "benchmark") $ \tmpdir -> do
	db <- benchDb tmpdir n
	liftIO $ runMode mode
		[ bgroup "keys database"
			[ getAssociatedFilesHitBench db
			, getAssociatedFilesMissBench db
			, getAssociatedKeyHitBench db
			, getAssociatedKeyMissBench db
			, addAssociatedFileOldBench db
			, addAssociatedFileNewBench db
			]
		]
#else
benchmarkDbs _ = giveup "not built with criterion, cannot benchmark"
#endif

#ifdef WITH_BENCHMARK

getAssociatedFilesHitBench :: BenchDb -> Benchmark
getAssociatedFilesHitBench (BenchDb h num _) = bench ("getAssociatedFiles (hit)") $ nfIO $ do
	n <- getStdRandom (randomR (1,num))
	SQL.getAssociatedFiles (keyN n) (SQL.ReadHandle h)

getAssociatedFilesMissBench :: BenchDb -> Benchmark
getAssociatedFilesMissBench (BenchDb h _ _) = bench ("getAssociatedFiles (miss)") $ nfIO $
	SQL.getAssociatedFiles keyMiss (SQL.ReadHandle h)

getAssociatedKeyHitBench :: BenchDb -> Benchmark
getAssociatedKeyHitBench (BenchDb h num _) = bench ("getAssociatedKey (hit)") $ nfIO $ do
	n <- getStdRandom (randomR (1,num))
	SQL.getAssociatedKey (fileN n) (SQL.ReadHandle h)

getAssociatedKeyMissBench :: BenchDb -> Benchmark
getAssociatedKeyMissBench (BenchDb h _ _) = bench ("getAssociatedKey (miss)") $ nfIO $
	SQL.getAssociatedKey fileMiss (SQL.ReadHandle h)

addAssociatedFileOldBench :: BenchDb -> Benchmark
addAssociatedFileOldBench (BenchDb h num _) = bench ("addAssociatedFile to (old)") $ nfIO $ do
	n <- getStdRandom (randomR (1,num))
	SQL.addAssociatedFile (keyN n) (fileN n) (SQL.WriteHandle h)
	H.flushDbQueue h

addAssociatedFileNewBench :: BenchDb -> Benchmark
addAssociatedFileNewBench (BenchDb h num mv) = bench ("addAssociatedFile to (new)") $ nfIO $ do
	n <- takeMVar mv
	putMVar mv (n+1)
	SQL.addAssociatedFile (keyN n) (fileN (num+n)) (SQL.WriteHandle h)
	H.flushDbQueue h

populateAssociatedFiles :: H.DbQueue -> Integer -> IO ()
populateAssociatedFiles h num = do
	forM_ [1..num] $ \n ->
		SQL.addAssociatedFile (keyN n) (fileN n) (SQL.WriteHandle h)
	H.flushDbQueue h

keyN :: Integer -> Key
keyN n = mkKey $ \k -> k
	{ keyName = S.toShort (B8.pack $ "key" ++ show n)
	, keyVariety = OtherKey "BENCH"
	}

fileN :: Integer -> TopFilePath
fileN n = asTopFilePath (toOsPath ("file" ++ show n))

keyMiss :: Key
keyMiss = keyN 0 -- 0 is never stored

fileMiss :: TopFilePath
fileMiss = fileN 0 -- 0 is never stored

data BenchDb = BenchDb H.DbQueue Integer (MVar Integer)

benchDb :: OsPath -> Integer -> Annex BenchDb
benchDb tmpdir num = do
	liftIO $ putStrLn $ "setting up database with " ++ show num ++ " items"
	initDb db SQL.createTables
	h <- liftIO $ H.openDbQueue db SQL.containedTable
	liftIO $ populateAssociatedFiles h num
	sz <- liftIO $ getFileSize db
	liftIO $ putStrLn $ "size of database on disk: " ++ 
		roughSize storageUnits False sz
	mv <- liftIO $ newMVar 1
	return (BenchDb h num mv)
  where
	db = tmpdir </> toOsPath (show num) </> literalOsPath "db"

#endif /* WITH_BENCHMARK */
