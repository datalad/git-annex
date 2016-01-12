{- git-annex benchmark
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Command.Benchmark where

import Command
import Database.Types
import qualified Database.Keys.SQL as SQL
import qualified Database.Queue as H
import Utility.Tmp
import Git.FilePath

import Criterion.Main
import Criterion.Internal (runAndAnalyse)
import Criterion.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.DeepSeq
import System.FilePath
import System.Random

cmd :: Command
cmd = noRepo (withParams benchmark) $
	dontCheck repoExists $
		command "benchmark" SectionTesting
			"run benchmarks"
			paramNothing
			(withParams (liftIO . benchmark))

benchmark :: CmdParams -> IO ()
benchmark _ = withTmpDirIn "." "benchmark" $ \tmpdir -> do
	-- benchmark different sizes of databases
	dbs <- mapM (benchDb tmpdir)
		[ 1000
		, 10000
		-- , 100000
		]
	runCriterion $
		bgroup "keys database" $ flip concatMap dbs $ \db ->
			[ getAssociatedFilesHitBench db
			, getAssociatedFilesMissBench db
			, getAssociatedKeyHitBench db
			, getAssociatedKeyMissBench db
			]

getAssociatedFilesHitBench :: BenchDb -> Benchmark
getAssociatedFilesHitBench ( BenchDb h num) = bench ("getAssociatedFiles from " ++ show num ++ " (hit)") $ nfIO $ do
	n <- getStdRandom (randomR (1,num))
	SQL.getAssociatedFiles (keyN n) (SQL.ReadHandle h)

getAssociatedFilesMissBench :: BenchDb -> Benchmark
getAssociatedFilesMissBench ( BenchDb h num) = bench ("getAssociatedFiles from " ++ show num ++ " (miss)") $ nfIO $
	SQL.getAssociatedFiles keyMiss (SQL.ReadHandle h)

getAssociatedKeyHitBench :: BenchDb -> Benchmark
getAssociatedKeyHitBench (BenchDb h num) = bench ("getAssociatedKey from " ++ show num ++ " (hit)") $ nfIO $ do
	n <- getStdRandom (randomR (1,num))
	SQL.getAssociatedKey (fileN n) (SQL.ReadHandle h)

getAssociatedKeyMissBench :: BenchDb -> Benchmark
getAssociatedKeyMissBench (BenchDb h num) = bench ("getAssociatedKey from " ++ show num ++ " (miss)") $ nfIO $
	SQL.getAssociatedKey fileMiss (SQL.ReadHandle h)

populateAssociatedFiles :: H.DbQueue -> Int -> IO ()
populateAssociatedFiles h num = do
	forM_ [1..num] $ \n ->
		SQL.addAssociatedFile (keyN n) (fileN n) (SQL.WriteHandle h)
	H.flushDbQueue h

keyN :: Int -> SKey
keyN n = SKey ("key" ++ show n)

fileN :: Int -> TopFilePath
fileN n = asTopFilePath ("file" ++ show n)

keyMiss :: SKey
keyMiss = keyN 0 -- 0 is never stored

fileMiss :: TopFilePath
fileMiss = fileN 0 -- 0 is never stored

data BenchDb = BenchDb H.DbQueue Int

benchDb :: FilePath -> Int -> IO BenchDb
benchDb tmpdir num = do
	putStrLn $ "setting up database with " ++ show num
	H.initDb f SQL.createTables
	h <- H.openDbQueue f SQL.containedTable
	populateAssociatedFiles h num
	return (BenchDb h num)
  where
	f = tmpdir </> "db" ++ show num

instance NFData TopFilePath where
	rnf = rnf . getTopFilePath

instance NFData SKey where
	rnf (SKey s) = rnf s
	
-- can't use Criterion's defaultMain here because it looks at
-- command-line parameters
runCriterion :: Benchmark -> IO ()
runCriterion = withConfig defaultConfig . runAndAnalyse (const True)
