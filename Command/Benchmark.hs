{- git-annex benchmark
 -
 - Copyright 2016-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.Benchmark where

import Command
import Types.Benchmark
#ifdef WITH_BENCHMARK
import Database.Benchmark

import Criterion.Main
import Criterion.Main.Options (parseWith)
#endif

cmd :: BenchmarkGenerator -> Command
cmd generator = command "benchmark" SectionTesting
	"benchmark git-annex commands"
	paramNothing
	(seek generator <$$> optParser)

data BenchmarkOptions 
	= BenchmarkOptions CmdParams CriterionMode
	| BenchmarkDatabases CriterionMode

optParser :: CmdParamsDesc -> Parser BenchmarkOptions
optParser desc = benchmarkoptions <|> benchmarkdatabases
  where
	benchmarkoptions = BenchmarkOptions
		<$> cmdParams desc
		<*> criterionopts
	benchmarkdatabases = BenchmarkDatabases
		<$> criterionopts
		<* flag' () 
			( long "databases"
			<> help "benchmark sqlite databases"
			)
#ifdef WITH_BENCHMARK
	criterionopts = parseWith defaultConfig
#else
	criterionopts = pure ()
#endif

seek :: BenchmarkGenerator -> BenchmarkOptions -> CommandSeek
#ifdef WITH_BENCHMARK
seek generator (BenchmarkOptions ps mode) = do
	runner <- generator ps
	liftIO $ runMode mode [ bench (unwords ps) $ nfIO runner ]
seek _ (BenchmarkDatabases mode) = benchmarkDbs mode
#else
seek _ _ = giveup "git-annex is not built with benchmarking support"
#endif
