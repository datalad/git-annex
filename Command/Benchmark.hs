{- git-annex benchmark
 -
 - Copyright 2016-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.Benchmark where

import Command
import Types.Benchmark

#ifdef WITH_BENCHMARK
import Criterion.Main
import Criterion.Main.Options (parseWith, Mode)
#endif

cmd :: BenchmarkGenerator -> Command
cmd generator = command "benchmark" SectionTesting
	"benchmark git-annex commands"
	paramNothing
	(seek generator <$$> optParser)

#ifndef WITH_BENCHMARK
type Mode = ()
#endif

data BenchmarkOptions = BenchmarkOptions CmdParams Mode

optParser :: CmdParamsDesc -> Parser BenchmarkOptions
optParser desc = BenchmarkOptions
	<$> cmdParams desc
#ifdef WITH_BENCHMARK
	-- parse criterion's options
	<*> parseWith defaultConfig
#else
	<*> pure ()
#endif

seek :: BenchmarkGenerator -> BenchmarkOptions -> CommandSeek
#ifdef WITH_BENCHMARK
seek generator (BenchmarkOptions ps mode) = do
	runner <- generator ps
	liftIO $ runMode mode [ bench (unwords ps) $ nfIO runner ]
#else
seek _ _ = giveup "git-annex is not built with benchmarking support"
#endif
