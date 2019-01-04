{- git-annex benchmark
 -
 - Copyright 2016-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Command.Benchmark where

import Command
import Types.Benchmark

import Criterion.Main
import Criterion.Main.Options (parseWith, Mode)

cmd :: BenchmarkGenerator -> Command
cmd generator = command "benchmark" SectionTesting
	"benchmark git-annex commands"
	paramNothing
	(seek generator <$$> optParser)

data BenchmarkOptions = BenchmarkOptions CmdParams Mode

optParser :: CmdParamsDesc -> Parser BenchmarkOptions
optParser desc = BenchmarkOptions
	<$> cmdParams desc
	-- parse criterion's options
	<*> parseWith defaultConfig

seek :: BenchmarkGenerator -> BenchmarkOptions -> CommandSeek
seek generator (BenchmarkOptions ps mode) = do
	runner <- generator ps
	liftIO $ runMode mode [ bench (unwords ps) $ nfIO runner ]
