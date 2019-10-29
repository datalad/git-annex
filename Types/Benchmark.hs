{- git-annex benchmark data types.
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Types.Benchmark where

import Annex
import Types.Command
#ifdef WITH_BENCHMARK
import Criterion.Main.Options (Mode)
#endif

type BenchmarkGenerator = [String] -> Annex (IO ())

type MkBenchmarkGenerator = [Command] -> BenchmarkGenerator

#ifdef WITH_BENCHMARK
type CriterionMode = Mode
#else
type CriterionMode = ()
#endif
