{- git-annex benchmark data types.
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Benchmark where

import Annex
import Types.Command

type BenchmarkGenerator = [String] -> Annex (IO ())

type MkBenchmarkGenerator = [Command] -> BenchmarkGenerator
