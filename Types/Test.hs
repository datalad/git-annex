{- git-annex test data types.
 -
 - Copyright 2011-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Test where

import Test.Tasty.Options

import Types.Concurrency
import Types.Command

data TestOptions = TestOptions
	{ tastyOptionSet :: OptionSet
	, keepFailuresOption :: Bool
	, fakeSsh :: Bool
	, concurrentJobs :: Maybe Concurrency
	, internalData :: CmdParams
	}

type TestRunner = TestOptions -> IO ()
