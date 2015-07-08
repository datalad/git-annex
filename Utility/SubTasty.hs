{- Running tasty as a subcommand.
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.SubTasty where

import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Runners
import Options.Applicative

-- Uses tasty's option parser, modified to expect a subcommand.
parseOpts :: String -> [Ingredient] -> TestTree -> [String] -> IO OptionSet
parseOpts subcommand is ts = 
	handleParseResult . execParserPure (prefs idm) pinfo
  where
	pinfo = info (helper <*> subpinfo) (fullDesc <> header desc)
	subpinfo = subparser $ command subcommand $ 
		suiteOptionParser is ts
			`info`
		progDesc desc
	desc = "Builtin test suite"
