{- git-annex test data types.
 -
 - Copyright 2011-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Types.Test where

#ifdef WITH_TESTSUITE
import Test.Tasty.Options
#endif

#ifdef WITH_TESTSUITE
type TestOptions = OptionSet
#else
type TestOptions = ()
#endif

type TestRunner = TestOptions -> IO ()
