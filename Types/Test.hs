{- git-annex test data types.
 -
 - Copyright 2011-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Types.Test where

#ifdef WITH_TESTSUITE
import Test.Tasty.Options
import Data.Monoid
import Prelude
import Types.Command
#endif

#ifdef WITH_TESTSUITE
data TestOptions = TestOptions
	{ tastyOptionSet :: OptionSet
	, keepFailuresOption :: Bool
	, fakeSsh :: Bool
	, internalData :: CmdParams
	}

instance Monoid TestOptions where
	mempty = TestOptions mempty False False mempty
	mappend a b = TestOptions
		(tastyOptionSet a <> tastyOptionSet b)
		(keepFailuresOption a || keepFailuresOption b)
		(fakeSsh a || fakeSsh b)
		(internalData a <> internalData b)

#else
type TestOptions = ()
#endif

type TestRunner = TestOptions -> IO ()
