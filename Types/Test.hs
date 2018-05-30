{- git-annex test data types.
 -
 - Copyright 2011-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Types.Test where

import Test.Tasty.Options
import Data.Monoid
#if MIN_VERSION_base(4,9,0)
import qualified Data.Semigroup as Sem
#endif
import Prelude

import Types.Command

data TestOptions = TestOptions
	{ tastyOptionSet :: OptionSet
	, keepFailuresOption :: Bool
	, fakeSsh :: Bool
	, internalData :: CmdParams
	}

appendTestOptions :: TestOptions -> TestOptions -> TestOptions
appendTestOptions a b = TestOptions
	(tastyOptionSet a <> tastyOptionSet b)
	(keepFailuresOption a || keepFailuresOption b)
	(fakeSsh a || fakeSsh b)
	(internalData a <> internalData b)

#if MIN_VERSION_base(4,9,0)
instance Sem.Semigroup TestOptions where
	(<>) = appendTestOptions
#endif

instance Monoid TestOptions where
	mempty = TestOptions mempty False False mempty
#if MIN_VERSION_base(4,11,0)
#elif MIN_VERSION_base(4,9,0)
	mappend = (Sem.<>)
#else
	mappend = appendTestOptions
#endif

type TestRunner = TestOptions -> IO ()
