{- git-annex test data types.
 -
 - Copyright 2011-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Test where

import Test.Tasty.Options
import Data.Monoid
import qualified Data.Semigroup as Sem
import Prelude

import Types.Command

data TestOptions = TestOptions
	{ tastyOptionSet :: OptionSet
	, keepFailuresOption :: Bool
	, fakeSsh :: Bool
	, internalData :: CmdParams
	}

instance Sem.Semigroup TestOptions where
	a <> b = TestOptions
		(tastyOptionSet a <> tastyOptionSet b)
		(keepFailuresOption a || keepFailuresOption b)
		(fakeSsh a || fakeSsh b)
		(internalData a <> internalData b)

instance Monoid TestOptions where
	mempty = TestOptions mempty False False mempty

type TestRunner = TestOptions -> IO ()
