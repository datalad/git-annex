{- Persistent sqlite database utilities.
 -
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Database.Utility (
	insertUniqueFast,
) where

import Control.Monad
import Database.Persist.Class

{- insertUnique_ is 2x as fast as insertUnique, so use when available.
 -
 - It would be difficult to write the type signature here, since older
 - versions of persistent have different constraints on insertUnique.
 -}
#if MIN_VERSION_persistent(2,14,5)
insertUniqueFast x = void (insertUnique_ x)
#else
insertUniqueFast x = void (insertUnique x)
#endif
