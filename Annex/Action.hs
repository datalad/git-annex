{- git-annex actions
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Action where

import qualified Data.Map as M
#ifndef mingw32_HOST_OS
import System.Posix.Signals
#endif

import Annex.Common
import qualified Annex
import Annex.Content

{- Actions to perform each time ran. -}
startup :: Annex ()
startup =
#ifndef mingw32_HOST_OS
	liftIO $ void $ installHandler sigINT Default Nothing
#else
	return ()
#endif

{- Cleanup actions. -}
shutdown :: Bool -> Annex ()
shutdown nocommit = do
	saveState nocommit
	sequence_ =<< M.elems <$> Annex.getState Annex.cleanup
	liftIO reapZombies -- zombies from long-running git processes
