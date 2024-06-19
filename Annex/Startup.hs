{- git-annex startup
 -
 - Copyright 2010-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Startup where

import Annex.Common
import qualified Annex
import Logs.Cluster

#ifndef mingw32_HOST_OS
import Control.Concurrent.STM
import System.Posix.Signals
#endif

{- Run when starting up the main git-annex program. -}
startup :: Annex ()
startup = do
	startupSignals
	gc <- Annex.getGitConfig
	when (isinitialized gc)
		startupAnnex
  where
	isinitialized gc = annexUUID gc /= NoUUID
		&& isJust (annexVersion gc)

{- Run when starting up the main git-annex program when
 - git-annex has already been initialized.
 - Alternatively, run after initialization.
 -}
startupAnnex :: Annex ()
startupAnnex = doQuietAction $
	-- Logs.Location needs clusters to be loaded before it is used,
	-- in order for a cluster to be treated as the location of keys
	-- that are located in any of its nodes.
	void loadClusters

startupSignals :: Annex ()
startupSignals = do
#ifndef mingw32_HOST_OS
	av <- Annex.getRead Annex.signalactions
	let propagate sig = liftIO $ installhandleronce sig av
	propagate sigINT
	propagate sigQUIT
	propagate sigTERM
	propagate sigTSTP
	propagate sigCONT
	propagate sigHUP
	-- sigWINCH is not propagated; it should not be needed,
	-- and the concurrent-output library installs its own signal
	-- handler for it.
	-- sigSTOP and sigKILL cannot be caught, so will not be propagated.
  where
	installhandleronce sig av = void $
		installHandler sig (CatchOnce (gotsignal sig av)) Nothing
	gotsignal sig av = do
		mapM_ (\a -> a (fromIntegral sig)) =<< atomically (readTVar av)
		raiseSignal sig
		installhandleronce sig av
#else
       return ()
#endif
