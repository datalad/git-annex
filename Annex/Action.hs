{- git-annex actions
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Action (
	action,
	verifiedAction,
	startup,
	shutdown,
	stopCoProcesses,
	stopNonConcurrentSafeCoProcesses,
) where

import qualified Data.Map as M

import Annex.Common
import qualified Annex
import Annex.Content
import Annex.CatFile
import Annex.CheckAttr
import Annex.HashObject
import Annex.CheckIgnore
import Annex.TransferrerPool

import Control.Concurrent.STM
#ifndef mingw32_HOST_OS
import System.Posix.Signals
#endif

{- Runs an action that may throw exceptions, catching and displaying them. -}
action :: Annex () -> Annex Bool
action a = tryNonAsync a >>= \case
	Right () -> return True
	Left e -> do
		warning (show e)
		return False

verifiedAction :: Annex Verification -> Annex (Bool, Verification)
verifiedAction a = tryNonAsync a >>= \case
	Right v -> return (True, v)
	Left e -> do
		warning (show e)
		return (False, UnVerified)


{- Actions to perform each time ran. -}
startup :: Annex ()
startup = do
#ifndef mingw32_HOST_OS
	av <- Annex.getState Annex.signalactions
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

{- Cleanup actions. -}
shutdown :: Bool -> Annex ()
shutdown nocommit = do
	saveState nocommit
	sequence_ =<< M.elems <$> Annex.getState Annex.cleanupactions
	stopCoProcesses

{- Stops all long-running child processes, including git query processes. -}
stopCoProcesses :: Annex ()
stopCoProcesses = do
	stopNonConcurrentSafeCoProcesses
	emptyTransferrerPool

{- Stops long-running child processes that use handles that are not safe
 - for multiple threads to access at the same time. -}
stopNonConcurrentSafeCoProcesses :: Annex ()
stopNonConcurrentSafeCoProcesses = do
	catFileStop
	checkAttrStop
	hashObjectStop
	checkIgnoreStop
