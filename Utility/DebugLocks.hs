{- Pinpointing location of MVar/STM deadlocks
 -
 - Copyright 2018 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utility.DebugLocks (debugLocks) where

import Control.Monad.Catch
import Control.Monad.IO.Class
#ifdef DEBUGLOCKS
import Control.Exception (BlockedIndefinitelyOnSTM, BlockedIndefinitelyOnMVar)
import GHC.Stack
import System.IO
#endif

{- Wrap around any action, and if it dies due to deadlock, will display
 - a call stack on stderr when DEBUGLOCKS is defined.
 -
 - Should be zero cost to call when DEBUGLOCKS is not defined.
 -}
#ifdef DEBUGLOCKS
debugLocks :: HasCallStack => (MonadCatch m, MonadIO m) => m a -> m a
debugLocks a = a `catches`
	[ Handler (\ (e :: BlockedIndefinitelyOnMVar) -> go "MVar" e callStack)
	, Handler (\ (e :: BlockedIndefinitelyOnSTM) -> go "STM" e callStack)
	]
  where
	go ty e cs = do
		liftIO $ do
			hPutStrLn stderr $ 
				ty ++ " deadlock detected " ++ prettyCallStack cs
			hFlush stderr
		throwM e
#else
-- No HasCallStack constraint.
debugLocks :: (MonadCatch m, MonadIO m) => m a -> m a
debugLocks a = a
#endif
