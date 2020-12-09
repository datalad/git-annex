{- git-annex actions
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

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
startup = return ()

{- Cleanup actions. -}
shutdown :: Bool -> Annex ()
shutdown nocommit = do
	saveState nocommit
	sequence_ =<< M.elems <$> Annex.getState Annex.cleanup
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
